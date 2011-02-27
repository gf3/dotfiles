# shelve.py
#
# Copyright 2007 Bryan O'Sullivan <bos@serpentine.com>
# Copyright 2007 TK Soh <teekaysoh@gmailcom>
#
# This software may be used and distributed according to the terms of
# the GNU General Public License, incorporated herein by reference.

'''interactive change selection to set aside that may be restored later'''

from mercurial.i18n import _
from mercurial import cmdutil, commands, cmdutil, hg, mdiff, patch, revlog
from mercurial import util, fancyopts, extensions
import copy, cStringIO, errno, operator, os, re, shutil, tempfile

lines_re = re.compile(r'@@ -(\d+),(\d+) \+(\d+),(\d+) @@\s*(.*)')

def internalpatch(patchobj, ui, strip, cwd, reverse=False, files={}):
    """use builtin patch to apply <patchobj> to the working directory.
    returns whether patch was applied with fuzz factor.
    
    Adapted from patch.internalpatch() to support reverse patching.
    """

    eolmode = ui.config('patch', 'eol', 'strict')

    if eolmode.lower() not in patch.eolmodes:
        raise util.Abort(_('Unsupported line endings type: %s') % eolmode)
    
    try:
        fp = file(patchobj, 'rb')
    except TypeError:
        fp = patchobj
    if cwd:
        curdir = os.getcwd()
        os.chdir(cwd)
    try:
        ret = patch.applydiff(ui, fp, files, strip=strip, eolmode=eolmode)
    finally:
        if cwd:
            os.chdir(curdir)
    if ret < 0:
        raise PatchError
    return ret > 0

def scanpatch(fp):
    lr = patch.linereader(fp)

    def scanwhile(first, p):
        lines = [first]
        while True:
            line = lr.readline()
            if not line:
                break
            if p(line):
                lines.append(line)
            else:
                lr.push(line)
                break
        return lines

    while True:
        line = lr.readline()
        if not line:
            break
        if line.startswith('diff --git a/'):
            def notheader(line):
                s = line.split(None, 1)
                return not s or s[0] not in ('---', 'diff')
            header = scanwhile(line, notheader)
            fromfile = lr.readline()
            if fromfile.startswith('---'):
                tofile = lr.readline()
                header += [fromfile, tofile]
            else:
                lr.push(fromfile)
            yield 'file', header
        elif line[0] == ' ':
            yield 'context', scanwhile(line, lambda l: l[0] in ' \\')
        elif line[0] in '-+':
            yield 'hunk', scanwhile(line, lambda l: l[0] in '-+\\')
        else:
            m = lines_re.match(line)
            if m:
                yield 'range', m.groups()
            else:
                raise patch.PatchError('unknown patch content: %r' % line)

class header(object):
    diff_re = re.compile('diff --git a/(.*) b/(.*)$')
    allhunks_re = re.compile('(?:index|new file|deleted file) ')
    pretty_re = re.compile('(?:new file|deleted file) ')
    special_re = re.compile('(?:index|new|deleted|copy|rename) ')

    def __init__(self, header):
        self.header = header
        self.hunks = []

    def binary(self):
        for h in self.header:
            if h.startswith('index '):
                return True

    def pretty(self, fp):
        for h in self.header:
            if h.startswith('index '):
                fp.write(_('this modifies a binary file (all or nothing)\n'))
                break
            if self.pretty_re.match(h):
                fp.write(h)
                if self.binary():
                    fp.write(_('this is a binary file\n'))
                break
            if h.startswith('---'):
                fp.write(_('%d hunks, %d lines changed\n') %
                         (len(self.hunks),
                          sum([h.added + h.removed for h in self.hunks])))
                break
            fp.write(h)

    def write(self, fp):
        fp.write(''.join(self.header))

    def allhunks(self):
        for h in self.header:
            if self.allhunks_re.match(h):
                return True

    def files(self):
        fromfile, tofile = self.diff_re.match(self.header[0]).groups()
        if fromfile == tofile:
            return [fromfile]
        return [fromfile, tofile]

    def filename(self):
        return self.files()[-1]

    def __repr__(self):
        return '<header %s>' % (' '.join(map(repr, self.files())))

    def special(self):
        for h in self.header:
            if self.special_re.match(h):
                return True

def countchanges(hunk):
    add = len([h for h in hunk if h[0] == '+'])
    rem = len([h for h in hunk if h[0] == '-'])
    return add, rem

class hunk(object):
    maxcontext = 3

    def __init__(self, header, fromline, toline, proc, before, hunk, after):
        def trimcontext(number, lines):
            delta = len(lines) - self.maxcontext
            if False and delta > 0:
                return number + delta, lines[:self.maxcontext]
            return number, lines

        self.header = header
        self.fromline, self.before = trimcontext(fromline, before)
        self.toline, self.after = trimcontext(toline, after)
        self.proc = proc
        self.hunk = hunk
        self.added, self.removed = countchanges(self.hunk)

    def __cmp__(self, rhs):
        # since the hunk().toline needs to be adjusted when hunks are
        # removed/added, we can't take it into account when we cmp
        attrs = ['header', 'fromline', 'proc', 'hunk', 'added', 'removed']
        for attr in attrs:
            selfattr = getattr(self, attr, None)
            rhsattr = getattr(rhs, attr, None)

            if selfattr is None or rhsattr is None:
                raise util.Abort(_('non-existant attribute %s') % attr)

            rv = cmp(selfattr, rhsattr)
            if rv != 0:
                return rv
        return rv


    def write(self, fp):
        delta = len(self.before) + len(self.after)
        if self.after and self.after[-1] == '\\ No newline at end of file\n':
            delta -= 1
        fromlen = delta + self.removed
        tolen = delta + self.added
        fp.write('@@ -%d,%d +%d,%d @@%s\n' %
                 (self.fromline, fromlen, self.toline, tolen,
                  self.proc and (' ' + self.proc)))
        fp.write(''.join(self.before + self.hunk + self.after))

    pretty = write

    def filename(self):
        return self.header.filename()

    def __repr__(self):
        return '<hunk %r@%d>' % (self.filename(), self.fromline)

def parsepatch(fp):
    class parser(object):
        def __init__(self):
            self.fromline = 0
            self.toline = 0
            self.proc = ''
            self.header = None
            self.context = []
            self.before = []
            self.hunk = []
            self.stream = []

        def addrange(self, (fromstart, fromend, tostart, toend, proc)):
            self.fromline = int(fromstart)
            self.toline = int(tostart)
            self.proc = proc

        def addcontext(self, context):
            if self.hunk:
                h = hunk(self.header, self.fromline, self.toline, self.proc,
                         self.before, self.hunk, context)
                self.header.hunks.append(h)
                self.stream.append(h)
                self.fromline += len(self.before) + h.removed
                self.toline += len(self.before) + h.added
                self.before = []
                self.hunk = []
                self.proc = ''
            self.context = context

        def addhunk(self, hunk):
            if self.context:
                self.before = self.context
                self.context = []
            self.hunk = hunk

        def newfile(self, hdr):
            self.addcontext([])
            h = header(hdr)
            self.stream.append(h)
            self.header = h

        def finished(self):
            self.addcontext([])
            return self.stream

        transitions = {
            'file': {'context': addcontext,
                     'file': newfile,
                     'hunk': addhunk,
                     'range': addrange},
            'context': {'file': newfile,
                        'hunk': addhunk,
                        'range': addrange},
            'hunk': {'context': addcontext,
                     'file': newfile,
                     'range': addrange},
            'range': {'context': addcontext,
                      'hunk': addhunk},
            }

    p = parser()

    state = 'context'
    for newstate, data in scanpatch(fp):
        try:
            p.transitions[state][newstate](p, data)
        except KeyError:
            raise patch.PatchError('unhandled transition: %s -> %s' %
                                   (state, newstate))
        state = newstate
    return p.finished()

def filterpatch(ui, chunks, shouldprompt=True):
    chunks = list(chunks)
    chunks.reverse()
    seen = {}
    def consumefile():
        consumed = []
        while chunks:
            if isinstance(chunks[-1], header):
                break
            else:
                consumed.append(chunks.pop())
        return consumed
    
    resp_all = [None]

    """ If we're not to prompt (i.e. they specified the --all flag) 
        we pre-emptively set the 'all' flag """
    if shouldprompt == False:
        resp_all = ['y']
    
    resp_file = [None]
    applied = {}
    def prompt(query):
        if resp_all[0] is not None:
            return resp_all[0]
        if resp_file[0] is not None:
            return resp_file[0]
        while True:
            resps = _('[Ynsfdaq?]')
            choices = (_('&Yes, shelve this change'),
                    _('&No, skip this change'),
                    _('&Skip remaining changes to this file'),
                    _('Shelve remaining changes to this &file'),
                    _('&Done, skip remaining changes and files'),
                    _('Shelve &all changes to all remaining files'),
                    _('&Quit, shelving no changes'),
                    _('&?'))
            r = ui.promptchoice("%s %s " % (query, resps), choices)
            if r == 7:
                c = shelve.__doc__.find('y - shelve this change')
                for l in shelve.__doc__[c:].splitlines():
                    if l: ui.write(_(l.strip()) + '\n')
                continue
            elif r == 0: # yes
                ret = 'y'
            elif r == 1: # no
                ret = 'n'
            elif r == 2: # Skip
                ret = resp_file[0] = 'n'
            elif r == 3: # file (shelve remaining)
                ret = resp_file[0] = 'y'
            elif r == 4: # done, skip remaining
                ret = resp_all[0] = 'n'
            elif r == 5: # all
                ret = resp_all[0] = 'y'
            elif r == 6: # quit
                raise util.Abort(_('user quit'))
            return ret
    while chunks:
        chunk = chunks.pop()
        if isinstance(chunk, header):
            resp_file = [None]
            fixoffset = 0
            hdr = ''.join(chunk.header)
            if hdr in seen:
                consumefile()
                continue
            seen[hdr] = True
            if resp_all[0] is None:
                chunk.pretty(ui)
            if shouldprompt == True:
                r = prompt(_('shelve changes to %s?') %
                       _(' and ').join(map(repr, chunk.files())))
            else:
                r = 'y'
            
            if r == 'y':
                applied[chunk.filename()] = [chunk]
                if chunk.allhunks():
                    applied[chunk.filename()] += consumefile()
            else:
                consumefile()
        else:
            if resp_file[0] is None and resp_all[0] is None:
                chunk.pretty(ui)
            r = prompt(_('shelve this change to %r?') %
                       chunk.filename())
            if r == 'y':
                if fixoffset:
                    chunk = copy.copy(chunk)
                    chunk.toline += fixoffset
                applied[chunk.filename()].append(chunk)
            else:
                fixoffset += chunk.removed - chunk.added
    return reduce(operator.add, [h for h in applied.itervalues()
                                 if h[0].special() or len(h) > 1], [])

def refilterpatch(allchunk, selected):
    ''' return unshelved chunks of files to be shelved '''
    l = []
    fil = []
    for c in allchunk:
        if isinstance(c, header):
            if len(l) > 1 and l[0] in selected:
                fil += l
            l = [c]
        elif c not in selected:
            l.append(c)
    if len(l) > 1 and l[0] in selected:
        fil += l
    return fil

def makebackup(ui, repo, dir, files):
    try:
        os.mkdir(dir)
    except OSError, err:
        if err.errno != errno.EEXIST:
            raise

    backups = {}
    for f in files:
        fd, tmpname = tempfile.mkstemp(prefix=f.replace('/', '_')+'.',
                                       dir=dir)
        os.close(fd)
        ui.debug('backup %r as %r\n' % (f, tmpname))
        util.copyfile(repo.wjoin(f), tmpname)
        backups[f] = tmpname

    return backups

def getshelfpath(repo, name):
    if name:
        shelfpath = "shelves/" + name
    else:
        # Check if a shelf from an older version exists
        if os.path.isfile(repo.join('shelve')):
            shelfpath = 'shelve'
        else:
            shelfpath = "shelves/default"
    
    return shelfpath
    
def shelve(ui, repo, *pats, **opts):
    '''interactively select changes to set aside

    If a list of files is omitted, all changes reported by "hg status"
    will be candidates for shelving.

    You will be prompted for whether to shelve changes to each
    modified file, and for files with multiple changes, for each
    change to use.

    The shelve command works with the Color extension to display
    diffs in color.

    On each prompt, the following responses are possible::

      y - shelve this change
      n - skip this change

      s - skip remaining changes to this file
      f - shelve remaining changes to this file

      d - done, skip remaining changes and files
      a - shelve all changes to all remaining files
      q - quit, shelving no changes

      ? - display help'''

    if not ui.interactive:
        raise util.Abort(_('shelve can only be run interactively'))

    # List all the active shelves by name and return '
    if opts['list']:
        listshelves(ui,repo)
        return

    forced = opts['force'] or opts['append']
    
    # Shelf name and path
    shelfname = opts.get('name')
    shelfpath = getshelfpath(repo, shelfname)
    
    if os.path.exists(repo.join(shelfpath)) and not forced:
        raise util.Abort(_('shelve data already exists'))
            
    def shelvefunc(ui, repo, message, match, opts):
        changes = repo.status(match=match)[:5]
        modified, added, removed = changes[:3]
        files = modified + added + removed
        diffopts = mdiff.diffopts(git=True, nodates=True)
        patch_diff = ''.join(patch.diff(repo, repo.dirstate.parents()[0],
                           match=match, changes=changes, opts=diffopts))
        
        fp = cStringIO.StringIO(patch_diff)
        ac = parsepatch(fp)
        fp.close()
        
        chunks = filterpatch(ui, ac, not opts['all'])
        rc = refilterpatch(ac, chunks)

        contenders = {}
        for h in chunks:
            try: contenders.update(dict.fromkeys(h.files()))
            except AttributeError: pass

        newfiles = [f for f in files if f in contenders]

        if not newfiles:
            ui.status(_('no changes to shelve\n'))
            return 0

        modified = dict.fromkeys(changes[0])

        backupdir = repo.join('shelve-backups')

        try:
            bkfiles = [f for f in newfiles if f in modified]
            backups = makebackup(ui, repo, backupdir, bkfiles)
            
            # patch to shelve
            sp = cStringIO.StringIO()
            for c in chunks:
                if c.filename() in backups:
                    c.write(sp)
            doshelve = sp.tell()
            sp.seek(0)

            # patch to apply to shelved files
            fp = cStringIO.StringIO()
            for c in rc:
                if c.filename() in backups:
                    c.write(fp)
            dopatch = fp.tell()
            fp.seek(0)

            try:
                # 3a. apply filtered patch to clean repo (clean)
                if backups:
                    hg.revert(repo, repo.dirstate.parents()[0], backups.has_key)

                # 3b. apply filtered patch to clean repo (apply)
                if dopatch:
                    ui.debug('applying patch\n')
                    ui.debug(fp.getvalue())
                    patch.internalpatch(fp, ui, 1, repo.root)
                del fp

                # 3c. apply filtered patch to clean repo (shelve)
                if doshelve:
                    ui.debug("saving patch to shelve\n")
                    if opts['append']:
                        f = repo.opener(shelfpath, "a")
                    else:
                        f = repo.opener(shelfpath, "w")
                    f.write(sp.getvalue())
                    del f
                del sp
            except:
                try:
                    for realname, tmpname in backups.iteritems():
                        ui.debug('restoring %r to %r\n' % (tmpname, realname))
                        util.copyfile(tmpname, repo.wjoin(realname))
                    ui.debug('removing shelve file\n')
                    os.unlink(repo.join(shelfpath))
                except OSError:
                    pass

            return 0
        finally:
            try:
                for realname, tmpname in backups.iteritems():
                    ui.debug('removing backup for %r : %r\n' % (realname, tmpname))
                    os.unlink(tmpname)
                os.rmdir(backupdir)
            except OSError:
                pass
    fancyopts.fancyopts([], commands.commitopts, opts)
    
    # wrap ui.write so diff output can be labeled/colorized
    def wrapwrite(orig, *args, **kw):
        label = kw.pop('label', '')
        for chunk, l in patch.difflabel(lambda: args):
            orig(chunk, label=label + l)
    oldwrite = ui.write
    extensions.wrapfunction(ui, 'write', wrapwrite)
    try:
        return cmdutil.commit(ui, repo, shelvefunc, pats, opts)
    finally:
        ui.write = oldwrite

def listshelves(ui, repo):
    # Check for shelve file at old location first
    if os.path.isfile(repo.join('shelve')):
        ui.status('default\n')
    
    # Now go through all the files in the shelves folder and list them out
    dirname = repo.join('shelves')
    if os.path.isdir(dirname):
        for filename in os.listdir(repo.join('shelves')):
            ui.status(filename + '\n')
    
def unshelve(ui, repo, **opts):
    '''restore shelved changes'''

    # Shelf name and path
    shelfname = opts.get('name')
    shelfpath = getshelfpath(repo, shelfname)

    # List all the active shelves by name and return '
    if opts['list']:
        listshelves(ui,repo)
        return
        
    try:
        patch_diff = repo.opener(shelfpath).read()
        fp = cStringIO.StringIO(patch_diff)
        if opts['inspect']:
            ui.status(fp.getvalue())
        else:
            files = []
            ac = parsepatch(fp)
            for chunk in ac:
                if isinstance(chunk, header):
                    files += chunk.files()
            backupdir = repo.join('shelve-backups')
            backups = makebackup(ui, repo, backupdir, set(files))

            ui.debug('applying shelved patch\n')
            patchdone = 0
            try:
                try:
                    fp.seek(0)
                    internalpatch(fp, ui, 1, repo.root)
                    patchdone = 1
                except:
                    if opts['force']:
                        patchdone = 1
                    else:
                        ui.status('restoring backup files\n')
                        for realname, tmpname in backups.iteritems():
                            ui.debug('restoring %r to %r\n' % 
                                     (tmpname, realname))
                            util.copyfile(tmpname, repo.wjoin(realname))
            finally:
                try:
                    ui.debug('removing backup files\n')
                    shutil.rmtree(backupdir, True)
                except OSError:
                    pass

            if patchdone:
                ui.debug("removing shelved patches\n")
                os.unlink(repo.join(shelfpath))
                ui.status("unshelve completed\n")
    except IOError:
        ui.warn('nothing to unshelve\n')

cmdtable = {
    "shelve":
        (shelve,
         [('A', 'addremove', None,
           _('mark new/missing files as added/removed before shelving')),
          ('f', 'force', None,
           _('overwrite existing shelve data')),
          ('a', 'append', None,
           _('append to existing shelve data')),
          ('', 'all', None,
           _('shelve all changes')),
          ('n', 'name', '',
           _('shelve changes to specified shelf name')),
           ('l', 'list', None, _('list active shelves')),
         ] + commands.walkopts,
         _('hg shelve [OPTION]... [FILE]...')),
    "unshelve":
        (unshelve,
         [('i', 'inspect', None, _('inspect shelved changes only')),
          ('f', 'force', None, 
           _('proceed even if patches do not unshelve cleanly')),
           ('n', 'name', '',
            _('unshelve changes from specified shelf name')),
           ('l', 'list', None, _('list active shelves')),
         ],
         _('hg unshelve [OPTION]...')),
}
