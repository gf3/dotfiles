#!/usr/bin/env python

from __future__ import with_statement

'''get repository information for use in a shell prompt

Take a string, parse any special variables inside, and output the result.

Useful mostly for putting information about the current repository into
a shell prompt.
'''

import re
import os
import subprocess
from datetime import datetime, timedelta
from os import path
from mercurial import extensions, commands, cmdutil, help
from mercurial.node import hex, short

CACHE_PATH = ".hg/prompt/cache"
CACHE_TIMEOUT = timedelta(minutes=15)

FILTER_ARG = re.compile(r'\|.+\((.*)\)')

def _cache_remote(repo, kind):
    cache = path.join(repo.root, CACHE_PATH, kind)
    c_tmp = cache + '.temp'

    # This is kind of a hack and I feel a little bit dirty for doing it.
    IGNORE = open('NUL:','w') if subprocess.mswindows else open('/dev/null','w')

    subprocess.call(['hg', kind, '--quiet'], stdout=file(c_tmp, 'w'), stderr=IGNORE)
    os.rename(c_tmp, cache)
    return

def _with_groups(groups, out):
    out_groups = [groups[0]] + [groups[-1]]

    if any(out_groups) and not all(out_groups):
        print 'Error parsing prompt string.  Mismatched braces?'

    out = out.replace('%', '%%')
    return ("%s" + out + "%s") % (out_groups[0][:-1] if out_groups[0] else '',
                                  out_groups[1][1:] if out_groups[1] else '')

def _get_filter(name, g):
    '''Return the filter with the given name, or None if it was not used.'''
    matching_filters = filter(lambda s: s and s.startswith('|%s' % name), g)
    if not matching_filters:
        return None

    # Later filters will override earlier ones, for now.
    f = matching_filters[-1]

    return f

def _get_filter_arg(f):
    if not f:
        return None

    args = FILTER_ARG.match(f).groups()
    if args:
        return args[0]
    else:
        return None

def prompt(ui, repo, fs='', **opts):
    '''get repository information for use in a shell prompt

    Take a string and output it for use in a shell prompt. You can use
    keywords in curly braces::

        $ hg prompt "currently on {branch}"
        currently on default

    You can also use an extended form of any keyword::

        {optional text here{keyword}more optional text}

    This will expand the inner {keyword} and output it along with the extra
    text only if the {keyword} expands successfully.  This is useful if you
    have a keyword that may not always apply to the current state and you
    have some text that you would like to see only if it is appropriate::

        $ hg prompt "currently at {bookmark}"
        currently at
        $ hg prompt "{currently at {bookmark}}"
        $ hg bookmark my-bookmark
        $ hg prompt "{currently at {bookmark}}"
        currently at my-bookmark

    See 'hg help prompt-keywords' for a list of available keywords.
    '''

    def _basename(m):
        return _with_groups(m.groups(), path.basename(repo.root)) if repo.root else ''

    def _bookmark(m):
        try:
            book = extensions.find('bookmarks').current(repo)
        except AttributeError:
            book = getattr(repo, '_bookmarkcurrent', None)
        return _with_groups(m.groups(), book) if book else ''

    def _branch(m):
        g = m.groups()

        branch = repo.dirstate.branch()
        quiet = _get_filter('quiet', g)

        out = branch if (not quiet) or (branch != 'default') else ''

        return _with_groups(g, out) if out else ''

    def _closed(m):
        g = m.groups()

        quiet = _get_filter('quiet', g)

        p = repo[None].parents()[0]
        pn = p.node()
        branch = repo.dirstate.branch()
        closed = (p.extra().get('close')
                  and pn in repo.branchheads(branch, closed=True))
        out = 'X' if (not quiet) and closed else ''

        return _with_groups(g, out) if out else ''

    def _count(m):
        g = m.groups()
        query = [g[1][1:]] if g[1] else ['all()']
        return _with_groups(g, str(len(cmdutil.revrange(repo, query))))

    def _node(m):
        g = m.groups()

        parents = repo[None].parents()
        p = 0 if '|merge' not in g else 1
        p = p if len(parents) > p else None

        format = short if '|short' in g else hex

        node = format(parents[p].node()) if p is not None else None
        return _with_groups(g, str(node)) if node else ''

    def _patch(m):
        g = m.groups()

        try:
            extensions.find('mq')
        except KeyError:
            return ''

        q = repo.mq

        if _get_filter('quiet', g) and not len(q.series):
            return ''

        if _get_filter('applied', g):
            out = str(len(q.applied))
        elif _get_filter('unapplied', g):
            out = str(len(q.unapplied(repo)))
        elif _get_filter('count', g):
            out = str(len(q.series))
        else:
            out = q.applied[-1].name if q.applied else ''

        return _with_groups(g, out) if out else ''

    def _patches(m):
        g = m.groups()

        try:
            extensions.find('mq')
        except KeyError:
            return ''

        join_filter = _get_filter('join', g)
        join_filter_arg = _get_filter_arg(join_filter)
        sep = join_filter_arg if join_filter else ' -> '

        patches = repo.mq.series
        applied = [p.name for p in repo.mq.applied]
        unapplied = filter(lambda p: p not in applied, patches)

        if _get_filter('hide_applied', g):
            patches = filter(lambda p: p not in applied, patches)
        if _get_filter('hide_unapplied', g):
            patches = filter(lambda p: p not in unapplied, patches)

        if _get_filter('reverse', g):
            patches = reversed(patches)

        pre_applied_filter = _get_filter('pre_applied', g)
        pre_applied_filter_arg = _get_filter_arg(pre_applied_filter)
        post_applied_filter = _get_filter('post_applied', g)
        post_applied_filter_arg = _get_filter_arg(post_applied_filter)

        pre_unapplied_filter = _get_filter('pre_unapplied', g)
        pre_unapplied_filter_arg = _get_filter_arg(pre_unapplied_filter)
        post_unapplied_filter = _get_filter('post_unapplied', g)
        post_unapplied_filter_arg = _get_filter_arg(post_unapplied_filter)

        for n, patch in enumerate(patches):
            if patch in applied:
                if pre_applied_filter:
                    patches[n] = pre_applied_filter_arg + patches[n]
                if post_applied_filter:
                    patches[n] = patches[n] + post_applied_filter_arg
            elif patch in unapplied:
                if pre_unapplied_filter:
                    patches[n] = pre_unapplied_filter_arg + patches[n]
                if post_unapplied_filter:
                    patches[n] = patches[n] + post_unapplied_filter_arg

        return _with_groups(g, sep.join(patches)) if patches else ''

    def _queue(m):
        g = m.groups()

        try:
            extensions.find('mq')
        except KeyError:
            return ''

        q = repo.mq

        out = os.path.basename(q.path)
        if out == 'patches' and not os.path.isdir(q.path):
            out = ''
        elif out.startswith('patches-'):
            out = out[8:]

        return _with_groups(g, out) if out else ''

    def _remote(kind):
        def _r(m):
            g = m.groups()

            cache_dir = path.join(repo.root, CACHE_PATH)
            cache = path.join(cache_dir, kind)
            if not path.isdir(cache_dir):
                os.makedirs(cache_dir)

            cache_exists = path.isfile(cache)

            cache_time = (datetime.fromtimestamp(os.stat(cache).st_mtime)
                          if cache_exists else None)
            if not cache_exists or cache_time < datetime.now() - CACHE_TIMEOUT:
                if not cache_exists:
                    open(cache, 'w').close()
                subprocess.Popen(['hg', 'prompt', '--cache-%s' % kind])

            if cache_exists:
                with open(cache) as c:
                    count = len(c.readlines())
                    if g[1]:
                        return _with_groups(g, str(count)) if count else ''
                    else:
                        return _with_groups(g, '') if count else ''
            else:
                return ''
        return _r

    def _rev(m):
        g = m.groups()

        parents = repo[None].parents()
        parent = 0 if '|merge' not in g else 1
        parent = parent if len(parents) > parent else None

        rev = parents[parent].rev() if parent is not None else -1
        return _with_groups(g, str(rev)) if rev >= 0 else ''

    def _root(m):
        return _with_groups(m.groups(), repo.root) if repo.root else ''

    def _status(m):
        g = m.groups()

        st = repo.status(unknown=True)[:5]
        modified = any(st[:4])
        unknown = len(st[-1]) > 0

        flag = ''
        if '|modified' not in g and '|unknown' not in g:
            flag = '!' if modified else '?' if unknown else ''
        else:
            if '|modified' in g:
                flag += '!' if modified else ''
            if '|unknown' in g:
                flag += '?' if unknown else ''

        return _with_groups(g, flag) if flag else ''

    def _tags(m):
        g = m.groups()

        sep = g[1][1:] if g[1] else ' '
        tags = repo[None].tags()

        return _with_groups(g, sep.join(tags)) if tags else ''

    def _task(m):
        try:
            task = extensions.find('tasks').current(repo)
            return _with_groups(m.groups(), task) if task else ''
        except KeyError:
            return ''

    def _tip(m):
        g = m.groups()

        format = short if '|short' in g else hex

        tip = repo[len(repo) - 1]
        rev = tip.rev()
        tip = format(tip.node()) if '|node' in g else tip.rev()

        return _with_groups(g, str(tip)) if rev >= 0 else ''

    def _update(m):
        if not repo.branchtags():
            # We are in an empty repository.
            return ''

        current_rev = repo[None].parents()[0]
        to = repo[repo.branchtags()[current_rev.branch()]]
        return _with_groups(m.groups(), '^') if current_rev != to else ''


    if opts.get("angle_brackets"):
        tag_start = r'\<([^><]*?\<)?'
        tag_end = r'(\>[^><]*?)?>'
        brackets = '<>'
    else:
        tag_start = r'\{([^{}]*?\{)?'
        tag_end = r'(\}[^{}]*?)?\}'
        brackets = '{}'

    patterns = {
        'bookmark': _bookmark,
        'branch(\|quiet)?': _branch,
        'closed(\|quiet)?': _closed,
        'count(\|[^%s]*?)?' % brackets[-1]: _count,
        'node(?:'
            '(\|short)'
            '|(\|merge)'
            ')*': _node,
        'patch(?:'
            '(\|applied)'
            '|(\|unapplied)'
            '|(\|count)'
            '|(\|quiet)'
            ')*': _patch,
        'patches(?:' +
            '(\|join\([^%s]*?\))' % brackets[-1] +
            '|(\|reverse)' +
            '|(\|hide_applied)' +
            '|(\|hide_unapplied)' +
            '|(\|pre_applied\([^%s]*?\))' % brackets[-1] +
            '|(\|post_applied\([^%s]*?\))' % brackets[-1] +
            '|(\|pre_unapplied\([^%s]*?\))' % brackets[-1] +
            '|(\|post_unapplied\([^%s]*?\))' % brackets[-1] +
            ')*': _patches,
        'queue': _queue,
        'rev(\|merge)?': _rev,
        'root': _root,
        'root\|basename': _basename,
        'status(?:'
            '(\|modified)'
            '|(\|unknown)'
            ')*': _status,
        'tags(\|[^%s]*?)?' % brackets[-1]: _tags,
        'task': _task,
        'tip(?:'
            '(\|node)'
            '|(\|short)'
            ')*': _tip,
        'update': _update,

        'incoming(\|count)?': _remote('incoming'),
        'outgoing(\|count)?': _remote('outgoing'),
    }

    if opts.get("cache_incoming"):
        _cache_remote(repo, 'incoming')

    if opts.get("cache_outgoing"):
        _cache_remote(repo, 'outgoing')

    for tag, repl in patterns.items():
        fs = re.sub(tag_start + tag + tag_end, repl, fs)
    ui.status(fs)

def _pull_with_cache(orig, ui, repo, *args, **opts):
    """Wrap the pull command to delete the incoming cache as well."""
    res = orig(ui, repo, *args, **opts)
    cache = path.join(repo.root, CACHE_PATH, 'incoming')
    if path.isfile(cache):
        os.remove(cache)
    return res

def _push_with_cache(orig, ui, repo, *args, **opts):
    """Wrap the push command to delete the outgoing cache as well."""
    res = orig(ui, repo, *args, **opts)
    cache = path.join(repo.root, CACHE_PATH, 'outgoing')
    if path.isfile(cache):
        os.remove(cache)
    return res

def uisetup(ui):
    extensions.wrapcommand(commands.table, 'pull', _pull_with_cache)
    extensions.wrapcommand(commands.table, 'push', _push_with_cache)

cmdtable = {
    "prompt":
    (prompt, [
        ('', 'angle-brackets', None, 'use angle brackets (<>) for keywords'),
        ('', 'cache-incoming', None, 'used internally by hg-prompt'),
        ('', 'cache-outgoing', None, 'used internally by hg-prompt'),
    ],
    'hg prompt STRING')
}
help.helptable += (
    (['prompt-keywords', 'prompt-keywords'], ('Keywords supported by hg-prompt'),
     (r'''hg-prompt currently supports a number of keywords.

Some keywords support filters.  Filters can be chained when it makes
sense to do so.  When in doubt, try it!

bookmark
     Display the current bookmark (requires the bookmarks extension).

branch
     Display the current branch.

     |quiet
         Display the current branch only if it is not the default branch.

closed
     Display `X` if working on a closed branch (i.e. committing now would reopen
     the branch).

count
     Display the number of revisions in the given revset (the revset `all()`
     will be used if none is given).

     See `hg help revsets` for more information.

     |REVSET
         The revset to count.

incoming
     Display nothing, but if the default path contains incoming changesets the
     extra text will be expanded.

     For example: `{incoming changes{incoming}}` will expand to
     `incoming changes` if there are changes, otherwise nothing.

     Checking for incoming changesets is an expensive operation, so `hg-prompt`
     will cache the results in `.hg/prompt/cache/` and refresh them every 15
     minutes.

     |count
         Display the number of incoming changesets (if greater than 0).

node
     Display the (full) changeset hash of the current parent.

     |short
         Display the hash as the short, 12-character form.

     |merge
         Display the hash of the changeset you're merging with.

outgoing
     Display nothing, but if the current repository contains outgoing
     changesets (to default) the extra text will be expanded.

     For example: `{outgoing changes{outgoing}}` will expand to
     `outgoing changes` if there are changes, otherwise nothing.

     Checking for outgoing changesets is an expensive operation, so `hg-prompt`
     will cache the results in `.hg/prompt/cache/` and refresh them every 15
     minutes.

     |count
         Display the number of outgoing changesets (if greater than 0).

patch
     Display the topmost currently-applied patch (requires the mq
     extension).

     |count
         Display the number of patches in the queue.

     |applied
         Display the number of currently applied patches in the queue.

     |unapplied
         Display the number of currently unapplied patches in the queue.

     |quiet
         Display a number only if there are any patches in the queue.

patches
     Display a list of the current patches in the queue.  It will look like
     this:

         :::console
         $ hg prompt '{patches}'
         bottom-patch -> middle-patch -> top-patch

     |reverse
         Display the patches in reverse order (i.e. topmost first).

     |hide_applied
         Do not display applied patches.

     |hide_unapplied
         Do not display unapplied patches.

     |join(SEP)
         Display SEP between each patch, instead of the default ` -> `.

     |pre_applied(STRING)
         Display STRING immediately before each applied patch.  Useful for
         adding color codes.

     |post_applied(STRING)
         Display STRING immediately after each applied patch.  Useful for
         resetting color codes.

     |pre_unapplied(STRING)
         Display STRING immediately before each unapplied patch.  Useful for
         adding color codes.

     |post_unapplied(STRING)
         Display STRING immediately after each unapplied patch.  Useful for
         resetting color codes.

queue
     Display the name of the current MQ queue.

rev
     Display the repository-local changeset number of the current parent.

     |merge
         Display the repository-local changeset number of the changeset you're
         merging with.

root
     Display the full path to the root of the current repository, without a
     trailing slash.

     |basename
         Display the directory name of the root of the current repository. For
         example, if the repository is in `/home/u/myrepo` then this keyword
         would expand to `myrepo`.

status
     Display `!` if the repository has any changed/added/removed files,
     otherwise `?` if it has any untracked (but not ignored) files, otherwise
     nothing.

     |modified
         Display `!` if the current repository contains files that have been
         modified, added, removed, or deleted, otherwise nothing.

     |unknown
         Display `?` if the current repository contains untracked files,
         otherwise nothing.

tags
     Display the tags of the current parent, separated by a space.

     |SEP
         Display the tags of the current parent, separated by `SEP`.

task
     Display the current task (requires the tasks extension).

tip
     Display the repository-local changeset number of the current tip.

     |node
         Display the (full) changeset hash of the current tip.

     |short
         Display a short form of the changeset hash of the current tip (must be
         used with the **|node** filter)

update
     Display `^` if the current parent is not the tip of the current branch,
     otherwise nothing.  In effect, this lets you see if running `hg update`
     would do something.
''')),
)
