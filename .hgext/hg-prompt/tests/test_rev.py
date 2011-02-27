'''Test output of {node}.'''

from nose import *
from util import *


def _parent_rev():
    opts = { 'template': '{rev}', 'rev': '.', 'date': None, 'user': None }
    
    _ui = get_sandbox_ui()
    _ui.pushbuffer()
    commands.log(_ui, get_sandbox_repo(), **opts)
    
    return _ui.popbuffer()


@with_setup(setup_sandbox, teardown_sandbox)
def test_nullrev():
    output = prompt(fs='{rev}')
    assert output == ''
    
    output = prompt(fs='{ at revision {rev}}')
    assert output == ''


@with_setup(setup_sandbox, teardown_sandbox)
def test_rev():
    hg_commit()
    
    output = prompt(fs='{rev}')
    assert output == _parent_rev()
    
    output = prompt(fs='{ at revision {rev}}')
    assert output == ' at revision %s' % _parent_rev()
    
    hg_commit()
    output = prompt(fs='{rev}')
    assert output == _parent_rev()


@with_setup(setup_sandbox, teardown_sandbox)
def test_merge_filter():
    hg_commit('one.txt')
    hg_commit('one.txt')
    rev_to_merge = _parent_rev()
    
    hg_update(0)
    hg_commit('two.txt')
    hg_merge(1)
    
    output = prompt(fs='{rev|merge}')
    assert output == rev_to_merge
    
    output = prompt(fs='{ merging with {rev|merge}}')
    assert output == ' merging with %s' % rev_to_merge
