'''Test output of {node}.'''

from nose import *
from util import *


def _parent_node():
    opts = { 'template': '{node}', 'rev': '.', 'date': None, 'user': None }
    
    _ui = get_sandbox_ui()
    _ui.pushbuffer()
    commands.log(_ui, get_sandbox_repo(), **opts)
    
    return _ui.popbuffer()


@with_setup(setup_sandbox, teardown_sandbox)
def test_node():
    output = prompt(fs='{node}')
    assert output == '0000000000000000000000000000000000000000'
    
    output = prompt(fs='{ at node {node}}')
    assert output == ' at node 0000000000000000000000000000000000000000'
    
    hg_commit()
    output = prompt(fs='{node}')
    assert output == _parent_node()
    
    hg_commit()
    output = prompt(fs='{node}')
    assert output == _parent_node()
    
    hg_update(0)
    output = prompt(fs='{node}')
    assert output == _parent_node()


@with_setup(setup_sandbox, teardown_sandbox)
def test_short_filter():
    output = prompt(fs='{node|short}')
    assert output == '0000000000000000000000000000000000000000'[:12]
    
    output = prompt(fs='{ at node {node|short}}')
    assert output == ' at node ' + '0000000000000000000000000000000000000000'[:12]
    
    hg_commit()
    output = prompt(fs='{node|short}')
    assert output == _parent_node()[:12]
    
    hg_commit()
    output = prompt(fs='{node|short}')
    assert output == _parent_node()[:12]
    
    hg_update(0)
    output = prompt(fs='{node|short}')
    assert output == _parent_node()[:12]


@with_setup(setup_sandbox, teardown_sandbox)
def test_merge_filter():
    hg_commit('one.txt')
    hg_commit('one.txt')
    node_to_merge = _parent_node()
    
    hg_update(0)
    hg_commit('two.txt')
    hg_merge(1)
    
    output = prompt(fs='{node|merge}')
    assert output == node_to_merge
    
    output = prompt(fs='{node|merge|short}')
    assert output == node_to_merge[:12]
