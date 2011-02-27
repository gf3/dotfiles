'''Test output of {tip}.'''

from nose import *
from util import *


def _tip_rev():
    opts = { 'template': '{rev}', }
    
    _ui = get_sandbox_ui()
    _ui.pushbuffer()
    commands.tip(_ui, get_sandbox_repo(), **opts)
    
    return _ui.popbuffer()

def _tip_node():
    opts = { 'template': '{node}', }
    
    _ui = get_sandbox_ui()
    _ui.pushbuffer()
    commands.tip(_ui, get_sandbox_repo(), **opts)
    
    return _ui.popbuffer()


@with_setup(setup_sandbox, teardown_sandbox)
def test_empty_repo():
    output = prompt(fs='{tip}')
    assert output == ''
    
    output = prompt(fs='{ the tip is at {tip}}')
    assert output == ''


@with_setup(setup_sandbox, teardown_sandbox)
def test_tip():
    hg_commit()
    
    output = prompt(fs='{tip}')
    assert output == _tip_rev()
    
    output = prompt(fs='{ the tip is {tip}}')
    assert output == ' the tip is %s' % _tip_rev()
    
    hg_commit()
    output = prompt(fs='{tip}')
    assert output == _tip_rev()


@with_setup(setup_sandbox, teardown_sandbox)
def test_node_filter():
    hg_commit()
    
    output = prompt(fs='{tip|node}')
    assert output == _tip_node()
    
    output = prompt(fs='{ the tip is {tip|node}}')
    assert output == ' the tip is %s' % _tip_node()
    
    hg_commit()
    output = prompt(fs='{tip|node}')
    assert output == _tip_node()


@with_setup(setup_sandbox, teardown_sandbox)
def test_short_filter():
    hg_commit()
    
    output = prompt(fs='{tip|node|short}')
    assert output == _tip_node()[:12]
    
    output = prompt(fs='{ the tip is {tip|node|short}}')
    assert output == ' the tip is %s' % _tip_node()[:12]
    
    hg_commit()
    output = prompt(fs='{tip|node|short}')
    assert output == _tip_node()[:12]
