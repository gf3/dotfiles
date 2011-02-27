'''Test output of {update}.'''

from nose import *
from util import *


@with_setup(setup_sandbox, teardown_sandbox)
def test_empty_repo():
    output = prompt(fs='{update}')
    assert output == ''
    
    output = prompt(fs='{ need to update? {update}}')
    assert output == ''


@with_setup(setup_sandbox, teardown_sandbox)
def test_single_branch():
    hg_commit()
    
    output = prompt(fs='{update}')
    assert output == ''
    
    output = prompt(fs='{ need to update? {update}}')
    assert output == ''
    
    hg_commit()
    
    output = prompt(fs='{update}')
    assert output == ''
    
    output = prompt(fs='{ need to update? {update}}')
    assert output == ''
    
    hg_update(0)
    
    output = prompt(fs='{update}')
    assert output == '^'
    
    output = prompt(fs='{ need to update? {update}}')
    assert output == ' need to update? ^'


@with_setup(setup_sandbox, teardown_sandbox)
def test_multiple_branches():
    hg_commit()
    hg_commit()
    hg_commit()
    
    hg_update(0)
    hg_commit('two.txt')
    hg_commit('two.txt')
    
    #  @    4
    #  |
    #  o    3 
    #  |
    #  | o  2
    #  | |
    #  | o  1
    #  |/
    #  |
    #  o    0
    
    hg_log()
    
    hg_update(4)
    output = prompt(fs='{update}')
    assert output == ''
    
    hg_update(3)
    output = prompt(fs='{update}')
    assert output == '^'
    
    # This test case matches the behavior of Mercurial, but it seems a bit
    # unintuitive to me.
    hg_update(2)
    output = prompt(fs='{update}')
    assert output == '^'
    
    hg_update(1)
    output = prompt(fs='{update}')
    assert output == '^'
    
    hg_update(0)
    output = prompt(fs='{update}')
    assert output == '^'


@with_setup(setup_sandbox, teardown_sandbox)
def test_multiple_named_branches():
    hg_commit()
    hg_commit()
    hg_commit()
    
    hg_update(0)
    hg_branch('test')
    hg_commit('two.txt')
    hg_commit('two.txt')
    
    #  @    4 (test)
    #  |
    #  o    3 (test)
    #  |
    #  | o  2 (default)
    #  | |
    #  | o  1 (default)
    #  |/
    #  |
    #  o    0 (default)
    
    hg_log()
    
    hg_update(4)
    output = prompt(fs='{update}')
    assert output == ''
    
    hg_update(3)
    output = prompt(fs='{update}')
    assert output == '^'
    
    hg_update(2)
    output = prompt(fs='{update}')
    assert output == ''
    
    hg_update(1)
    output = prompt(fs='{update}')
    assert output == '^'
    
    hg_update(0)
    output = prompt(fs='{update}')
    assert output == '^'

