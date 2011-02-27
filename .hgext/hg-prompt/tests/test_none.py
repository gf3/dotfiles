'''Test output without keywords.'''

from nose import *
from util import *


@with_setup(setup_sandbox, teardown_sandbox)
def test_blank():
    output = prompt(fs='')
    assert output == ''


@with_setup(setup_sandbox, teardown_sandbox)
def test_text():
    output = prompt(fs='test one two three')
    assert output == 'test one two three'


@with_setup(setup_sandbox, teardown_sandbox)
def test_invalid_keyword():
    output = prompt(fs='{invalidkeyword}')
    assert output == '{invalidkeyword}'
