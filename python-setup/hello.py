#!/usr/bin/env python

# Setup elpy as described in:
# https://github.com/jorgenschaefer/elpy/wiki

# And work through:
# http://ivory.idyll.org/articles/nose-intro.html

# any function that starts with test gets run with
# nosetests hello.py
# you can see which tests were run with:
# nosetests -v hello.py

import re

EMAIL_REGEXP = r'[\S.]+@[\S.]+'


def test_email_regexp():
    # a regular e-mail address should match
    assert re.match(EMAIL_REGEXP, 'test@nowhere.com')

    # no domain should fail
    assert not re.match(EMAIL_REGEXP, 'test@')


class TestExampleTwo:
    def test_c(self):
        assert 'c' == 'c'


def test_b():
    assert 'b' == 'b'

if __name__ == '__main__':
    print "hello"
    print re.match(r'[ab]*', "ababab")
    print re.match(r'[ab]*', "abababa")
