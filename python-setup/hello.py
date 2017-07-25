#!/usr/bin/env python

# Setup elpy as described in:
# https://github.com/jorgenschaefer/elpy/wiki

# And work through:
# http://ivory.idyll.org/articles/nose-intro.html

# any function that starts with test gets run with
# nosetests hello.py
# you can see which tests were run with:
# nosetests -v hello.py
# -vv tells you where nose is looking for tests
# nosetests -vv hello.py
# and you can include doctests
# nosetests --with-doctest -vv hello.py

import unittest

# this is a nose test, it gets picked up by virtue of starting test_
def test_b():     
    assert 'b' == 'b'

# this too
class TestExampleTwo:
    def test_c(self):
        assert 'c' == 'c'

# this is a classic unittest testcase, nose should pick it up too
class ExampleTest(unittest.TestCase):
    def test_b(self):
        self.assert_(1 == 2)


# and this is a doctest, which nose will pick up if told to
def multiply(a, b):
    """
    >>> multiply(3,4)
    12
    >>> multiply(-1, -1)
    1
    """
    return a*b




# here we run the various test frameworks programmatically
if __name__ == '__main__':
    print "hello"

    print "-------------nose"
    import nose
    nose.run(argv=['', '-vv', '--exe', '--with-doctest', './hello.py'])

    print "-------------doctest"
    import doctest
    doctest.testmod()

    print "------------unittest"
    import unittest
    try:
        unittest.main()
    except SystemExit, exc:
        pass
