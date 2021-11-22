#!/usr/bin/env python3

import sys

print(sys.version)


# Definition for singly-linked list.
class ListNode(object):
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

    def __repr__(self):
        return str(self.val) + ((", " + self.next.__repr__()) if (self.next) else "")


# a3=ListNode(3)
# a2=ListNode(4,next=a3)
# a=ListNode(2,next=a2)


def makeList(nodelist):
    a = None
    for v in reversed(nodelist):
        a = ListNode(v, next=a)
    return a


# l1= makeList([2,4,3])
# l2= makeList([5,6,4])


class TreeNode(object):
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


t = TreeNode(1, TreeNode(2, TreeNode(3, TreeNode(4))), TreeNode(5))


def preorderTraversal(root):
    if root:
        return [root.val] + preorderTraversal(root.left) + preorderTraversal(root.right)
    else:
        return []


print(preorderTraversal(t))

print("end of boilerplate")


# end of useful boilerplate

pairs = {"(": ")", "[": "]", "{": "}", "<": ">"}
openings = pairs.keys()
closings = pairs.values()


def cancels(b, a):
    return pairs[b] == a


def isV(s):
    prefix = ""
    while s:
        # print(s, ":", prefix, ":", s[0])
        if s[0] in openings:
            prefix = prefix + s[0]
            s = s[1:]
        elif s[0] in closings:
            if prefix and cancels(prefix[-1], s[0]):
                prefix = prefix[:-1]
                s = s[1:]
            else:
                return False
        else:
            s = s[1:]
    if prefix:
        return False
    else:
        return True


class Solution:
    def isValid(self, s: str) -> bool:
        return isV(s)


# test cases

inputs = {
    "()[]{}": True,
    "<()[]{}>": True,
    "()": True,
    "()[]{}": True,
    "(<)>[]{}": False,
    "(]": False,
    "([)]": False,
    "{[]}": True,
    "[": False,
    "]": False,
    "<>": True,
    "": True,
    "a": True,
    "<a>()": True,
}


for (i, v) in inputs.items():
    print(i, " -> ", repr(v))
    retval = Solution().isValid(i)
    print(i, " -> ", repr(retval))
    assert retval == v
