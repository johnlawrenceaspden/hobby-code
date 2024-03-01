#!/usr/bin/env python3

from typing import Optional
import sys

print(sys.version)


# Definition for singly-linked list.
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

    def __repr__(self):
        return str(self.val) + ((", " + self.next.__repr__()) if (self.next) else "")


assert repr(ListNode(2, next=ListNode(4, next=ListNode(3)))) == "2, 4, 3"


# Seriously, linked-list shenanigans in Python? Oh well, I can write lisp in any syntax


def cons(v, nodelist):
    return ListNode(val=v, next=nodelist)


def car(lst):
    return lst.val


def cdr(lst):
    return lst.next


assert car(cdr(ListNode(2, next=ListNode(4, next=ListNode(3))))) == 4
assert (car(cdr(cons(0, cons(1, None))))) == 1

# take a nice sensible python list and turn it into the above abortion
def makeList(nodelist):
    a = None
    for v in reversed(nodelist):
        a = cons(v, a)
    return a


def reverse(l1):
    a = None
    while l1:
        a = cons(car(l1), a)
        l1 = cdr(l1)
    return a


def compare(l1, l2):
    while l1 and l2:
        # print(l1, ":", l2)

        if car(l1) != car(l2):
            return False
        l1 = cdr(l1)
        l2 = cdr(l2)

    if not (l1) and not (l2):
        return True
    else:
        return False


assert compare(cons(4, makeList([1, 2])), makeList([4, 1, 2]))
assert compare(cons(4, makeList([])), makeList([4]))
assert compare(reverse(reverse(makeList([2, 4, 3]))), reverse(makeList([3, 4, 2])))
assert compare(reverse(makeList([2, 4, 3])), makeList([3, 4, 2]))
assert compare(makeList([5, 6, 4]), makeList([5, 6, 4]))
assert not compare(makeList([5, 6, 4]), makeList([5, 5, 4]))


class TreeNode:
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

##########################################################################################
print("end of boilerplate")


def merge(l1, l2):
    a = None
    while l1 and l2:
        if l1.val < l2.val:
            a = cons(l1.val, a)
            l1 = l1.next
        else:
            a = cons(l2.val, a)
            l2 = l2.next
        # print(a, ":", l1, ":", l2)

    while l1:
        a = cons(l1.val, a)
        l1 = l1.next
        # print(a, ":", l1, ":", l2)

    while l2:
        a = cons(l2.val, a)
        l2 = l2.next
        # print(a, ":", l1, ":", l2)

    a = reverse(a)
    return a


class Solution:
    def mergeTwoLists(
        self, l1: Optional[ListNode], l2: Optional[ListNode]
    ) -> Optional[ListNode]:
        return merge(l1, l2)


# test cases

inputs = [
    ([], [], []),
    ([], [0], [0]),
    ([0], [], [0]),
    ([0], [1], [0, 1]),
    ([0], [1, 2], [0, 1, 2]),
    ([0, 2], [1], [0, 1, 2]),
    ([1, 2, 3], [1, 3, 4], [1, 1, 2, 3, 3, 4]),
    ([1, 2, 4], [1, 3, 4], [1, 1, 2, 3, 4, 4]),
]

for i in inputs:
    l1 = makeList(i[0])
    l2 = makeList(i[1])
    r = makeList(i[2])
    print(l1, l2, " -> ", repr(r))
    retval = Solution().mergeTwoLists(l1, l2)
    print(l1, l2, " -> ", repr(retval))
    assert compare(retval, r)
