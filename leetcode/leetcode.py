# Definition for singly-linked list.
from typing import Optional


class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next


def sillylist2list(l):
    L = []
    while l:
        L = L + [l.val]
        l = l.next
    return L


def list2sillylist(L):
    if L == []:
        return None
    else:
        l = ListNode(L[-1])
        for c in list(reversed(L))[1:]:
            l = ListNode(c, l)
        return l


class Solution:
    def addTwoNumbers(
        self, l1: Optional[ListNode], l2: Optional[ListNode]
    ) -> Optional[ListNode]:
        L1 = sillylist2list(l1)
        L2 = sillylist2list(l2)

        L3 = [
            int(i)
            for i in list(
                reversed(
                    list(
                        str(
                            int("".join([str(i) for i in reversed(L1)]))
                            + int("".join([str(i) for i in reversed(L2)]))
                        )
                    )
                )
            )
        ]

        return list2sillylist(L3)


s = Solution()


def test(l1, l2, result):
    r = sillylist2list(s.addTwoNumbers(list2sillylist(l1), list2sillylist(l2)))
    print(l1, l2, "->", r)
    assert r == result


test([2, 4, 3], [5, 6, 4], [7, 0, 8])
test([0], [0], [0])
test([9, 9, 9, 9, 9, 9, 9], [9, 9, 9, 9], [8, 9, 9, 9, 0, 0, 0, 1])
