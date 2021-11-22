#!/usr/bin/env python3

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
