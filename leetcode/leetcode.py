class Solution(object):
    def maximumOddBinaryNumber(self, s):
        """
        :type s: str
        :rtype: str
        """
        return "".join(list(reversed(sorted(s)[0:-1])) + ["1"])


s = Solution()
s.maximumOddBinaryNumber("01010")
