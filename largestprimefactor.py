#!/usr/bin/env python

[ x for x in range(2,1000000) if (600851475 % x == 0)]

# [3, 5, 7, 9, 15, 21, 25, 35, 45, 49, 63, 75, 105, 147, 175, 225, 245, 315, 441, 525, 735, 1225, 1575, 2205, 3675, 11025, 54499, 163497, 272495, 381493, 490491, 817485]


[ x for x in range(2,1000000) if (600851475/3 % x == 0)]
# [3, 5, 7, 15, 21, 25, 35, 49, 75, 105, 147, 175, 245, 525, 735, 1225, 3675, 54499, 163497, 272495, 381493, 817485]

[ x for x in range(2,1000000) if (600851475/3/3 % x == 0)]
# [5, 7, 25, 35, 49, 175, 245, 1225, 54499, 272495, 381493]

[ x for x in range(2,1000000) if (600851475/3/3/5 % x == 0)]
# [5, 7, 35, 49, 245, 54499, 272495, 381493]

[ x for x in range(2,1000000) if (600851475/3/3/5/5 % x == 0)]
# [7, 49, 54499, 381493]

[ x for x in range(2,1000000) if (600851475/3/3/5/5/7 % x == 0)]
# [7, 54499, 381493]

[ x for x in range(2,1000000) if (600851475/3/3/5/5/7/7 % x == 0)]
# [54499]

3*3*5*5*7*7*54499
#600851475