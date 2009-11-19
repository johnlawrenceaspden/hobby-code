def qsort(l):
    print "qsort: ", l
    if len(l)<=1:
        return l
    pivot= l[0]
    tail = l[1:]
    lower = [x for x in tail if x<pivot]
    upper = [x for x in tail if x>=pivot]
    print "lower", lower
    print "upper", upper
    return qsort(lower)+[pivot]+qsort(upper)

qsort([1,3,4,6,7,4,3,4,7])

# [1 3 4 6 7 4 3 4 7]
# 1 [3 4 6 7 4 3 4 7]
# 1 3 [4 6 7 4 3 4 7]
# 1 3 [3] 4 [6 7 4 4 7]
# 1 3 3 4 [ 4 4] 6 [7 7]
# 1 3 3 4 4 [4] 6 7 [7]
# 1 3 3 4 4 4 6 7 7

