#!/usr/bin/env python3

# (1 2 4 5) (1 3 4 6) = 1->2, 2->4->6, 6->1, 3->4, 4->5, 5->1->3 = (1 2 6)(3 4 5)

a = [1, 2, 4, 5]
b = [1, 3, 4, 6]

ad = dict(zip(a, a[1:] + a[0:1]))
bd = dict(zip(b, b[1:] + b[0:1]))

jr = list(set([*ad.keys(), *bd.keys()]))

im = [bd.get(j, j) for j in [ad.get(i, i) for i in jr]]

print(jr)
print(im)

jd = dict(zip(jr, im))

elts = set(jd.keys())

ll = []
while elts:
    i = min(elts)
    print("(", i, end=",")
    cyc = [i]
    j = i
    while True:
        elts.remove(j)
        j = jd[j]
        if j == i:
            break
        print(j, end=",")
        cyc.append(j)
    print(")")
    ll.append(cyc)

print(ll)
