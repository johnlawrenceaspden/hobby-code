from collections import Counter

for S in range(1, 101):
    T = [(x + 1) * (y + 1) for x in range(S) for y in range(S)]
    C = Counter(T)
    m = max(C.values())
    most = [(c, v) for (c, v) in C.items() if v == m]
    if len(most) == 1:
        print(f"In a {S} by {S} grid {[c for (c,v) in most]}, appears {m} times")
    else:
        print(f"In a {S} by {S} grid {[c for (c,v) in most]}, appear {m} times")
