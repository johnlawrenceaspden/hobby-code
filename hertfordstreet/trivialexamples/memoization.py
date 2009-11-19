class MemoizeMutable:
    """Memoize(fn) - an instance which acts like fn but memoizes its arguments
       Will work on functions with mutable arguments (slower than Memoize)
    """
    def __init__(self, fn):
        self.fn = fn
        self.memo = {}
    def __call__(self, *args):
        import cPickle
        str = cPickle.dumps(args)
        if not self.memo.has_key(str):
            self.memo[str] = self.fn(*args)
        return self.memo[str]


def statictest(s,x):
    print "calculating for", s, x
    return s*sum(x)

statictest=MemoizeMutable(statictest)

print statictest(30, [2,3])
print statictest(20, [2,3])
print statictest(30, [2,3])
print statictest(20, [2,4])
