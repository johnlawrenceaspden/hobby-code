;;http://w01fe.com/blog/category/programming/lisp-programming/page/3/

(apply distinct? (interleave (iterate inc 0) (iterate inc 10)))
