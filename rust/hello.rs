// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https://doc.rust-lang.org/rust-by-example/hello/comment.html

   must read:
   https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html
   https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html

   https://dev.to/doziestar/rusts-ownership-system-memory-safety-without-garbage-collection-gp3

*/


// to print the type of a thing
// println!("{} {:?}",type_name_of_val(a),a);

#[allow(unused_imports)]
use std::any::type_name_of_val;

//call this as p(a)
#[allow(dead_code)]
fn p<T: std::fmt::Debug>(v: T) {
    println!("{} {:?}", std::any::type_name_of_val(&v), v);
}

fn main(){
    println!("hello");

    let mut m = 32.90;
    m=m+0.1;

    p(m);
}
