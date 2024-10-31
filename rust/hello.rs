// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https://doc.rust-lang.org/rust-by-example/hello/comment.html

   must read:
   https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html
   https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html

   https://dev.to/doziestar/rusts-ownership-system-memory-safety-without-garbage-collection-gp3

*/


// to print the type of a thing
// println!("{} {:?}",type_name_of_val(&a),a);

#[allow(unused_imports)]
use std::any::type_name_of_val;

// call this as p(&a)
#[allow(dead_code)]
fn p<T: std::fmt::Debug>(v: &T) {
    println!("{} {:?}", std::any::type_name_of_val(&*v), v);
}

fn doom(s:&mut String){
    s.push_str(" and even more doom");
}

fn main(){
    println!("hello");

    //but Strings have a part on the heap, and so they get moved and borrowed

    let mut m = String::from("impending");
    let m2 = &mut m;
    m2.push_str(" doom");
    m.push_str(" and more doom");

    let r1 = &m;
    let r2 = &m;

    println!("{}, {}", r1, r2);

    doom(&mut m);

    println!("{}, {}", m, m);
    
    p(&m);
    
}
