// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */

// fn print_type_of<T: std::fmt::Debug>(v: &T) {
//     println!("{:?}{}", v, std::any::type_name::<T>());
// }

use std::any::type_name_of_val;

fn p<T: std::fmt::Debug>(v: &T) {
    println!("{} {:?}", std::any::type_name::<T>(), v);
}

fn s<T: std::fmt::Debug>(v: T) {
    println!("{} {} {:?}", std::any::type_name_of_val(&v), std::any::type_name::<T>(), v);
}


// I have no idea what is going on here
fn main(){
    println!("hello");

    let mut m = 32.90;
    m=m+0.1;
    println!("{} {:?}", type_name_of_val(&m), m);
    s(m);
    p(&m);

    let m = [1,1,1];
    println!("{} {:?}", type_name_of_val(&m), m);
    s(m);
    p(&m);

    
    let a = "hello!\n".as_bytes();
    println!("{} {:?}",type_name_of_val(a),a);
    println!("{} {:?}",type_name_of_val(&a),a);
    s(a);
    p(&a);
}
