// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html 
   fn main(){println!("hello")}
*/


// fn print_type_of<T: std::fmt::Debug>(v: &T) {
//     println!("{:?}{}", v, std::any::type_name::<T>());
// }

#[allow(unused_imports)]
use std::any::type_name_of_val;

// to print the type of a thing
// println!("{} {:?}",type_name_of_val(a),a);

//call this as s(&a)
#[allow(dead_code)]
fn s<T: std::fmt::Debug>(v: &T) {
    println!("{} {} {:?}", std::any::type_name::<T>(), std::any::type_name_of_val(v),v);
}
//call this as p(a)
#[allow(dead_code)]
fn p<T: std::fmt::Debug>(v: T) {
    println!("{} {:?}", std::any::type_name_of_val(&v), v);
}

fn main(){
    println!("hello");
    let a = "hello";
    println!("{} {:?}",type_name_of_val(a),a);
    p(a);
    s(&a);
    
    let a=[1,2,3];
    //println!("{} {:?}",type_name_of_val(a),a);
    p(a);
    s(&a);

    let a="hello".as_bytes();
    println!("{} {:?}",type_name_of_val(a),a);
    p(a);
    s(&a);

}
