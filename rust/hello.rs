// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https://doc.rust-lang.org/rust-by-example/hello/comment.html
*/


// to print the type of a thing
// println!("{} {:?}",type_name_of_val(&a),a);

#[allow(unused_imports)]
use std::any::type_name_of_val;

// call this as p(&a, "name", "label")
#[allow(dead_code)]
fn p<T: std::fmt::Debug>(v: &T, name:&str, label:&str) {
    let s = format!("({}) type {} value {:?}" ,name, std::any::type_name_of_val(&*v), v);
    if label.is_empty(){
        println!("{s}");
    } else {
        println!("{label} {s}");
    }
}


macro_rules! p {
    ($a: expr, $b: expr) => {
        p(&$a, stringify!($a), $b)
    };
    ($a: expr) => {
        p(&$a, stringify!($a),"")
    };
    () =>{println!("");}
}



fn add(a: i32, b: i32) -> i32 {
    a + b
}

macro_rules! add {
    ($a: expr, $b: expr) => {
        add($a, $b)
    };
    ($a: expr) => {
        add($a, 0)
    };
    () => {
        add(0, 0)
    };
}


fn addopt(a: Option<i32>, b: Option<i32>) -> i32 {
    a.unwrap_or(0) + b.unwrap_or(0)
}





fn main(){
    println!("hello");


    
    p!(addopt(Some(3), Some(4)),"add two real things");
    p!(addopt(None, Some(4)), "dummy first argument");
    p!(addopt(Some(4),None), "dummy second argument");
    p!(addopt(None, None), "no arguments at all");

    p!();
    
    p!(add!());
    p!(add!(4));
    p!(add!(4,3));

    p!();

    p(&7, "7", "");
    p(&7, "7", "seven");
    p!(7, "seven");
    p!(7);

    p!();
    
    let s=String::from("doom");
    p(&s, "s", "");
    p!(s, "the string s is");
    p!(s);
}
