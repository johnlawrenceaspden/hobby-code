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
    if label.is_empty(){
        println!("({}) type {} value {:?}" ,name, std::any::type_name_of_val(&*v), v);
    } else {
        println!("{} ({}) type {} value {:?}",label ,name, std::any::type_name_of_val(&*v), v);
    }
}


macro_rules! p {
    ($a: expr, $b: expr) => {
        p(&$a, stringify!($a), $b)
    };
    ($a: expr) => {
        p(&$a, stringify!($a),"")
    };
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
    a.unwrap_or(1) + b.unwrap_or(2)
}





fn main(){
    println!("hello");

    println!("{}",addopt(Some(3), Some(4)));
    println!("{}",addopt(None, Some(4)));
    println!("{}",addopt(Some(4),None));
    println!("{}",addopt(None, None));
    
    println!("{}",add!());
    println!("{}",add!(4));
    println!("{}",add!(4,3));

    let s=String::from("doom");
    p(&7, "7", "");
    p(&7, "7", "seven");
    p(&s, "s", "");

    
    p!(7, "seven");
    p!(7);
    p!(s, "the string s is");
    p!(s);

    p!(add!(4,3));
//    println!("{}",add!(4,3));
}
