// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */


// to print the type of a thing
// println!("{} {:?}",type_name_of_val(a),a);

#[allow(unused_imports)]
use std::any::type_name_of_val;

//call this as p(a)
#[allow(dead_code)]
fn p<T: std::fmt::Debug>(v: T) {
    println!("{} {:?}", std::any::type_name_of_val(&v), v);
}

use std::fs;

#[allow(unused_must_use)] 
fn main(){
    println!("hello");

    // write a file, causes warning about unused result, which apparently can't be ignored here,
    // have to #[allow(unused_must_use)] for all of main
    fs::write("hello.txt","fluffy");
    
    // causes warning about unused variable, which we can tell the compiler to ignore
    #[allow(unused_variables)]
    let a=fs::write("hello.txt","fluffy!"); 

    // does not cause warning about unused variable
    let _=fs::write("hello.txt","fluffy!!");
    //p(_); can't actually use the _ variable
    
    //just looking at the result makes the warning go away
    p(fs::write("hello.txt","fluffy!!!"));

    // you can also unwrap it
    fs::write("hello.txt","fluffy!!").unwrap();
    p(fs::write("hello.txt","fluffy!!").unwrap());

    
    let c="hello!\n".as_bytes(); p(c);
    let m = fs::write(format!("hello.txt"), c);  p(m);

    println!("read it back");
    let a = fs::read("hello.txt"); p(a);
    let a = fs::read("hello.txt").unwrap();

    // p(a);
    p(a.clone()); //mysteriously have to use clone here or p steals the value??

    
    let a = std::str::from_utf8(&a).unwrap(); p(a);

    println!("hello.txt: {}",a);

}
