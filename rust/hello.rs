// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */

fn main(){
    println!("hello");
    for n in 1..10{
        println!("{0} yo {0}",n);
    }
}
