// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */

fn factorial(n:i32) ->i32{
    if n==0 {return 1;}
    else {return n*factorial(n-1);}
}

fn main(){
    println!("yo {}",factorial(12));
    println!("yo {}",factorial(13));
}
