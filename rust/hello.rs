// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */

fn factorial(n:i128) ->i128{
    if n==0 {return 1;}
    else {return n*factorial(n-1);}
}

fn main(){
    for n in 1..{
        println!("{} yo {}",n,factorial(n));
    }
}
