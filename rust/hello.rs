// you can run me with rustc hello.rs && ./hello

/*
Playing with the tutorial at
https:doc.rust-lang.org/rust-by-example/hello/comment.html
/* block /*comments*/ nest*/
*/

///weird-ass comment, wonder what this does?
fn main(){
    println!("Hello World from Rust!");
    println!("I am instructed to add a second line here but am a bit too crusty...");
    println!("{} days",3); // oh that's nice println("%i days",3);
    println!("{0} is {0} and all alone","one"); // args numbered from 0
    println!("{subject} {object} {verb}", subject = "ego",  verb="amo", object="te",);
    let message = 416;
    println!("{0},Ob{0:b},Ox{0:x}", message);
    println!("{},Ob{:b}", 0b110100000,0b110100000);
    println!("{:0>5}",23);
    println!("{:0<5}",23); //the fuck?
    println!("{:0>width$}", 23, width=50); //note the $

    //structures we have, and derive(Debug) gives us a default print
    #[derive(Debug)]
    struct Structure(i32,i64);
    let s=Structure(9,13);
    println!("{:?}",s);

    let pi=3.14159265358;
    println!("pi is roughly {pi:.3}");
    #[derive(Debug)]
    struct Deep(Structure);
    println!("{0:?}{0:#?}",Deep(Structure(7,12)));
}
