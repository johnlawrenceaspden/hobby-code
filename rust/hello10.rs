// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https://doc.rust-lang.org/rust-by-example/hello/comment.html

   must read:
   https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html
   https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html

   https://dev.to/doziestar/rusts-ownership-system-memory-safety-without-garbage-collection-gp3

*/


// to print the type of a thing
// println!("{} {:?}",type_name_of_val(a),a);

#[allow(unused_imports)]
use std::any::type_name_of_val;

//call this as p(a)
#[allow(dead_code)]
fn p<T: std::fmt::Debug>(v: T) {
    println!("{} {:?}", std::any::type_name_of_val(&v), v);
}

fn pborrow<T: std::fmt::Debug>(v: &T) {
    println!("{} {:?}", std::any::type_name_of_val(&*v), v);
}


fn main(){
    println!("hello");
    
    let mut s = String::from("hello"); // s comes into scope, "hello" is a string literal, hardcoded into the program text
    // from that we create a String, which is a complex object on the heap
    // String::from has allocated memory on the heap
    
    // guess that s is on the stack, containing a pointer
    // or more properly there is a pointer on the stack called s, which points at the object in the heap
    println!("{s}");

    // Normally we'd have to either free the heap object manually, or rely on the garbage collector to do it.
    // if we forget to free, that's a memory leak
    // if we free too early that will cause chaos
    // if we free twice that might also cause chaos
    // you need to free once and only once, after the variable is used, but before s goes out of scope and the address of the heap object is lost.
    // Rust has no garbage collector and no way to free things, so there'd be a memory leak after this function ends

    // Rust frees the object when s goes out of scope, by calling the drop function

    // Which it seems we can't get to? String::drop?
    // s.drop();

    // This pattern is called RAII Resource Acquistion is Initialization in C++
    
    //having s mutable allows us to modify the object on the stack
    // if s i not mutable we can't do this, the compiler complains that it cannot borrow s as mutable because it is not declared to be mutable.
    s.push_str(", world!");

    println!("{s}"); // this prints hello world!

    println!("{}",type_name_of_val(&s)); // don't know why I have to take a reference here

    println!("{s}"); // this prints hello world!
    
    let a=s.clone(); println!("{} {:?}",type_name_of_val(&a),a);

    //bind the value 5 to x, then copy it and bind the value 5 to y
    let mut x = 5;
    let y = x;
    p(x); p(y);
    // there should be two 32-bit fives on the stack, x and y are names for the stack positions

    x=6;
    
    p(x); p(y);

    let s1 = String::from("hello");

    // s1 refers to a position on the stack where there are a pointer, a length, and a capacity
    //    the pointer is to the characters h e l l o on the heap.
    // the length is the current length of the string, the capacity is how much memory the allocator gave us.
    
    let s2 = s1; // This only moves the reference, it does not make a second object

    // value has moved to s2, so s1 can no longer be used!
    // and this is a compile time error
    // p(s1); //borrow of moved value;
    // imagine the s2 copied the three things in s1 and then nulled it
    // rust calls this shallow copy a move, because it invalidates the previous data
    // this means it doesn't get freed twice when s1 and s2 go out of scope

    // we can make a copy of both metadata and heap data (deep copy)
    let s3=s2.clone();

    // Irritatingly my type-printing function does the same thing
    p(s2.clone()); p(s3);

    // and so once you've printed the strings out you can't do it again.
    p(s2);  // this is fine because p got passed a deep copy so didn't destroy s2
    //p(s3); //this one's an error because s3 got moved when p(s3) was called

    //p(s2); // now an error
    // There is a Copy trait for things that get duplicated on the stack,
    // e.g. ints bools floats chars and tuples that only contain Copy types
    // And a Drop trait for things that need deallocation

    let s1=String::from("hello again");
    let s2=s1; //moved it
    pborrow(&s2); // give pborrow a reference to s2, rather than pborrow having to steal it
    // since pborrow does not own the string, it does not get dropped when pborrow exits
    // p(s1); //error because it got moved
    p(s2.clone()); // we're fine because pborrow only borrowed it
    p(s2); // fine because the last call of p destroyed a deep copy made with clone()
    // p(s2); // would be an error
    
}
