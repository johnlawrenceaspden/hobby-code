// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */

fn print_type_of<T: std::fmt::Debug>(v: &T) {
    println!("{:?}{}", v, std::any::type_name::<T>());
}

#[allow(unused_variables)]
fn main() {
    println!("hello ");

    // Variables can be type annotated.
    let logical: bool = true;

    let a_float: f64 = 1.0;  // Regular annotation
    let an_integer   = 5i32; // Suffix annotation

    // Or a default will be used.
    let default_float   = 3.0; // `f64`
    let default_integer = 7;   // `i32`

    print_type_of(&(logical, a_float, an_integer, default_float, default_integer));
    
    // A type can also be inferred from context.
    let mut inferred_type = 12; // Type i64 is inferred from another line.
    print_type_of(&(inferred_type,));
    inferred_type = 4294967296i64;
    print_type_of(&(inferred_type,));

    // A mutable variable's value can be changed.
    let mut mutable = 12; // Mutable `i32`

    print_type_of(&(mutable,));
    mutable = 21;

    print_type_of(&(mutable,));
    // Error! The type of a variable can't be changed.
    // mutable = true;
    
    mutable = 22;
    print_type_of(&(mutable,));

    // Variables can be overwritten with shadowing.
    let mutable = true;
    print_type_of(&(mutable,));
    /* Compound types - Array and Tuple */

    // Array signature consists of Type T and length as [T; length].
    let my_array= [1, 2, 3, 4, 5];
    print_type_of(&(my_array,));

    let my_array :[i32;5]= [1, 2, 3, 4, 5];
    print_type_of(&(my_array,));
    // Tuple is a collection of values of different types 
    // and is constructed using parentheses ().
    let my_tuple = (5u32, 1u8, true, -5.04f32);
    print_type_of(&(my_tuple,));

}
