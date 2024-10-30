// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
https:doc.rust-lang.org/rust-by-example/hello/comment.html
 */

fn main() {
    let mut count = 0;
    println!(
        "{}",
        loop {
            count += 1;
            if count > 10 {
                break count;
            };
            println!("hello");
            if count > 5 {
                continue;
            };
            let mut count2 = 0;
            loop {
                count2 += 1;
                println!("helloooooo");
                if count2 > count {
                    break;
                }
            }
        }
    )
}
