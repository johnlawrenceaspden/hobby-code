use ferris_says::say;
use std::io::{stdout, BufWriter};

/* Block Comment */
// line comment
/// weird comment

fn main() {
    println!("Hello, world!");
    let stdout=stdout();
    let message=String::from("Yo bitches");
    let width=message.chars().count();
    let mut writer = BufWriter::new(stdout.lock());
    say(&message, width, &mut writer).unwrap();
}
