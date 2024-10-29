// you can run me with rustc hello.rs && ./hello

/* Playing with the tutorial at
   https:doc.rust-lang.org/rust-by-example/hello/comment.html */

use std::fmt::{self, Formatter, Display};

#[derive(Debug)]
struct City {
    name: &'static str,
    // Latitude
    lat: f32,
    // Longitude
    lon: f32,
}

const CITIES:[City;3]=
    [
        City { name: "Dublin", lat: 53.347778, lon: -6.259722 },
        City { name: "Oslo", lat: 59.95, lon: 10.75 },
        City { name: "Vancouver", lat: 49.25, lon: -123.1 },
    ];


impl Display for City {
    // `f` is a buffer, and this method must write the formatted string into it.
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let lat_c = if self.lat >= 0.0 { 'N' } else { 'S' };
        let lon_c = if self.lon >= 0.0 { 'E' } else { 'W' };

        // `write!` is like `format!`, but it will write the formatted string
        // into a buffer (the first argument).
        write!(f, "{}: {:.3}°{} {:.3}°{}",
               self.name, self.lat.abs(), lat_c, self.lon.abs(), lon_c)
    }
}


#[derive(Debug)]
struct Colour {
    red: u8,
    green: u8,
    blue: u8,
}

impl Display for Colour{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result{
        write!(f,"0x{:0>2X}{:0>2X}{:0>2X}",self.red, self.green,self.blue)}
}

const COLOURS:[Colour;3]=[
        Colour { red: 128, green: 255, blue: 90 },
        Colour { red: 0, green: 3, blue: 254 },
        Colour { red: 0, green: 0, blue: 0 },
    ];

fn main() {
    for city in CITIES {
        println!("{:?}", city);
        println!("{}", city);
    }
    for colour in COLOURS {
        // Switch this to use {} once you've added an implementation
        // for fmt::Display.
        println!("{:?}", colour);
        println!("{}", colour);
    }
}

