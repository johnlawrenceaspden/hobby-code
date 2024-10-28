// you can run me with rustc hello.rs && ./hello

fn main(){
    for n in 1..10{
        println!("{0} yo {0}",n);
    }
    
    let a=["milton","chesterton"].map(|s|{format!("inputs/{s}.txt")});
    println!("{:?}",a);
    a.iter().for_each(|h| println!("{h}"));
}
