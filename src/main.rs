use pl::compile_default;

fn main() {
    // println!("{:?}", compile_default("(x: Nat) => x"));
    println!("{:?}", compile_default("(x: Nat) => (x, \"hi\")")); 
}