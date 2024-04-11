use pl::compile_default;

fn main() {
    // println!("{:?}", compile_default("(x: Nat) => x"));
    println!("{:?}", compile_default("((x: Nat) => `_ + _`(x, 3) / 3 + 5)(21)")); 
    println!("{:?}", compile_default("2919 % 5 + 20 - 10"));  // (x: Nat) => x
    println!("{:?}", compile_default("1 + 2 * 3 + 4"));  // (x: Nat) => x
}