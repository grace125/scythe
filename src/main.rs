use pl::compile_default;

fn main() {

    println!("{:?}", compile_default("
        let (x: Str): Nat := 3;
        x
    ")); 
    
    // println!("{:?}", compile_default("
    //     ((x: Nat) => x) 5
    // ")); 

    // println!("{:?}", compile_default("
    //     let id := (T: Type) => (x: T) => x;
    //     (id Nat) 5
    // ")); 



    // // println!("{:?}", compile_default("(x: Nat) => x"));
    // println!("{:?}", compile_default("((x: Nat) => `_ + _`(x, 3) / 3 + 5)(21)")); 
    // println!("{:?}", compile_default("2919 % 5 + 20 - 10"));  // (x: Nat) => x
    // println!("{:?}", compile_default("1 + 2 * 3 + 4"));  // (x: Nat) => x

    // println!("{:?}", compile_default(" 
    //     let x := 1 + 1;
    //     let y := (x + 3) - 2;
    //     y + 1
    // "));

    // println!("{:?}", compile_default(" 
    //     let x := 1 + 1;
    //     let y := (x + 3) - 2;
    //     y + 1
    // "));

    // println!("{:?}", compile_default(" 
    //     let x := 5;
    //     let f := (n: Nat) => n + x;
    //     f(3)
    // "));

    // println!("{:?}", compile_default(" 
    //     let g := (n: Nat) => {
    //         let a := n + 2;
    //         a + n
    //     };
    //     g(3)
    // "));

    // println!("{:?}", compile_default(" 
    //     let x := 1;
    //     let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := { let x := x; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x }; x
    // "));

    // println!("{:?}", compile_default(" 
    //     (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => 
    //     (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => 
    //     (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => (x: Nat) => x
    // "));

    // println!("{:?}", compile_default(" 
    //     let x := 1;
    //     let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; let x := x;
    //     let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; let x := x;
    //     let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; let x := x; 
    //     x
    // "));

    

    // // println!("{:?}", compile_default(" 
    // //     rec f: Nat -> Nat := x => f(x - 1)
    // //     f(5)
    // // "));
}