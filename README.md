# My own language compiler consisting of a lexer, a recursive descent parser, a semantic analyzer, and an interpreter as well as a codegen, written in rust.

### The language can be compiled to machine code with llvm or interpreted in rust.

### Running:
`cargo run <source_file>`

### What I have in mind for the syntax:
```
func add_three(i32 x, i32 y, i32 z) => i32 {
    return x + y + z;
}

func main() => i32 {
    let output: String = "the result"; // explicit types
    let x: i32 = 21;
    let y = -69; // inferred types
    let z = 420;
    print("{} is: {}", output, add_three(x, y, z));

    // return 0; // implicit
}
```