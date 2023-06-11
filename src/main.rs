use std::io::{self, BufRead};

use monkey_interpreter::Lexer;

fn main() {
    println!("This is the Monkey programming language\n\
        Type in commands for evaluation...");

    let stdin = io::stdin();
    
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);

        for tok in lexer {
            println!("{:?}", tok);
        }
    }
}
