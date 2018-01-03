#[macro_use] extern crate failure;

mod lox;
use lox::Lox;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut lox = Lox::new();
    if args.len() > 2 {
        println!("Usage: yarli [script]");
        println!();
        println!("If script path is provided, runs the script,");
        println!("otherwise runs REPL.");
    } else if args.len() == 2 {
        lox.run_file(&args[1]);
    } else {
        lox.run_prompt();
    }
}
