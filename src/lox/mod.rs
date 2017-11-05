use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::process::exit;

mod token;
mod scanner;

pub struct Lox {
    had_errors: bool,

}

impl Lox {
    pub fn new() -> Lox {
        Lox {
            had_errors: false,
        }
    }

    pub fn run_file(&mut self, path: &str) {
        let mut file = File::open(path).expect("Could not open source file");
        let mut contents = Vec::new();
        file.read_to_end(&mut contents).expect("Could not read source file content");
        self.run(&contents[..]);

        if self.had_errors {
            exit(65);
        }
    }

    pub fn run_prompt(&mut self) {
        let mut line = String::new();
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            match io::stdin().read_line(&mut line) {
                Ok(_) => self.run(line.as_bytes()),
                Err(_) => println!("Could not read input!")
            };
            line.clear();
            self.had_errors = false;
        }
    }

    pub fn run(&mut self, source: &[u8]) {
        let tokens = scanner::scan_tokens(source, self);
        for token in tokens {
            println!("{}", token);
        }
    }

    pub fn error(&mut self, line: u32, msg: &str) {
        self.report(line, "", msg);
    }

    fn report(&mut self, line: u32, place: &str, msg: &str) {
        println!("[line {}] Error {}: {}", line, place, msg);
        self.had_errors = true;
    }
}