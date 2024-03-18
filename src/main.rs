use std::io::Read;

use parser::TypeConstraintParser;
use solver::solve_constraints;
mod macros;
mod parser;
mod rules;
mod solver;

use clap::Parser;

const ERROR: &str = "\x1B[31;1m[ERROR]\x1B[0m"; // ANSI sequence for "[ERROR]" in red and bold

#[derive(Parser)]
#[command(
    version,
    about = "A type constraint solver for mini-haskell types.\n\nExpected format:\nt0 = (t1, t2) -> t3\nt1 = t4 -> Bool\nt3 = Int"
)]
struct Cli {
    /// constraint file, if no file is provided the program will read from stdin
    file: Option<String>,
    #[arg(short, long, default_value = "0", name = "goal variable")]
    // the ID of the variable for which should be solved, by default the program tries to solve for t0
    goal_var: usize,
}

fn main() {
    let cli = Cli::parse();
    let (content, name) = if let Some(path) = cli.file {
        (read_file(&path), path)
    } else {
        (read_stdin(), "stdin".to_owned())
    };
    let rules = match TypeConstraintParser::get_constraints(&content, &name) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    };
    solve_constraints(rules, cli.goal_var);
}

/// Read everything from standard input
fn read_stdin() -> String {
    let mut sin = std::io::stdin().lock();
    let mut content = String::new();
    if let Err(e) = sin.read_to_string(&mut content) {
        eprintln!("{ERROR}: stdin contains invalid data: {e}");
        std::process::exit(1);
    }
    content
}

/// Read file, if it exists and contains valid utf-8 encoded data
fn read_file(path: &String) -> String {
    if let Ok(mut file) = std::fs::OpenOptions::new().read(true).open(path) {
        let mut content = String::new();
        if let Err(e) = file.read_to_string(&mut content) {
            eprintln!("{ERROR}: file {path} contains invalid data: {e}");
            std::process::exit(1);
        }
        content
    } else {
        eprintln!("{ERROR}: could not open file {path}");
        std::process::exit(1);
    }
}
