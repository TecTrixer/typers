use std::io::Read;

use parser::TypeConstraintParser;
use solver::solve_constraints;
mod macros;
mod parser;
mod rules;
mod solver;

fn main() {
    let mut constraints = std::fs::OpenOptions::new()
        .read(true)
        .open("constraints.txt")
        .unwrap();
    let mut all_constraints = String::new();
    constraints.read_to_string(&mut all_constraints).unwrap();
    let rules = match TypeConstraintParser::get_constraints(&all_constraints, "constraints.txt") {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    };
    solve_constraints(rules, 0);
}
