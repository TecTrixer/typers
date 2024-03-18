use std::collections::HashSet;
use std::io::Read;

use parser::TypeConstraintParser;
use rules::{RuleExpr, RuleInfo};
mod macros;
mod parser;
mod rules;

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

fn solve_constraints(mut rules: Vec<RuleExpr>, goal_var: usize) {
    for (i, rule) in rules.iter().enumerate() {
        println!("Rule #{}: \x1B[35;1m{rule}\x1B[0m", i + 1);
    }
    let all_vars: HashSet<usize> = rules
        .all_vars_lhs()
        .union(&rules.all_vars_rhs())
        .map(|x| *x)
        .collect();
    let mut all_vars_vec: Vec<usize> = all_vars.clone().into_iter().collect();
    all_vars_vec.sort();
    print!("\nFound these variables:");
    let all_vars_len = all_vars_vec.len();
    for (idx, var) in all_vars_vec.into_iter().enumerate() {
        if idx == 0 {
            print!(" {{t{var}");
        } else if idx == all_vars_len - 1 {
            print!(", t{var}}}\n");
        } else {
            print!(", t{var}")
        }
    }
    // TODO: can we just loop here?
    for _ in 0..1000 {
        // remove simple rules
        loop {
            let mut found = false;
            for i in 0..rules.len() {
                if let Some((mut from, mut to)) = rules[i].is_simple() {
                    if from == to {
                        println!("Error, recursive definition t{from} = t{to}!");
                        std::process::exit(1);
                    } else if from < to {
                        std::mem::swap(&mut to, &mut from);
                    }

                    println!("Replacing t{from} with t{to}");
                    rules.swap_remove(i);

                    for rule in rules.iter_mut() {
                        rule.replace_var(from, to)
                    }
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
        }

        let mut found_new = false;
        'outer: for i in 0..rules.len() {
            for j in i + 1..rules.len() {
                if rules[i].var != rules[j].var {
                    continue;
                }
                if let Ok(new_rules) = rules[i].rhs.compare_types(&rules[j].rhs) {
                    println!(
                        "\nComparing the rule \x1B[31;1m{}\x1B[0m and \x1B[31;1m{}\x1B[0m these new rules have been found:",
                        rules[i], rules[j]
                    );
                    rules.swap_remove(j);
                    for rule in new_rules {
                        println!("\x1B[35;1m{rule}\x1B[0m");
                        rules.push(rule);
                    }
                } else {
                    println!(
                        "Impossible to combine these rules:\n{}\n{}",
                        rules[i], rules[j]
                    );
                    std::process::exit(1);
                }
                found_new = true;
                break 'outer;
            }
        }
        if !found_new {
            break;
        }
    }

    let mut goal_rule = rules
        .iter()
        .find(|r| r.var == goal_var)
        .expect(&format!(
            "\nDid not find any rule for var \x1B[31;1mt{goal_var}\x1B[0m!\n"
        ))
        .clone();
    println!("\nIncreasing details for goal rule t{goal_var}:\n");
    let goal_str = format!("t{goal_var}");
    let goal_pad = " ".repeat(goal_str.len());
    println!("{goal_str} = \x1B[33;1m{}\x1B[0m", goal_rule.rhs);
    let mut strings = vec![];
    // TODO: make sure there is no infinite loop here, add some rule application cycle detection here, e.g. t0 = t0 is infinite cycle
    let mut count = 0;
    while let Some(rule) = goal_rule.substitute_constraint(&rules) {
        count += 1;
        strings.push((
            format!("{goal_pad} = \x1B[33;1m{}\x1B[0m", goal_rule.rhs),
            rule,
        ));
        if count > 15 {
            break;
        }
    }

    // TODO: make better pretty printing here
    let mut max = 0;
    for (s, _) in strings.iter() {
        max = max.max(s.len());
    }
    for (s, rule) in strings.into_iter() {
        println!(
            "{s}{}| substituting \x1B[31;1m{}\x1B[0m",
            " ".repeat(max - s.len() + 2),
            rules.iter().find(|x| x.var == rule).unwrap()
        )
    }

    println!("\nAll details have been increased, this is the most general type:\n");
    println!("\x1B[32;1m{goal_rule}\x1B[0m");
}
