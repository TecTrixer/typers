use crate::rules::{RuleExpr, RuleInfo};
use std::collections::HashSet;
const START_RULE: &str = "\x1B[35;1m"; // ANSI sequence for purple and bold
const START_VAR: &str = "\x1B[31;1m"; // ANSI sequence for red and bold
const ERROR: &str = "\x1B[31;1m[ERROR]\x1B[0m"; // ANSI sequence for "[ERROR]" in red and bold
const CLEAR: &str = "\x1B[0m"; // ANSI sequence for plain style
const SECTION_PADDING: &str = "   "; // padding to differentiate sections from headings

/// Solve the constraints by first accumulating them and then substituting
pub fn solve_constraints(mut rules: Vec<RuleExpr>, goal_var: usize) {
    println!("Found these rules ...\n");
    print_rules(&rules);

    println!("Found these variables ...\n");
    print_variables(&rules);

    println!("Accumulating constraints ...\n");
    accumulate_constraints(&mut rules);

    // TODO: make sure there is no infinite loop here, add some rule application cycle detection here, e.g. t0 = t0 is infinite cycle

    println!("Substituting constraints ...\n");
    let final_rule = substitute_constraints(&mut rules, goal_var);

    println!("All details have been increased, this is the most general type:\n");
    print_result(&final_rule);
}

/// Print the given rules
fn print_rules(rules: &Vec<RuleExpr>) {
    for (i, rule) in rules.iter().enumerate() {
        println!(
            "{SECTION_PADDING}Rule #{}: {START_RULE}{rule}{CLEAR}",
            i + 1
        );
    }
    println!("");
}

/// Find and print all variables
fn print_variables(rules: &Vec<RuleExpr>) {
    let all_vars: HashSet<usize> = rules
        .all_vars_lhs()
        .union(&rules.all_vars_rhs())
        .map(|x| *x)
        .collect();
    let mut all_vars_vec: Vec<usize> = all_vars.clone().into_iter().collect();
    all_vars_vec.sort();
    let all_vars_len = all_vars_vec.len();
    for (idx, var) in all_vars_vec.into_iter().enumerate() {
        if idx == 0 {
            print!("{SECTION_PADDING}{{{START_VAR}t{var}{CLEAR}");
        } else if idx == all_vars_len - 1 {
            print!(", {START_VAR}t{var}{CLEAR}}}\n");
        } else {
            print!(", {START_VAR}t{var}{CLEAR}")
        }
    }
    println!("");
}

/// Remove all rules of the form `tX = tY` by replacing `X` with `Y` in all rules (where `X` < `Y`)
/// Assumes that the goal variable has the lowest ID, since otherwise it might replace it
fn remove_simple_rules(rules: &mut Vec<RuleExpr>) {
    loop {
        let mut found = false;
        for i in 0..rules.len() {
            if let Some((mut from, mut to)) = rules[i].is_simple() {
                if from == to {
                    eprintln!("{ERROR}: recursive definition t{from} = t{to}!");
                    std::process::exit(1);
                } else if from < to {
                    std::mem::swap(&mut to, &mut from);
                }

                println!("{SECTION_PADDING}Replacing {START_VAR}t{from}{CLEAR} with {START_VAR}t{to}{CLEAR} ...");
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
    println!("");
}

/// Accumulate constraints by comparing rules with the same left hand side and replacing variables which are equal
fn accumulate_constraints(rules: &mut Vec<RuleExpr>) {
    // Compare rules greedily to get new constraints
    // TODO: can we just loop here?
    for _ in 0..1000 {
        remove_simple_rules(rules);
        // Iterate over rules to find two matching ones
        let mut found_new = false;
        'outer: for i in 0..rules.len() {
            for j in i + 1..rules.len() {
                if !rules[i].has_same_lhs(&rules[j]) {
                    continue;
                }
                // Get all new constraints by comparing the rules
                if let Ok(new_rules) = rules[i].compare_rules(&rules[j]) {
                    println!(
                        "{SECTION_PADDING}By comparing {START_RULE}{}{CLEAR} and {START_RULE}{}{CLEAR}, these new rules have been found:\n",
                        rules[i], rules[j]
                    );
                    rules.swap_remove(j);
                    for rule in new_rules {
                        println!("{SECTION_PADDING}{SECTION_PADDING}{START_RULE}{rule}{CLEAR}");
                        rules.push(rule);
                    }
                    println!("");
                } else {
                    eprintln!(
                        "{ERROR}: impossible to combine these rules:\n{}\n{}",
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
}

/// Find rule for goal variable
fn find_goal_rule(rules: &mut Vec<RuleExpr>, goal_var: usize) -> RuleExpr {
    match rules.iter().find(|r| r.has_lhs(goal_var)) {
        Some(ref goal_rule) => (*goal_rule).clone(),
        None => {
            eprintln!("{ERROR}: could not find a constraint with {START_VAR}t{goal_var}{CLEAR} on the left hand side");
            std::process::exit(1);
        }
    }
}

/// Substitute constraints, assumes that there is no cycle in the rules
fn substitute_constraints(rules: &mut Vec<RuleExpr>, goal_var: usize) -> RuleExpr {
    let mut goal_rule = find_goal_rule(rules, goal_var);
    let goal_str = format!("t{goal_var}");
    let goal_pad = " ".repeat(goal_str.len());
    println!(
        "{SECTION_PADDING}{goal_str} = \x1B[33;1m{}\x1B[0m",
        goal_rule.rhs
    );
    let mut strings = vec![];
    let mut count = 0;
    // Substitute constraints one by one
    while let Some(rule) = goal_rule.substitute_constraint(&rules) {
        count += 1;
        strings.push((
            format!("{goal_pad} = \x1B[33;1m{}\x1B[0m", goal_rule.rhs),
            rule,
        ));
        // catch infinite cycles, should have already been avoided though!
        if count > 30 {
            eprintln!(
                "{ERROR}: you probably have an infinite cycle or your rule is really complicated"
            );
            std::process::exit(1);
        }
    }

    let mut max = 0;
    for (s, _) in strings.iter() {
        max = max.max(s.len());
    }
    for (s, rule) in strings.into_iter() {
        println!(
            "{SECTION_PADDING}{s}{}| substituting {START_RULE}{}{CLEAR}",
            " ".repeat(max - s.len() + 2),
            rules.iter().find(|x| x.has_lhs(rule)).unwrap()
        )
    }
    println!("");
    goal_rule
}

/// Print the result of the substitution
fn print_result(rule: &RuleExpr) {
    println!("{SECTION_PADDING}\x1B[32;1m{rule}\x1B[0m");
}
