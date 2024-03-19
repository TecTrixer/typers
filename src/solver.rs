use crate::rules::{RuleExpr, RuleInfo};
use std::collections::{HashMap, HashSet, VecDeque};
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

    check_cycles(&rules);
    println!("No cycle found in constraints ... \n");

    println!("Substituting constraints ...\n");
    let final_rule = substitute_constraints(&mut rules, goal_var);

    println!("All details have been increased, this is the most general type:\n");
    print_result(&final_rule);
}

/// Checks if the rules contain any cycle, assumes that all left hand sides are unique, uses topological sorting
fn check_cycles(rules: &Vec<RuleExpr>) {
    // Build graph
    let mut edge_list: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut in_degree: HashMap<usize, usize> = HashMap::new();
    // Build edge_list and in_degree
    for rule in rules {
        let rhs = rule.all_vars_rhs();
        in_degree.entry(rule.var).or_insert(0);
        for v in rhs.iter() {
            in_degree.entry(*v).and_modify(|x| *x += 1).or_insert(1);
        }
        edge_list.insert(rule.var, rule.all_vars_rhs());
    }

    // Enqueue all zero in_degree nodes
    let mut queue = VecDeque::new();
    for (key, val) in in_degree.iter() {
        if *val == 0 {
            queue.push_back(*key);
        }
    }

    // BFS traversal
    let mut num_visited = 0;
    while let Some(u) = queue.pop_front() {
        num_visited += 1;

        // adjust in_degree of next nodes
        if let Some(edges) = edge_list.get(&u) {
            for v in edges {
                let degree = in_degree.get_mut(v).unwrap();
                *degree -= 1;
                // if in_degree is 0 we need to enqueue the node
                if *degree == 0 {
                    queue.push_back(*v);
                }
            }
        }
    }
    if num_visited != in_degree.len() {
        eprintln!("{ERROR}: detected cycle in constraints, cannot proceed ...");
        std::process::exit(1);
    }
}

/// Print the given rules
fn print_rules(rules: &Vec<RuleExpr>) {
    for rule in rules {
        println!("{SECTION_PADDING}{START_RULE}{rule}{CLEAR}");
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
        } else {
            print!(", {START_VAR}t{var}{CLEAR}")
        }
        if idx == all_vars_len - 1 {
            print!("}}\n");
        }
    }
    println!("");
}

/// Remove all rules of the form `tX = tY` by replacing `X` with `Y` in all rules (where `X` < `Y`)
/// Assumes that the goal variable has the lowest ID, since otherwise it might replace it
fn remove_simple_rules(rules: &mut Vec<RuleExpr>) {
    let mut found_any = false;
    loop {
        let mut found = false;
        for i in 0..rules.len() {
            if let Some((mut from, mut to)) = rules[i].is_simple() {
                if from == to {
                    eprintln!("{ERROR}: recursive definition {START_RULE}t{from} = t{to}{CLEAR}!");
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
                found_any = true;
                break;
            }
        }
        if !found {
            break;
        }
    }
    if found_any {
        println!("");
    }
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
                        "{SECTION_PADDING}Comparing these rules\n{SECTION_PADDING}{SECTION_PADDING}{START_RULE}{}{CLEAR}\n{SECTION_PADDING}{SECTION_PADDING}{START_RULE}{}{CLEAR}\n{SECTION_PADDING}These new rules have been found:",
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
        if count > 300 {
            eprintln!(
                "\n{ERROR}: either your constraint system is very complicated or there was an undetected cycle in the constraints"
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
