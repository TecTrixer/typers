use std::{collections::HashSet, fmt::Display};

fn main() {
    let mut rules = vec![];

    // TODO: write a proper parser for the rules

    // // t0 = t1 -> t2
    // rules.push(rule!(0, fun!(var!(1), var!(2))));
    // // t2 = t3 -> t4
    // rules.push(rule!(2, fun!(var!(3), var!(4))));
    // // t4 = (t5, t6)
    // rules.push(rule!(4, tup!(var!(5), var!(6))));
    // // t7 = Int
    // rules.push(rule!(7, int!()));
    // // t6 = Bool
    // rules.push(rule!(6, boolean!()));
    // // t1 = t7 -> t5
    // rules.push(rule!(1, fun!(var!(7), var!(5))));
    // // t1 = t8 -> Int
    // rules.push(rule!(1, fun!(var!(8), int!())));
    // // t3 = t8
    // rules.push(rule!(3, var!(8)));
    // // goal is t0
    // let goal_var = 0;

    // // t2 = t7 -> Int
    // rules.push(rule!(2, fun!(var!(7), int!())));
    // // t4 = t7
    // rules.push(rule!(4, var!(7)));
    // // t4 = (Int, t6)
    // rules.push(rule!(4, fun!(int!(), var!(6))));
    // // t5 = Int
    // rules.push(rule!(5, int!()));
    // // t3 = t4 -> t5
    // rules.push(rule!(3, fun!(var!(4), var!(5))));
    // // t1 = t2 -> t3
    // rules.push(rule!(1, fun!(var!(2), var!(3))));
    // // goal is t1
    // let goal_var = 1;

    // // t0 = t1 -> t2
    // rules.push(rule!(0, fun!(var!(1), var!(2))));
    // // t2 = Bool
    // rules.push(rule!(2, boolean!()));
    // // t1 = ((t3 -> Int) t4)
    // rules.push(rule!(1, tup!(fun!(var!(3), int!()), var!(4))));
    // // t3 = (t5, t6)
    // rules.push(rule!(3, tup!(var!(5), var!(6))));
    // // t5 = Int
    // rules.push(rule!(5, int!()));
    // // t6 = Bool
    // rules.push(rule!(6, boolean!()));
    // let goal_var = 0;

    // // t0 = t4 -> t5
    // rules.push(rule!(0, fun!(var!(4), var!(5))));
    // // t1 = t2 -> t3 -> t4 -> t5
    // rules.push(rule!(
    //     1,
    //     fun!(var!(2), fun!(var!(3), fun!(var!(4), var!(5))))
    // ));

    // t0 = t4
    rules.push(rule!(0, var!(4)));
    // t1 = t2 -> t3 -> t4
    rules.push(rule!(1, fun!(var!(2), fun!(var!(3), var!(4)))));

    // t1 = (t13 -> t14) -> (t12 -> t13) -> t12 -> t14
    rules.push(rule!(
        1,
        fun!(
            fun!(var!(13), var!(14)),
            fun!(fun!(var!(12), var!(13)), fun!(var!(12), var!(14)))
        )
    ));
    // t2 = (t7 -> t8) -> (t6 -> t7) -> t6 -> t8
    rules.push(rule!(
        2,
        fun!(
            fun!(var!(7), var!(8)),
            fun!(fun!(var!(6), var!(7)), fun!(var!(6), var!(8)))
        )
    ));
    // t3 = (t10 -> t11) -> (t9 -> t10) -> t9 -> t11
    rules.push(rule!(
        3,
        fun!(
            fun!(var!(10), var!(11)),
            fun!(fun!(var!(9), var!(10)), fun!(var!(9), var!(11)))
        )
    ));
    let goal_var = 0;

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
    // TODO: make sure there is no infinite loop here
    while let Some(rule) = goal_rule.increase_detail(&rules) {
        strings.push((
            format!("{goal_pad} = \x1B[33;1m{}\x1B[0m", goal_rule.rhs),
            rule,
        ));
    }

    // TODO: make better pretty printing here
    let mut max = 0;
    for (s, _) in strings.iter() {
        max = max.max(s.len());
    }
    for (s, rule) in strings.into_iter() {
        println!(
            "{s}{}| substituting \x1B[31;1mt{rule}\x1B[0m",
            " ".repeat(max - s.len() + 4)
        )
    }

    println!("\nAll details have been increased, this is the most general type:\n");
    println!("\x1B[32;1m{goal_rule}\x1B[0m");
}

trait RuleInfo {
    fn all_vars_lhs(&self) -> HashSet<usize>;
    fn all_vars_rhs(&self) -> HashSet<usize>;
}

impl RuleInfo for Vec<Rule> {
    fn all_vars_lhs(&self) -> HashSet<usize> {
        let mut res = HashSet::new();
        for rule in self {
            res.extend(&rule.all_vars_lhs());
        }
        res
    }
    fn all_vars_rhs(&self) -> HashSet<usize> {
        let mut res = HashSet::new();
        for rule in self {
            res.extend(&rule.all_vars_rhs());
        }
        res
    }
}

impl Type {
    fn is_complex(&self) -> bool {
        match &self {
            Type::Function(_, _) => true,
            Type::Tuple(_, _) => false,
            Type::Var(_) => false,
            Type::Bool => false,
            Type::Int => false,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Type {
    Function(Box<Type>, Box<Type>),
    Tuple(Box<Type>, Box<Type>),
    Var(usize),
    Bool,
    Int,
}
// return true if detail of type could be increased with the given rules
fn increase_detail(first: Box<Type>, rules: &Vec<Rule>) -> (Option<usize>, Box<Type>) {
    let mut cpy = first.clone();
    (
        match *first {
            Type::Function(left, right) => {
                let (res, left) = increase_detail(left, rules);
                if let Some(rule) = res {
                    return (Some(rule), fun!(left, right));
                }
                let (res, right) = increase_detail(right, rules);
                if let Some(rule) = res {
                    return (Some(rule), fun!(left, right));
                }
                None
            }
            Type::Tuple(left, right) => {
                let (res, left) = increase_detail(left, rules);
                if let Some(rule) = res {
                    return (Some(rule), tup!(left, right));
                }
                let (res, right) = increase_detail(right, rules);
                if let Some(rule) = res {
                    return (Some(rule), tup!(left, right));
                }
                None
            }
            Type::Var(x) => {
                if let Some(rule) = rules.iter().find(|r| r.var == x) {
                    cpy = rule.rhs.clone();
                    Some(x)
                } else {
                    None
                }
            }
            Type::Bool => None,
            Type::Int => None,
        },
        cpy,
    )
}

impl Type {
    // return all vars contained in the right side
    fn all_vars(&self) -> HashSet<usize> {
        let mut res = HashSet::new();
        match self {
            Type::Function(left, right) => {
                res.extend(&left.all_vars());
                res.extend(&right.all_vars());
            }
            Type::Tuple(left, right) => {
                res.extend(&left.all_vars());
                res.extend(&right.all_vars());
            }
            Type::Var(x) => {
                res.insert(*x);
            }
            Type::Int => (),
            Type::Bool => (),
        }
        res
    }
    // Take in two types and then return
    fn compare_types(&self, other: &Type) -> Result<Vec<Rule>, ()> {
        match &self {
            Type::Function(sleft, sright) => match other {
                Type::Function(oleft, oright) => {
                    let mut rules = sleft.compare_types(oleft)?;
                    rules.append(&mut sright.compare_types(oright)?);
                    Ok(rules)
                }
                Type::Var(x) => Ok(vec![rule!(*x, Box::new(self.clone()))]),
                _ => Err(()),
            },
            Type::Tuple(sleft, sright) => match other {
                Type::Tuple(oleft, oright) => {
                    let mut rules = sleft.compare_types(oleft)?;
                    rules.append(&mut sright.compare_types(oright)?);
                    Ok(rules)
                }
                Type::Var(x) => Ok(vec![rule!(*x, Box::new(self.clone()))]),
                _ => Err(()),
            },
            Type::Var(x) => match other {
                Type::Var(c) => {
                    // we only want rules where left type id is smaller than right type id
                    if x < c {
                        Ok(vec![rule!(*x, var!(*c))])
                    } else if c < x {
                        Ok(vec![rule!(*c, var!(*x))])
                    } else {
                        // c == x -> no rule needs to be inserted
                        Ok(vec![])
                    }
                }
                _ => Ok(vec![rule!(*x, Box::new(other.clone()))]),
            },
            Type::Bool => match other {
                Type::Var(x) => Ok(vec![rule!(*x, Box::new(self.clone()))]),
                Type::Bool => Ok(vec![]),
                _ => Err(()),
            },
            Type::Int => match other {
                Type::Var(x) => Ok(vec![rule!(*x, Box::new(self.clone()))]),
                Type::Int => Ok(vec![]),
                _ => Err(()),
            },
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Type::Function(t1, t2) => {
                let t1_out = if t1.is_complex() {
                    format!("({t1})")
                } else {
                    format!("{t1}")
                };
                let t2_out = if t2.is_complex() {
                    format!("({t2})")
                } else {
                    format!("{t2}")
                };
                write!(f, "{t1_out} -> {t2_out}")
            }
            Type::Tuple(t1, t2) => {
                let t1_out = if t1.is_complex() {
                    format!("({t1})")
                } else {
                    format!("{t1}")
                };
                let t2_out = if t2.is_complex() {
                    format!("({t2})")
                } else {
                    format!("{t2}")
                };
                write!(f, "({t1_out}, {t2_out})")
            }
            Type::Var(x) => write!(f, "t{x}"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Rule {
    var: usize,
    rhs: Box<Type>,
}

impl Rule {
    fn increase_detail(&mut self, rules: &Vec<Rule>) -> Option<usize> {
        let (res, rhs) = increase_detail(self.rhs.clone(), rules);
        self.rhs = rhs;
        res
    }
}

impl RuleInfo for Rule {
    fn all_vars_lhs(&self) -> HashSet<usize> {
        HashSet::from([self.var])
    }
    fn all_vars_rhs(&self) -> HashSet<usize> {
        self.rhs.all_vars()
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{} = {}", self.var, self.rhs)
    }
}
mod helper {
    #[macro_export]
    macro_rules! fun {
        ($a:expr, $b:expr) => {
            Box::new(Type::Function($a, $b))
        };
    }
    #[macro_export]
    macro_rules! tup {
        ($a:expr, $b:expr) => {
            Box::new(Type::Tuple($a, $b))
        };
    }
    #[macro_export]
    macro_rules! var {
        ($a:expr) => {
            Box::new(Type::Var($a))
        };
    }
    #[macro_export]
    macro_rules! boolean {
        () => {
            Box::new(Type::Bool)
        };
    }
    #[macro_export]
    macro_rules! int {
        () => {
            Box::new(Type::Int)
        };
    }

    #[macro_export]
    macro_rules! rule {
        ($var:expr, $rhs:expr) => {
            Rule {
                var: $var,
                rhs: $rhs,
            }
        };
    }
}
