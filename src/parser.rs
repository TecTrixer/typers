use crate::{
    boolean, fun, int, rule,
    rules::{RuleExpr, TypeExpr},
    tup, var,
};
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

#[derive(pest_derive::Parser)]
#[grammar = "TypeConstraints.ps"]
pub struct TypeConstraintParser;

pub fn parse(input: &str, path: &str) -> Result<Vec<RuleExpr>, String> {
    match TypeConstraintParser::parse(Rule::system, input) {
        Ok(mut pairs) => {
            let mut rules = vec![];
            let outer_pair = pairs.next().unwrap();
            for pair in outer_pair.into_inner() {
                rules.push(parse_constraint(pair.into_inner()));
            }
            Ok(rules)
        }
        Err(e) => Err(format!(
            "{}",
            e.with_path(path).renamed_rules(|r| match *r {
                Rule::eoi => "EOF".to_owned(),
                Rule::generic_type => "Type".to_owned(),
                Rule::primary => "primary Expression".to_owned(),
                Rule::atom => "Atom".to_owned(),
                Rule::bin_op => "binary Operator".to_owned(),
                Rule::app => "->".to_owned(),
                Rule::expr => "Expression".to_owned(),
                Rule::constraint => "Constraint".to_owned(),
                Rule::system => "Constraint System".to_owned(),
                Rule::WHITESPACE => "Whitespace".to_owned(),
                Rule::tuple => "Tuple".to_owned(),
                Rule::var => "Variable".to_owned(),
                Rule::int => "Int".to_owned(),
                Rule::bool => "Bool".to_owned(),
                Rule::COMMENT => "Comment".to_owned(),
            })
        )),
    }
}

fn parse_constraint(mut pairs: Pairs<Rule>) -> RuleExpr {
    // pair is of type Rule::var
    let pair = pairs.next().unwrap();
    let var = parse_var(pair);
    let res = rule!(var, parse_expr(pairs.next().unwrap().into_inner()));
    res
}

fn parse_var(pair: Pair<Rule>) -> usize {
    let num = pair.as_str()[1..].parse::<usize>().unwrap();
    num
}

fn parse_expr(mut pairs: Pairs<Rule>) -> Box<TypeExpr> {
    let first_atom = parse_atom(pairs.next().unwrap());
    if let None = pairs.next() {
        return first_atom;
    }
    let second_atom = parse_expr(pairs);
    fun!(first_atom, second_atom)
}

fn parse_atom(pair: Pair<Rule>) -> Box<TypeExpr> {
    match pair.as_rule() {
        Rule::generic_type => parse_generic_type(pair.into_inner()),
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::tuple => parse_tuple(pair.into_inner()),
        _ => unreachable!(),
    }
}

fn parse_generic_type(mut pairs: Pairs<Rule>) -> Box<TypeExpr> {
    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::int => int!(),
        Rule::bool => boolean!(),
        Rule::var => var!(parse_var(pair)),
        _ => unreachable!(),
    }
}

fn parse_tuple(mut pairs: Pairs<Rule>) -> Box<TypeExpr> {
    let first = parse_expr(pairs.next().unwrap().into_inner());
    let second = parse_expr(pairs.next().unwrap().into_inner());
    tup!(first, second)
}
