use crate::{
    boolean, fun, int, rule,
    rules::{RuleExpr, TypeExpr},
    tup, var,
};
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

/// Parsing type constraints using Pest with the grammar defined in `TypeConstraints.ps`
#[derive(pest_derive::Parser)]
#[grammar = "TypeConstraints.ps"]
pub struct TypeConstraintParser;

// TODO: check if every use of unwrap() is actually valid
impl TypeConstraintParser {
    /// Parse the given input and return a list of all found rules, if an error during parsing is encountered a meaningful error string will get created
    pub fn get_constraints(input: &str, path: &str) -> Result<Vec<RuleExpr>, String> {
        match Self::parse(Rule::system, input) {
            Ok(mut pairs) => Ok(Self::parse_system(pairs.next().unwrap())),
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
    /// Parse a system, assumes that the outer_pair is of type Rule::system
    fn parse_system(outer_pair: Pair<Rule>) -> Vec<RuleExpr> {
        let mut rules = vec![];
        for pair in outer_pair.into_inner() {
            rules.push(Self::parse_constraint(pair.into_inner()));
        }
        rules
    }

    /// Parse a constraint, assumes that pairs is an iterator with exactly one var and one expr
    fn parse_constraint(mut pairs: Pairs<Rule>) -> RuleExpr {
        // pair is of type Rule::var
        let pair = pairs.next().unwrap();
        let var = Self::parse_var(pair);
        let res = rule!(var, Self::parse_expr(pairs.next().unwrap().into_inner()));
        res
    }

    /// Parse a variable, assumes that the pair is of type var
    fn parse_var(pair: Pair<Rule>) -> usize {
        let num = pair.as_str()[1..].parse::<usize>().unwrap();
        num
    }

    /// Parse an expression, assumes that pairs is an iterator over at least one atom and then arbitrary many pairs of `->` and atoms
    fn parse_expr(mut pairs: Pairs<Rule>) -> Box<TypeExpr> {
        let first_atom = Self::parse_atom(pairs.next().unwrap());
        // Check if there is a next `->` sign
        if let None = pairs.next() {
            return first_atom;
        }
        let second_atom = Self::parse_expr(pairs);
        fun!(first_atom, second_atom)
    }

    /// Parse an atom, assumes that the pair is of type generic_type, tuple or expr
    fn parse_atom(pair: Pair<Rule>) -> Box<TypeExpr> {
        match pair.as_rule() {
            Rule::generic_type => Self::parse_generic_type(pair.into_inner()),
            Rule::expr => Self::parse_expr(pair.into_inner()),
            Rule::tuple => Self::parse_tuple(pair.into_inner()),
            _ => unreachable!(),
        }
    }

    /// Parse a generic_type, assumes that pairs is an iterator over exactly one of int, bool and var
    fn parse_generic_type(mut pairs: Pairs<Rule>) -> Box<TypeExpr> {
        let pair = pairs.next().unwrap();
        match pair.as_rule() {
            Rule::int => int!(),
            Rule::bool => boolean!(),
            Rule::var => var!(Self::parse_var(pair)),
            _ => unreachable!(),
        }
    }

    /// Parses a tuple, assumes that pairs is an iterator over exactly two expressions
    fn parse_tuple(mut pairs: Pairs<Rule>) -> Box<TypeExpr> {
        let first = Self::parse_expr(pairs.next().unwrap().into_inner());
        let second = Self::parse_expr(pairs.next().unwrap().into_inner());
        tup!(first, second)
    }
}
