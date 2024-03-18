/// Creates the variant TypeExpr::Function, requires two `TypeExpr` expressions
#[macro_export]
macro_rules! fun {
    ($a:expr, $b:expr) => {
        Box::new(crate::rules::TypeExpr::Function($a, $b))
    };
}
/// Creates the variant TypeExpr::Tuple, requires two `TypeExpr` expressions
#[macro_export]
macro_rules! tup {
    ($a:expr, $b:expr) => {
        Box::new(crate::rules::TypeExpr::Tuple($a, $b))
    };
}
/// Creates the variant TypeExpr::Var, requires one `usize` expressions
#[macro_export]
macro_rules! var {
    ($a:expr) => {
        Box::new(crate::rules::TypeExpr::Var($a))
    };
}
/// Creates the variant TypeExpr::Bool
#[macro_export]
macro_rules! boolean {
    () => {
        Box::new(crate::rules::TypeExpr::Bool)
    };
}
/// Creates the variant TypeExpr::Bool
#[macro_export]
macro_rules! int {
    () => {
        Box::new(crate::rules::TypeExpr::Int)
    };
}

/// Creates a new RuleExpr, requires one `usize` expression and one `TypeExpr` expression
#[macro_export]
macro_rules! rule {
    ($var:expr, $rhs:expr) => {
        RuleExpr {
            var: $var,
            rhs: $rhs,
        }
    };
}
