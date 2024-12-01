use std::cmp::{Ord, Ordering};

/// An expression capable of modifying itself if given an operator and an argument.
/// There may be multiple implementations for a single expression type with distinct operator types.
/// This is why operator type is specified by generic argument as opposed to an associated type.
pub trait Expression<Op> {
    fn reduce(&mut self, op: Op, arg: Self);
}

/// An operator capable of reporting its precedence and associativity.
pub trait Operator {
    type Precedence;
    type Associativity;

    fn precedence(&self) -> Self::Precedence;
    fn associativity(&self) -> Self::Associativity;
}

/// A list of N+1 expressions with N binary operators in between them.
pub struct ExprStack<Expr, Op> {
    expr: Expr,
    stack: Vec<(Op, Expr)>,
}

impl<Expr, Op> ExprStack<Expr, Op>
where
    Expr: Expression<Op>,
    Op: Operator,
    <Op as Operator>::Precedence: Ord,
    <Op as Operator>::Associativity: AssociativityRepr,
{
    pub fn new(expr: Expr) -> Self {
        Self {
            expr,
            stack: Vec::new(),
        }
    }
    // TODO: extract this code to a helper method
    /// Pushes an operator-expression pair to the end of the list,
    /// reducing expressions according to the rules of precedence.
    pub fn push(
        mut self,
        operator: Op,
        expr: Expr,
    ) -> Result<Self, <Op::Associativity as AssociativityRepr>::Error> {
        let precedence = operator.precedence();
        while let Some((last_op, _last_expr)) = self.stack.last() {
            let last_precedence = last_op.precedence();

            let associativity = last_op.associativity().associate(operator.associativity());

            match (last_precedence.cmp(&precedence), associativity) {
                (Ordering::Greater, _) | (Ordering::Equal, Ok(Associativity::Left)) => {
                    self.reduce()
                }
                // TODO: better errors with custom info from expressions and operators
                (Ordering::Equal, Err(e)) => return Err(e),
                (Ordering::Equal, Ok(Associativity::Right)) | (Ordering::Less, _) => break,
            }
        }
        self.stack.push((operator, expr));
        Ok(self)
    }

    fn reduce(&mut self) {
        if let Some((op, expr2)) = self.stack.pop() {
            let expr1_mut = self
                .stack
                .last_mut()
                .map_or(&mut self.expr, |pair| &mut pair.1);

            expr1_mut.reduce(op, expr2);
        }
    }
    /// Reduces all remaining operations and returns the single result.
    pub fn finish(mut self) -> Expr {
        while !self.stack.is_empty() {
            self.reduce();
        }

        self.expr
    }
}

impl<Expr, Op> ExprStack<Expr, Op>
where
    Expr: Expression<Op>,
    Op: Operator,
    Op::Associativity: AssociativityRepr<Error = NoError>,
    <Op as Operator>::Precedence: Ord,
{ // TODO: convenience method for infallible precedence
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl Associativity {
    pub fn mirror(self) -> Self {
        use Associativity::*;
        match self {
            Left => Right,
            Right => Left,
        }
    }
}
/// Trait representing the associativity of an operator type.
///
/// Implementing types `LeftAssociativity` and `RightAssociativity`
/// can be used for sets of operators
/// wherein all operators associate the same way.
/// They produce no associativity errors.
///
/// Implementing type `Option<Associativity>` uses the `None` variant
/// to encode operators which do not associate by intention.
pub trait AssociativityRepr { // TODO: same associativity at a given level of precedence (infallible)
    type Error;

    fn associate(self, next: Self) -> Result<Associativity, Self::Error>;
}

#[derive(Copy, Clone, Debug)]
pub struct AssociativityError; // TODO: add some information


impl AssociativityRepr for Associativity {
    type Error = AssociativityError;

    fn associate(self, next: Self) -> Result<Associativity, Self::Error> {
        if self == next {
            Ok(self)
        } else {
            Err(AssociativityError)
        }
    }
}

impl AssociativityRepr for Option<Associativity> {
    type Error = AssociativityError;

    fn associate(self, next: Self) -> Result<Associativity, Self::Error> {
        if let (Some(this), Some(next)) = (self, next) {
            this.associate(next)
        } else {
            Err(AssociativityError)
        }
    }
}

pub struct LeftAssociativity;
pub struct RightAssociativity;
pub enum NoError {}

impl AssociativityRepr for LeftAssociativity {
    type Error = NoError;

    fn associate(self, _next: Self) -> Result<Associativity, Self::Error> {
        Ok(Associativity::Left)
    }
}

impl AssociativityRepr for RightAssociativity {
    type Error = NoError;

    fn associate(self, _next: Self) -> Result<Associativity, Self::Error> {
        Ok(Associativity::Right)
    }
}

#[cfg(test)]
mod test {
    struct Plus;
    impl crate::Operator for Plus {
        type Precedence = ();
        type Associativity = crate::LeftAssociativity;

        fn precedence(&self) -> Self::Precedence {}
        fn associativity(&self) -> Self::Associativity {
            crate::LeftAssociativity
        }
    }
    struct Value(i32);
    impl crate::Expression<Plus> for Value {
        fn reduce(&mut self, Plus: Plus, arg: Self) {
            self.0 += arg.0;
        }
    }
    #[test]
    fn infallible_result_match() {
        let expr_stack = crate::ExprStack::new(Value(15));
        let Ok(expr_stack) = expr_stack.push(Plus, Value(13));
        let Ok(expr_stack) = expr_stack.push(Plus, Value(11));
        let result = expr_stack.finish().0;
        assert_eq!(result, 39);
    }
}
