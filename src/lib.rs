use std::cmp::{Ord, Ordering};

/// A binary operator capable of reporting its precedence and associativity.
///
/// Core trait of this library.
pub trait Operator<Expr> {
    type Precedence;
    type Associativity;

    fn apply(self, expr1: &mut Expr, expr2: Expr);
    fn precedence(&self) -> Self::Precedence;
    fn associativity(&self) -> Self::Associativity;
}

/// A list of N+1 expressions with N binary operators in between them.
#[derive(Clone, Debug)]
pub struct ExprStack<Expr, Op> {
    expr: Expr,
    stack: Vec<(Op, Expr)>,
}

impl<Expr, Op> ExprStack<Expr, Op>
where
    Op: Operator<Expr>,
    <Op as Operator<Expr>>::Precedence: Ord,
    <Op as Operator<Expr>>::Associativity: AssociativityRepr,
{
    pub fn new(expr: Expr) -> Self {
        Self {
            expr,
            stack: Vec::new(),
        }
    }
    /// Pushes an operator-expression pair to the end of the list,
    /// reducing expressions according to the rules of precedence.
    pub fn try_push(
        mut self,
        operator: Op,
        expr: Expr,
    ) -> Result<Self, <Op::Associativity as AssociativityRepr>::Error> {
        match self.push_inner(operator, expr) {
            Ok(()) => Ok(self),
            Err(e) => Err(e),
        }
    }
    fn push_inner(
        &mut self,
        operator: Op,
        expr: Expr,
    ) -> Result<(), <Op::Associativity as AssociativityRepr>::Error> {
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
        Ok(())
    }

    fn reduce(&mut self) {
        if let Some((op, expr2)) = self.stack.pop() {
            let expr1_mut = self
                .stack
                .last_mut()
                .map_or(&mut self.expr, |pair| &mut pair.1);

            op.apply(expr1_mut, expr2);
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
    Op: Operator<Expr>,
    Op::Associativity: AssociativityRepr<Error = NoError>,
    <Op as Operator<Expr>>::Precedence: Ord,
{
    /// Convenient alternative of `try_push` for operators which never fail to associate.
    pub fn push(&mut self, operator: Op, expr: Expr) {
        let Ok(()) = self.push_inner(operator, expr);
    }
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
pub trait AssociativityRepr {
    // TODO: same associativity at a given level of precedence (infallible)
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
    impl crate::Operator<i32> for Plus {
        type Precedence = ();
        type Associativity = crate::LeftAssociativity;

        fn apply(self, expr1: &mut i32, expr2: i32) {
            *expr1 += expr2;
        }
        fn precedence(&self) -> Self::Precedence {}
        fn associativity(&self) -> Self::Associativity {
            crate::LeftAssociativity
        }
    }
    #[test]
    fn infallible_result_match() {
        let mut expr_stack = crate::ExprStack::new(15);
        expr_stack.push(Plus, 13);
        expr_stack.push(Plus, 11);
        let result = expr_stack.finish();
        assert_eq!(result, 39);
    }
}
