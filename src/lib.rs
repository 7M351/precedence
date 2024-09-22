use std::cmp::{Ord, Ordering};

/// An expression capable of modifying itself if given an operator and an argument.
/// There may be multiple implementations for a single expression type with distinct operator types.
/// This is why operator type is specified by generic argument as opposed to an associated type.
pub trait Expression<Op> {
    fn reduce(&mut self, op: Op, arg: Self);

    type Info;
    fn info(&self) -> Self::Info;
}

#[derive(Copy, Clone, Debug)]
pub struct AssociativityError<I> {
    pub left_info: I,
    pub right_info: I,
}

/// An operator capable of reporting its precedence and associativity.
pub trait Operator {
    type Precedence;
    type Info;
    
    fn precedence(&self) -> Self::Precedence;
    fn associativity(&self) -> Associativity;
    fn info(&self) -> Self::Info;
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
{
    pub fn new(expr: Expr) -> Self {
        Self {
            expr,
            stack: Vec::new(),
        }
    }
    /// Pushes an operator-expression pair to the end of the list, reducing expressions according to the rules of precedence.
    pub fn push(&mut self, operator: Op, expr: Expr) -> Result<(), AssociativityError<Op::Info>> {
        let precedence = operator.precedence();
        while let Some((last_op, _last_expr)) = self.stack.last() {
            let last_precedence = last_op.precedence();
            match last_precedence.cmp(&precedence) {
                Ordering::Greater => self.reduce(),
                Ordering::Equal => {
                    use Associativity::*;
                    let assoc = operator.associativity();
                    let last_assoc = last_op.associativity();
                    match last_assoc.combine(assoc) {
                        Left => self.reduce(),
                        Right => break,
                        Neither => {
                            let err = AssociativityError {
                                left_info: last_op.info(),
                                right_info: operator.info(),
                            };
                            return Err(err);
                        },
                    }
                },
                Ordering::Less => break,
            }
        }
        self.stack.push((operator, expr));
        Ok(())
    }
    
    fn reduce(&mut self) {
        if let Some((op, expr2)) = self.stack.pop() {
            let expr1_mut = self.stack.last_mut()
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
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Associativity {
    Neither,
    Left,
    Right,
}

impl Associativity {
    pub fn mirror(self) -> Self {
        use Associativity::*;
        match self {
            Neither => Neither,
            Left => Right,
            Right => Left,
        }
    }

    fn combine(self, other: Self) -> Self {
        if self == other {
            self
        } else {
            Associativity::Neither
        }
    }
}

