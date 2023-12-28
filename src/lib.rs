use std::cmp::Ord;

/// An expression capable of modifying itself if given an operator and an argument.
/// There may be multiple implementations for a single expression type with distinct operator types.
/// This is why operator type is specified by generic argument as opposed to an associated type.
pub trait Expression<Op> {
    fn reduce(&mut self, op: Op, arg: Self);
}

/// An operator capable of reporting its precedence.
pub trait Operator {
    type Precedence;
    
    fn precedence(&self) -> Self::Precedence;
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
    /// Assumes left associativity when precedence is equal.
    pub fn push(&mut self, operator: Op, expr: Expr) {
        let precedence = operator.precedence();
        while let Some((last_op, _)) = self.stack.last() {
            if last_op.precedence() >= precedence {
                self.reduce();
            } else {
                break;
            }
        }
        self.stack.push((operator, expr));
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

