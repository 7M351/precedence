This library implements a stack useful for parsing expressions containing
binary operators with given precedences and associativities.

The type `ExprStack` implements the stack.
To use it, implement trait `Operator` for your type.

The implementation detects cases of incompatible associativity of adjacent
operators of equal precedence and reports them as errors (currently
irrecoverable). Some representations of associativity (e.g. "always left"
(type `LeftAssociativity`)) never produce such errors.
