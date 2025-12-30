Section 1.1
=========== 

Naming
------ 

Naming provides a means of abstraction. Computational objects may be extremely complex, and it would be cumbersome to have to remember and repeat their details each time we want to use them. The ability to create name-object pairs incrementally, in successive interactions makes the construction of large programs easier and more convinient.

The possibility of associating values with symbols and later retrieving them means that the interpreter must maintain some sort of memory that keeps track of the name-object pairs. This memory is called the **environment** (more precisely, the global environment, since we will see later that a computation may involve a number of different environments). 

Evaluating Combinations
-----------------------

Lisp encourages developers to think about computation in a "procedural" way. However, consider that in evaluating combinations, the interpreter itself is following a (recursive) procedure:

1. The interpreter first evaluates the subexpressions of the combination.
2. The interpreter then applies the procedure that is the value of the left-most subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

The evaluation rule is therefore recursive, in that in order to accomplish the evaluation process for a combination, we must first perform the evaluation process for each element of the combination.

Applicative-Order Evaluation
---------------------------- 

One of the main concepts in Section 1.1 is to introduce the difference between "**applicative-order**" evaluation and "**normal-order**" evaluation.

Applicative-order evaluation (or, in practical terms, something similar to but not exactly identical to it) is the commonly used in production interpreteres, for reasons of performance and efficiency. Nevertheless, normal-order evaluation can be useful as a heuristic tool, and its various uses are explored in later chapters in this book.

The illustrate the difference between the two models, consider the following function definitions:

```scheme
(define (square x) (* x x))

(define (sum-of-squares a b) (+ (square a) (square b)))

(define (f a) (sum-of-squares (+ a 1) (* a 2)))
```

We can calculate the  value of the `(f 5)` easily enough:

```scheme
(f 5)

==> 136
```

But the question is, how does the interpreter arrive at this answer?

In **applicative-order** evaluation, we first evaluate the operator and its operands, and then apply the resulting procedure to the resulting arguments. In other words, the function arguments are fully evaluated before the function is invoked.

Consider evaluation of the combination `(f 5)`:

```scheme
(f 5)
```

We fetch the definition for the function `f`:

```scheme
(sum-of-squares (+ a 1) (* a 2))
```

and replace the formal parameter `a` with the value `5`:

```scheme
(sum-of-squares (+ 5 1) (* 5 2))
```

We are using **applicative-order** evaluation, so we fully evaluate about arguments to obtain:

```scheme
(sum-of-squares 6 10)
```

Next we fetch the definition of `sum-of-squares`:

```scheme
(+ (square a) (square b))
```

and replace the formal parameters `a` and `b` with the values `6` and `10`, respectively:

```scheme
(+ (square 6) (square 10))
```

`+` is a primitive operation, but, since this is **applicative-order**, we now need to evaluate the two arguments of `+`. To evaluate `(square 6)`, we first fetch the definition of `square`:

```scheme
(* x x)
```

and replace the formal parameter `x` with the value `6`:

```scheme
(* 6 6)
36
```

Our evaluation now reduces to:

```scheme
(+ 36 (square 10))
```

The second argument `(square 10)` reduces to `100`, through a similar process, giving:

```scheme
(+ 36 100)
136
```

Pulling it all together, and evaluating the two formal parameters of `sum-of-squares` in successive order,  **applicative-order** evaluation would look something like:

```scheme
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(sum-of-squares 6 (* 5 2))
(sum-of-squares 6 10)
(+ (square 6) (square 10))
(+ (* 6 6) (square 10))
(+ 36 (square 10))
(+ 36 (* 10 10))
(+ 36 100)
136
```

Normal-Order Evaluation
-----------------------

The evaluation model described above is not the only way to perform expression evaluation. 

An alternative evaluation model would **not** evaluate the operands until their values were actually needed. Instead, it would first substitute operand expressions for parameters until it obtained an expression involving only primitive operators, and then it would perform the evaluation.

Such a model of expression evaluation is called **normal-order** evaluation.

In the normal-order model of expression evaluation, evaluation of `(f 5)` would proceed according to the following sequence of operations:

```scheme
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1)) (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
```

Having now arrived at an expression involving only primitive operations, the evaluator would proceed to evaluate the operands:

```scheme
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136
```

It can be shown that for procedures that can be modeled using substitution and that yield legitimate values, normal-order and applicative-order evaluation produce the same value. 

Lisp uses applicative-order evaluation, partly because of the additional efficiency obtained from avoiding multiple evaluations of expressions such `(+ 5 1)` and `(* 5 2)` above, and more significantly, because normal-order evaluation becomes much more complicated to deal with when we leave the realm of procedures that can be modeled by substitution.

Newton's Method
---------------

One way to derive Newton's method for numerical approximations of square roots is to start with a Taylor expansion:

$f(x) \approx f(a) + f'(a)(x-a) + \frac{1}{2}f''(a)(x-a)^2 + \frac{1}{6}f'''(a)(x-a)^3 + ...$

For Newton's Method, we ignore the higher-order terms with second derivatives or higher, hence:

$f(x) \approx f(a) + f'(a)(x-a)$

To calculate square roots, we are attempting to solve $x^2=a$, or in other words, we are searching for roots (zeros) of the equation:

$f(x) = x^2 - a = 0$

Differentiating, we have:

$f'(x) = 2x$

Since we are searching for $x$ such that $f(x) = 0$, we can substitute $f(x) = 0$ into our expression for Newton's approximation, and obtain:

$0 \approx f(a) + f'(a)(x-a)$

or solving for $x$:

$x \approx a - \frac{f(a)}{f'(a)}$

In terms of numerical analysis, we can then write the following as a model for numerically approximating the zeros of an arbitrary function $f(x)$: 

$x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}$

To calculate the square roots of $a$, we use $f(x) = x^2 - a$, and obtain:

$x_{n+1} = x_n - \frac{x_n^2-a}{2x_n} = \frac{2x_n^2 - x_n^2 + a}{2x_n} = \frac{x_n^2 + a}{2x_n}$

$x_{n+1} = \frac{1}{2}(x_n + \frac{a}{x_n})$

In other words, if we are calculating the square root of $a$ and we have an initial guess $x_n$, then the "next guess" $x_{n+1}$ should be to take the average of $x_n$ and $a/x_n$. This is exactly what we did by defining the `improve` procedure as:

```scheme
(define (improve guess x)
   (average guess (/ x guess)))
```

### Reference:
- [Square Roots via Newton's Method](https://math.mit.edu/~stevenj/18.335/newton-sqrt.pdf)
- [Applicative-Order vs. Normal-Order Evaluation](https://rivea0.github.io/blog/applicative-order-vs-normal-order)
- [Normal, Applicative and Lazy Evaluation](https://sookocheff.com/post/fp/evaluating-lambda-expressions/)