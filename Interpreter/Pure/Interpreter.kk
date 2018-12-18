// In Koka, we create type aliases using the "alias" keyword.
alias name = string

// The keyword "type" is for defining strictly positive algebraic data types.
type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
}

// The keyword "rectype" is for defining arbitrary algebraic data types. It's
// like "data" in Haskell. The following can't be defined using the keyword
// "type", because value appears to the left of the arrow in the Fun
// constructor.
//
// The syntax e t means the type t can have the effect(s) e. Below, div value
// means the type of values that can diverge, i.e. not terminate. We must use
// it because not all lambda calculus terms have a normal form and thus the
// interpreter can possily diverge. This is not optional - not doing so would
// be considered a type error if we tried to make a recursive call when defining
// the interpreter.
rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> div value)
}

// Koka syntax for type parameters is <>, so a list of a's is written list<a>
// The product of a and b is written (a, b)
alias env = list<(name, value)>

// Functions are declared with "fun". The syntax for if-then-else is, somewhat
// weirdly, if (condition) then ... else ...
fun lookupEnv(x : name, env : env) : value
{
    match(env)
    {
        Nil -> Wrong
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

// Note that the return type of add is value...
fun add(v1, v2 : value) : value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> Num(n + m)
        (_, _) -> Wrong
    }
}

// ... but the return type of apply is div value. The difference is that
// addition always terminates, whereas the function f need not, so we need
// to mark apply as possibly nonterminating using div.
fun apply(v1, v2 : value) : div value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> Wrong
    }
}

// The interpreter also has to be marked with div. Note that even though apply
// expects arguments of type value, we are passing it interp(t1, env) and
// interp(t2, env) which are of type div value. This doesn't pose a problem
// at all - we can use effectful computations as if they were pure values and
// Koka deals for us with the details of how this works.
fun interp(t : term, env : env) : div value
{
    match(t)
    {
        Var(x) -> lookupEnv(x, env)
        Const(n) -> Num(n)
        Add(t1, t2) -> add(interp(t1, env), interp(t2, env))
        Lam(x, t') -> Fun(fun(a) {interp(t', Cons((x, a), env))})
        App(t1, t2) -> apply(interp(t1, env), interp(t2, env))
    }
}

// It looks like Koka doesn't have typeclasses or anything like that.
// show is thus an ordinary monomorphic function.
fun show(t : term) : string
{
    match(t)
    {
        Var(x) -> x
        Const(n) -> show(n)
        Add(t1, t2) -> show(t1) + " + (" + show(t2) + ")"
        Lam(x, t') -> "λ" + x + "." + show(t')
        App(t1, t2) -> "(" + show(t1) + ")(" + show(t2) + ")"
    }
}

// But functions can be overloaded ad-hoc, so we can also define show for our
// type of values. Koka decides which of the overloaded functions should be
// used by using their type signatures. If Koka can't decide, we can help her
// by annotating the argument with the syntax f(arg : type).
fun show(v : value) : string
{
    match(v)
    {
        Wrong -> "<wrong>"
        Num(n) -> show(n)
        Fun(_) -> "<function>"
    }
}

// The syntax interp(t, []).show is a syntactic sugar for show(interp(t, []))
fun test(t : term) : div string
{
    interp(t, []).show
}

// Non-functions are defined with the keyword "val".
val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

// main actually has type () -> <div, console> ()
// This is because it can diverge (due to test which can diverge due to interp)
// and it performs console IO with the println function.
//
// But the type of main is not fixed. If we uncommented the call to random-bool
// at line 140, main would have type () -> <div, console, ndet> bool
// because random-bool() is a boolean (and the last expression in a function is
// considered to be the return value) and it has the effect ndet which stands
// for "nondeterminism".
//
// Also note that functions can contain "-" in their names, like in Lisp.
fun main()
{
    println("Interpreting " + show(term0))
    term0.test.println
    //random-bool()
}