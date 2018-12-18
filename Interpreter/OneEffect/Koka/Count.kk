alias name = string

type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
    Count
}

// We declare effects with the effect keyword. An effect has a name and it can
// have type parameters. Each effect is associated with a set of operations that
// have names and are of some type.
//
// Here we declare an effect called "state" that is polymorphic in the type of
// the thing it holds. It has two operations, get of type () -> s and put of
// of type s -> ()
effect state<s>
{
    fun get() : s
    fun put(x : s) : ()
}

// We can compose multiple effects by putting them in angle brackets.
// The notation <e_1, ..., e_n> a means a computation that possibly
// returns something of type a and can have effects e_1, ..., e_n.
// If there's only one effect, we can write e a instead of <e> a.
// Here our values will be both (possibly) divergent and stateful.
rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> <div, state<int>> value)
}

alias env = list<(name, value)>

fun lookupEnv(x : name, env : env) : value
{
    match(env)
    {
        Nil -> Wrong
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

// Contrary to Haskell and Idris, we don't need to use monadic style or any
// do-like notation - we can use put and get as if they were normal functions.
fun tick() : state<int> ()
{
    put(get() + 1)
}

// The code is not much different from Haskell and Idris, besides the fact that
// we can use ordinary sequencing ; instead of elaborate monadic sequencing.
fun add(v1, v2 : value) : state<int> value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> {tick(); Num(n + m)}
        (_, _) -> Wrong
    }
}

fun apply(v1, v2 : value) : <div, state<int>> value
{
    match(v1)
    {
        Fun(f) -> {tick(); f(v2)}
        _ -> Wrong
    }
}

// We interpret Count using get(). Note how much easier this is than in Haskell
// and Idris.
fun interp(t : term, env : env) : <div, state<int>> value
{
    match(t)
    {
        Var(x) -> lookupEnv(x, env)
        Const(n) -> Num(n)
        Add(t1, t2) -> add(interp(t1, env), interp(t2, env))
        Lam(x, t') -> Fun(fun(a) {interp(t', Cons((x, a), env))})
        App(t1, t2) -> apply(interp(t1, env), interp(t2, env))
        Count -> Num(get())
    }
}

fun show(t : term) : string
{
    match(t)
    {
        Var(x) -> x
        Const(n) -> show(n)
        Add(t1, t2) -> show(t1) + " + (" + show(t2) + ")"
        Lam(x, t') -> "λ" + x + "." + show(t')
        App(t1, t2) -> "(" + show(t1) + ")(" + show(t2) + ")"
        Count -> "Count"
    }
}

fun show(v : value) : string
{
    match(v)
    {
        Wrong -> "<wrong>"
        Num(n) -> show(n)
        Fun(_) -> "<function>"
    }
}

// If we have an effectful computation, we can get rid of the effect by
// handling it. We do this by pattern-matching on operations of the
// effect that we want to handle. In each case we can do something with
// the arguments of the operation and then abort our computation, resume
// it using the keyword "resume" or even resume it multiple times. We can
// also transform the result of the computation by matching the special
// "return" clause.
//
// Here we handle our state effect by passing the state in the second argument
// of resume. When we're done computing, we return the result together with its
// state. When we handle get, the first argument of resume is s, because get is
// supposed to return some state and this is the only state we have. When we
// handle put, the first argument of resume is (), because this is the only thing
// that matches put's return type.
val handle_state = handler(s)
{
    return x -> (x, s)
    get() -> resume(s, s)
    put(x) -> resume((), x)
}

// We can use our handler with the syntax
//
// handler_name(args) { expression to be handled }
//
// Here we want our initial state to be 0. After handling the interpreter,
// we get a pair (result, final state) which we match to transform it into
// a string. Note that the return type is div string, because we handle the
// state effect, but we didn't handle the div effect. In fact I think we
// can't handle it, because nontermination is a somewhat abstract and built-in
// effect that has no operations.
fun test(t : term) : div string
{
    match(handle_state(0) {interp(t, [])})
    {
        (v, s) -> show(v) + " (in " + show(s) + " steps)"
    }
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val count_term0 : term =
    Add(Count, Add(Count, Count))

val count_term1 : term =
    Add(Add(Count, Count), Count)

val testTerms : list<term> =
    [term0, count_term0, count_term1]

// We can iterate over a list using foreach. Note the type of this function
//
// foreach: forall<a, e> (xs : list <a>, action: a -> e ()) -> e ()
//
// which means that foreach takes a list of a and any effectful computation
// that transforms a to () and produces a computation returning () that has
// the same effects. This must have been way easier to implement than writing
// a functor instance with fmap for each functor separately!
fun main()
{
    foreach(testTerms, fun(t)
    {
        replicate('-', 50).string.println
        println("Interpreting " + show(t))
        t.test.println
        replicate('-', 50).string.println
    })
}