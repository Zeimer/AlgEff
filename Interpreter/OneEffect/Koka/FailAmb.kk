alias name = string

type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
    Fail
    Amb(t1 : term, t2 : term)
}

// We call our effect amb, which stands for "ambiguous". This is a misnomer,
// but the name ndet is already taken by the built-in nondeterminism effect
// (which is not really nondeterminism, but rather randomness).
// We make it so that the fail and flip operations correspond to Fail and Amb
// constructors from the syntax of our calculus.
effect amb
{
    fun fail() : a
    fun flip() : bool
}

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> <div, amb> value)
}

alias env = list<(name, value)>

fun lookupEnv(x : name, env : env) : amb value
{
    match(env)
    {
        Nil -> fail()
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

fun add(v1, v2 : value) : amb value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> Num(n + m)
        (_, _) -> fail()
    }
}

fun apply(v1, v2 : value) : <div, amb> value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> fail()
    }
}

// We handle Fail with fail() and Amb by using flip() to decide which of
// t1 and t2 we should interpret. Once again doing this is much less
// hassle than in Haskell or Idris.
fun interp(t : term, env : env) : <div, amb> value
{
    match(t)
    {
        Var(x) -> lookupEnv(x, env)
        Const(n) -> Num(n)
        Add(t1, t2) -> add(interp(t1, env), interp(t2, env))
        Lam(x, t') -> Fun(fun(a) {interp(t', Cons((x, a), env))})
        App(t1, t2) -> apply(interp(t1, env), interp(t2, env))
        Fail -> fail()
        Amb(t1, t2) -> if (flip()) then interp(t1, env) else interp(t2, env)
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
        App(t1, t2) -> "(" + show(t1) + ")" + show(t2)
        Fail -> "Fail"
        Amb(t1, t2) -> "Amb (" + show(t1) + ") (" + show(t2) + ")"
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

// We can handle the amb effect with the type maybe<value> by translating
// fail() to Nothing and making flip() always choose the left argument of
// Amb (which amounts to resuming our computation with True).
val handle_amb_maybe = handler
{
    return x -> Just(x)
    fail() -> Nothing
    flip() -> resume(True)
}

fun test_maybe(t : term) : div string
{
    match(handle_amb_maybe(fun() {interp(t, [])}))
    {
        Just(v) -> "Just(" + show(v) + ")"
        Nothing -> "Nothing"
    }
}

// To get all possible results, we handle fail() with the empty list and flip()
// by resuming first with True (which amounts to choosing the first argument of
// Amb) and then with False (which amounts to choosing the second argument) and
// finally concatenating the results.
val handle_amb_list = handler
{
    return x -> [x]
    fail() -> []
    flip() -> resume(True) + resume(False)
}

fun test_list(t : term) : div string
{
    show-list(handle_amb_list {interp(t, [])}, show)
}

// We can also translate our amb effect into the built-in ndet effect by
// translating fail() to Wrong and resuming with a random boolean for flip()
val handle_amb_random = handler
{
    return x -> x
    fail() -> Wrong
    flip() -> resume(random-bool())
}

fun test_random(t : term) : <div, ndet> string
{
    show(handle_amb_random {interp(t, [])})
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val failamb_term0 : term =
    Add(Const(42), Fail)

val failamb_term1 : term =
    Amb(Const(100), Const(12345))

val testTerms : list<term> =
    [term0, failamb_term0, failamb_term1]

// Functions are first class, so we can hold them inside a list. In fact, Koka
// is a purely functional language. Its Javascript-like syntax is only meant
// to be a trap for luring naïve imperative programmers into purely functional
// programming.
val tests : list<(term -> <div, ndet> string, string)> =
    [(test_maybe, "maybe"), (test_list, "list"), (test_random, "random")]

// Thanks to its type, foreach can be easily nested.
fun main()
{
    foreach(tests, fun(p)
    {
        val test = p.fst
        val name = p.snd

        replicate('=', 50).string.println
        println("Testing the " + name + " handler")
        replicate('=', 50).string.println
        foreach(testTerms, fun(term)
        {
            println("Interpreting " + show(term))
            term.test.println
            replicate('-', 50).string.println
        })
    })
}