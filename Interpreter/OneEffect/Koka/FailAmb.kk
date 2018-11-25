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

fun lookupEnv(x : name, env : env) : value
{
    match(env)
    {
        Nil -> Wrong
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

fun add(v1, v2 : value) : value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> Num(n + m)
        (_, _) -> Wrong
    }
}

fun apply(v1, v2 : value) : <div, amb> value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> Wrong
    }
}

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

val handle_amb_list = handler
{
    return x -> [x]
    fail() -> []
    flip() -> resume(False) + resume(True)
}

fun test_list(t : term) : div string
{
    show-list(handle_amb_list {interp(t, [])}, show)
}

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
    Amb(Const(100), Const(1234567890))

val testTerms : list<term> =
    [term0, failamb_term0, failamb_term1]

fun main()
{
    foreach([test_maybe, test_list, test_random], fun(test)
    {
        foreach(testTerms, fun(term)
        {
            println("Interpreting " + show(term))
            term.test.println
        })
    })
}