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

fun show(v : value) : string
{
    match(v)
    {
        Wrong -> "<wrong>"
        Num(n) -> show(n)
        Fun(_) -> "<function>"
    }
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

val handle_list = handler
{
    return x -> [x]
    fail() -> []
    flip() -> resume(False) + resume(True)
}

fun test(t : term) : div string
{
    show-list(handle_list(fun() {interp(t, [])}), show)
}

val handle_random = handler
{
    return x -> x
    fail() -> Wrong
    flip() -> resume(random-bool())
}

fun test'(t : term) : <div, ndet> string
{
    show(handle_random(fun() {interp(t, [])}))
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val term1 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Amb(Const(1), Const(10)), Amb(Const(100), Const(1000))))

fun main()
{
    term0.test.println
}