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

effect state<s>
{
    fun get() : s
    fun put(x : s) : ()
}

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

fun tick() : state<int> ()
{
    val n = get()
    put(n + 1)
}

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
        App(t1, t2) -> "(" + show(t1) + ")" + show(t2)
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

val handle_state = handler(s)
{
    return x -> (x, s)
    get() -> resume(s, s)
    put(x) -> resume((), x)
}

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

fun main()
{
    foreach(testTerms, fun(t)
    {
        println("Interpreting " + show(t))
        t.test.println
    })
}