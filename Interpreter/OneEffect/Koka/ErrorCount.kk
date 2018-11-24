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

effect err
{
    fun err(msg : string) : a
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
    Fun(f : value -> <div, state<int>, err>  value)
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

fun lookupEnv(x : name, env : env) : err value
{
    match(env)
    {
        Nil -> err("Variable " + x + " not bound!")
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

fun tick() : state<int> ()
{
    val n = get()
    put(n + 1)
}

fun add(v1, v2 : value) : <state<int>, err> value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> {tick(); Num(n + m)}
        (_, _) -> err("Can't add!")
    }
}

fun apply(v1, v2 : value) : <div, state<int>, err> value
{
    match(v1)
    {
        Fun(f) -> {tick(); f(v2)}
        _ -> err("Can't apply!")
    }
}

fun interp(t : term, env : env) : <div, state<int>, err> value
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

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val term1 : term =
    Add(Count, Add(Count, Count))

val term2 : term =
    Add(Add(Count, Count), Count)

val term3 : term =
    Var("O BOŻE TO JEST ZBOŻE")

val handle_state = handler(s)
{
    return v -> (v, s)
    get() -> resume(s, s)
    put(x) -> resume((), x)
}

val handle_err = handler
{
    return v -> show(v : value)
    err(msg) -> msg
}

fun test(t : term) : div string
{
    match(handle_state(0) {handle_err {interp(t, [])}})
    {
        (v, s) -> v + " (in " + show(s) + " steps)"
    }
    /*match(handle_err {handle_state(0) {interp(t, [])}})
    {
        (v, s) -> v + " (in " + show(s) + " steps)"
    }*/
}

fun main()
{
    term0.test.println
    term1.test.println
    term2.test.println
    term3.test.println
}