alias name = string

type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
}

effect err
{
    fun err(msg : string) : a
}

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> <div, err> value)
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

fun lookupEnv(x : name, env : env) : <err> value
{
    match(env)
    {
        Nil -> err("Variable " + x + " not bound!")
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

fun add(v1, v2 : value) : <err> value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> Num(n + m)
        (_, _) -> err("Can't add!")
    }
}

fun apply(v1, v2 : value) : <div, err> value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> err("Can't apply!")
    }
}

fun interp(t : term, env : env) : <div, err> value
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

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

fun test(t : term) : div string
{
    val h = handler
    {
        return x -> show(x : value)
        err(msg) -> msg
    }
    h(fun() {interp(t, [])})
}

fun main()
{
    term0.test.println
}