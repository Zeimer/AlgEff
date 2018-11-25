alias name = string

type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
    Out(t : term)
}

effect log<a>
{
    fun log(msg : a) : ()
}

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> <div, log<string>> value)
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

fun apply(v1, v2 : value) : <div, log<string>> value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> Wrong
    }
}

fun interp(t : term, env : env) : <div, log<string>> value
{
    match(t)
    {
        Var(x) -> lookupEnv(x, env)
        Const(n) -> Num(n)
        Add(t1, t2) -> add(interp(t1, env), interp(t2, env))
        Lam(x, t') -> Fun(fun(a) {interp(t', Cons((x, a), env))})
        App(t1, t2) -> apply(interp(t1, env), interp(t2, env))
        Out(t') ->
        {
            val v = interp(t', env)
            v.show.log
            v
        }
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
        Out(t') -> "Out(" + show(t') + ")"
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

val handle_log_string = handler(s)
{
    return v -> (v, s)
    log(msg : string) -> resume((), s + "\n" + msg)
}

fun test_string(t : term) : div string
{
    match(handle_log_string("") {interp(t, [])})
    {
        (v, s) -> "log: " + s + "\nvalue: " + show(v)
    }
}

val handle_log_list = handler(l)
{
    return v -> (v, l)
    log(msg : string) -> resume((), Cons(msg, l))
}

fun test_list(t : term) : div string
{
    match(handle_log_list([]) {interp(t, [])})
    {
        (v, l) -> "log: " + show-list(l.reverse, show) + "\nvalue: " + show(v)
    }
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val out_term0 : term =
    Out(Add(Out(Const(42)), Out(Const(23456789))))

val testTerms : list<term> =
    [term0, out_term0]

fun main()
{
    foreach([test_string, test_list], fun(test)
    {
        foreach(testTerms, fun(term)
        {
            println("Interpreting " + show(term))
            term.test.println
        })
    })
}