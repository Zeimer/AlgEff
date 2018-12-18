alias name = string

type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
}

// Our error effect has one operation, raise, of type string -> a. Note that
// raise is not polymorphic in a, which means that err can return a result of
// any type whatsoever. How it will achieve this is a matter of its handler.
effect err
{
    fun raise(msg : string) : a
}

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> <div, err> value)
}

alias env = list<(name, value)>

// In particular, the raise operation can act as a value for our calculus.
fun lookupEnv(x : name, env : env) : err value
{
    match(env)
    {
        Nil -> raise("Variable " + x + " not bound!")
        Cons((y, v), env') -> if (x == y) then v else lookupEnv(x, env')
    }
}

fun add(v1, v2 : value) : err value
{
    match((v1, v2))
    {
        (Num(n), Num(m)) -> Num(n + m)
        (_, _) -> raise("Can't add!")
    }
}

fun apply(v1, v2 : value) : <div, err> value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> raise("Can't apply!")
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

fun show(v : value) : string
{
    match(v)
    {
        Wrong -> "<wrong>"
        Num(n) -> show(n)
        Fun(_) -> "<function>"
    }
}

// We handle errors by translating them to the type either<string, value>.
// Pure values become Rights and errors become Lefts.
val handle_err = handler
{
    return v -> Right(v)
    raise(msg) -> Left(msg)
}

// After calling our handler, we have to convert both values and error
// messages into strings anyway.
fun test(t : term) : div string
{
    match(handle_err {interp(t, [])})
    {
        Left(msg) -> msg
        Right(v) -> show(v : value)
    }
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val error_term0 : term =
    Var("OBOŻETOJESTZBOŻE")

val testTerms : list<term> =
    [term0, error_term0]

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