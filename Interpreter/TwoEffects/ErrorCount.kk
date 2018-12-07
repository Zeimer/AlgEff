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

// We implement both our effects separately, because this is more modular (and
// less boilerplate-y) than putting all operations into one big effect (that
// would resemble creating one big custom monad in Haskell, which is precisely
// the thing we want to avoid).
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
    Fun(f : value -> <div, state<int>, err> value)
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
    put(get() + 1)
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

// Thanks to the separation of effects, we can handle each of them separately
// and then combine the handlers.
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

// We need to decide the order of the effects when we handle them and not right
// up front like in Haskell. This is a huge win, as it separates interface from
// implementation.
fun test(t : term) : div string
{
    match(handle_state(0) {handle_err {interp(t, [])}})
    {
        (v, s) -> v + " (in " + show(s) + " steps)"
    }
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val count_term0 : term =
    Add(Count, Add(Count, Count))

val count_term1 : term =
    Add(Add(Count, Count), Count)

val error_term0 : term =
    Var("O BOŻE TO JEST ZBOŻE")

val testTerms : list<term> =
    [term0, count_term0, count_term1, error_term0]

fun main()
{
    foreach(testTerms, fun(t)
    {
        println("Interpreting " + show(t))
        t.test.println
    })
}