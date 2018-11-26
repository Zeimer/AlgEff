alias name = string

type term
{
    Var(x : name)
    Const(n : int)
    Add(t1 : term, t2 : term)
    Lam(x : name, t : term)
    App(t1 : term, t2 : term)
    Count
    Fail
    Amb(t1 : term, t2 : term)
    Out(t : term)
}

effect state<s>
{
    fun get() : s
    fun put(x : s) : ()
}

effect err
{
    fun err(msg : string) : a
}

effect amb
{
    fun fail() : a
    fun flip() : bool
}

effect log<a>
{
    fun log(msg : a) : ()
}

alias all = <div, state<int>, err, amb, log<string>>

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> all value)
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

fun apply(v1, v2 : value) : all value
{
    match(v1)
    {
        Fun(f) -> {tick(); f(v2)}
        _ -> err("Can't apply!")
    }
}

fun interp(t : term, env : env) : all value
{
    match(t)
    {
        Var(x) -> lookupEnv(x, env)
        Const(n) -> Num(n)
        Add(t1, t2) -> add(interp(t1, env), interp(t2, env))
        Lam(x, t') -> Fun(fun(a) {interp(t', Cons((x, a), env))})
        App(t1, t2) -> apply(interp(t1, env), interp(t2, env))
        Count -> Num(get())
        Fail -> fail()
        Amb(t1, t2) -> {tick(); if (flip()) then interp(t1, env) else interp(t2, env)}
        Out(t') ->
        {
            tick()
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
        Count -> "Count"
        Fail -> "Fail"
        Amb(t1, t2) -> "Amb (" + show(t1) + ") (" + show(t2) + ")"
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

val handle_state = handler(s)
{
    return x -> (x, s)
    get() -> resume(s, s)
    put(x) -> resume((), x)
}

val handle_err = handler
{
    return v -> Right(v)
    err(msg) -> Left(msg)
}

val handle_amb_maybe = handler
{
    return x -> Just(x)
    fail() -> Nothing
    flip() -> resume(True)
}

val handle_amb_list = handler
{
    return x -> [x]
    fail() -> []
    flip() -> resume(False) + resume(True)
}

val handle_amb_random = handler
{
    return x -> x
    fail() -> Wrong
    flip() -> resume(random-bool())
}

val handle_log_string = handler(s)
{
    return v -> (v, s)
    log(msg : string) -> resume((), s + "\n" + msg)
}

val handle_log_list = handler(l)
{
    return v -> (v, l)
    log(msg : string) -> resume((), Cons(msg, l))
}

fun test_maybe_string(t : term) : pure string
{
    match(handle_err {handle_state(0) {handle_log_string("") {handle_amb_maybe {interp(t, [])}}}})
    {
        Left(msg) -> msg
        Right(((mv, log), s)) ->
        {
            val v = match(mv)
            {
                Nothing -> "Nothing"
                Just(v) -> show(v)
            }

            "log:" +
            log + "\n" +
            "result: " + v + " (in " + show(s) + " steps)"
        }
    }
}

fun test_random_list(t : term) : <pure, ndet> string
{
    match(handle_err {handle_state(0) {handle_log_list([]) {handle_amb_random {interp(t, [])}}}})
    {
        Left(msg) -> msg
        Right(((v, log), s)) ->
        {
            "log: " + show-list(log.reverse, show) + "\n" +
            "result: " + show(v) + " (in " + show(s) + " steps)"
        }
    }
}

/*
fun test_list_list(t : term) : <pure, ndet> string
{
    match(handle_err {handle_state(0) {handle_log_list([]) {handle_amb_random {interp(t, [])}}}})
    {
        Left(msg) -> msg
        Right(l) -> foreach(l, fun(vlog, s)
        {
            "log: " + show-list(vlog.snd.reverse, show) + "\n" +
            "result: " + vlog.fst.show + " (in " + show(s) + " steps)"
        })
    }
}*/

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val count_term0 : term =
    Add(Count, Add(Count, Count))

val count_term1 : term =
    Add(Add(Count, Count), Count)

val error_term0 : term =
    Var("OBOŻETOJESTZBOŻE")

val failamb_term0 : term =
    Add(Const(42), Fail)

val failamb_term1 : term =
    Amb(Const(100), Const(1234567890))

val out_term0 : term =
    Out(Add(Out(Const(42)), Out(Const(23456789))))

val all_term0 : term =
    Amb(Fail,
        Amb(Var("OBOŻETOJESTZBOŻE"),
        Out(Add(Out(Count),
            Out(Add(Out(Count),
                Out(Count)))))))

val testTerms : list<term> =
    [term0, count_term0, count_term1, error_term0, failamb_term0, failamb_term1, out_term0, all_term0]

fun main()
{
    foreach([test_maybe_string, test_random_list], fun(test)
    {
        foreach(testTerms, fun(term)
        {
            println("Interpreting " + show(term))
            term.test.println
        })
    })
}