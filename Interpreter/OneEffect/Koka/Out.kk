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

// Our log effect will be polymorphic in the type of messages.
effect log<a>
{
    fun log(msg : a) : ()
}

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> <div, log<value>> value)
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

fun apply(v1, v2 : value) : <div, log<value>> value
{
    match(v1)
    {
        Fun(f) -> f(v2)
        _ -> Wrong
    }
}

// We use our log effect by just calling log on the message.
fun interp(t : term, env : env) : <div, log<value>> value
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
            v.log
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

// We handle our log effect by transforming the logged values to strings and
// appending them to one big string log that is passed in the second argument
// of resume.
val handle_log_string = handler(s)
{
    return v -> (v, s)
    log(v : value) -> resume((), s + "\n" + v.show)
}

fun test_string(t : term) : div string
{
    match(handle_log_string("") {interp(t, [])})
    {
        (v, s) -> "log: " + s + "\nvalue: " + show(v)
    }
}

// We can also handle logging by appending the logged values to a list
// without converting them to strings first.
val handle_log_list = handler(l)
{
    return v -> (v, l)
    log(v : value) -> resume((), Cons(v, l))
}

fun test_list(t : term) : div string
{
    match(handle_log_list([]) {interp(t, [])})
    {
        (v, l) -> "log: " + show-list(l.reverse, show) + "\nvalue: " + show(v)
    }
}

// But the huge win lies in the ability to turn logging off entirely if we want
// to. In Haskell we have to accumulate the log and then eventually throw it
// away, which is wasteful (even if it's lazy - or even especially then, because
// thunks are not free). Here, we can throw the log away on the go without wasting
// any space.
val handle_log_nolog = handler
{
    return v -> v
    log(_ : value) -> resume(())
}

fun test_nolog(t : term) : div string
{
    show(handle_log_nolog {interp(t, [])})
}

val term0 : term =
    App(Lam("x", Add(Var("x"), Var("x"))),
        Add(Const(10), Const(11)))

val out_term0 : term =
    Out(Add(Out(Const(42)), Out(Const(23456789))))

val testTerms : list<term> =
    [term0, out_term0]

val tests : list<(term -> <div> string, string)> =
    [(test_string, "string"), (test_list, "list"), (test_nolog, "nolog")]

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