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

// We implement each of our effects separately, because this is more modular
// (and less boilerplate-y) than putting all operations into one big effect
// (that would resemble creating one big custom monad in Haskell, which is
// precisely the thing we want to avoid).
effect state<s>
{
    fun get() : s
    fun put(x : s) : ()
}

effect err
{
    fun raise(msg : string) : a
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

// We can create aliases not only for types, but also for rows of effects.
alias computation = <div, state<int>, err, amb, log<string>>

rectype value
{
    Wrong
    Num(n : int)
    Fun(f : value -> computation value)
}

alias env = list<(name, value)>

// We of course only need to mark types with the effects we are really using,
// just like in Idris or in Haskell using classes.
fun lookupEnv(x : name, env : env) : err value
{
    match(env)
    {
        Nil -> raise("Variable " + x + " not bound!")
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
        (_, _) -> raise("Can't add!")
    }
}

fun apply(v1, v2 : value) : computation value
{
    match(v1)
    {
        Fun(f) -> {tick(); f(v2)}
        _ -> raise("Can't apply!")
    }
}

// The interpreter is just a sum of the previous interpreters. Nothing fancy
// going on here.
fun interp(t : term, env : env) : computation value
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
        App(t1, t2) -> "(" + show(t1) + ")(" + show(t2) + ")"
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

// Thanks to the separation of effects, we can handle each of them separately
// and then combine the handlers. Our handlers are just like before.
val handle_state = handler(s)
{
    return x -> (x, s)
    get() -> resume(s, s)
    put(x) -> resume((), x)
}

val handle_err = handler
{
    return v -> Right(v)
    raise(msg) -> Left(msg)
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

val handle_log_nolog = handler
{
    return v -> v
    log(_ : value) -> resume(())
}

// We need to decide the order of the effects only when we want to run our
// computation, just like with Haskell's classes or Idris Effect library.
// But contrary to Haskell and Idris, composing effect handlers is very easy.
//
// In this handler, we always choose the leftmost Amb argument and put logs into
// one big string.
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

// In this handler, we keep the logs in a list (which we then have to reverse)
// and also collect all possible answers into a list.
fun test_list_list(t : term) : <div> string
{
    match(handle_err {handle_amb_list {handle_state(0) {handle_log_list([]) {interp(t, [])}}}})
    {
        Left(msg) -> msg
        Right(l) -> map(l, fun(x)
        {
            match(x)
            {
                ((v, log), s) ->
                    "log: " + show-list(log.reverse, show) + "\n" +
                    "result: " + v.show + " (in " + s.show + " steps)"
            }
        }).unlines
    }
}

// This time we turn off the logging and choose branches of Amb at random.
// By the way, the pure effect is just an alias for <div, exn> (which means
// nontermiantion and exceptions) - this directly corresponds to the notion of
// purity from Haskell and is weaker than the notion of purity from my slides.
fun test_random_nolog(t : term) : <pure, ndet> string
{
    match(handle_err {handle_state(0) {handle_log_nolog {handle_amb_random {interp(t, [])}}}})
    {
        Left(msg) -> msg
        Right((v, s)) ->
        {
            "result: " + show(v) + " (in " + show(s) + " steps)"
        }
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
    Var("OBOŻETOJESTZBOŻE")

val failamb_term0 : term =
    Add(Const(42), Fail)

val failamb_term1 : term =
    Amb(Const(100), Const(12345))

val out_term0 : term =
    Out(Add(Out(Const(42)), Out(Const(54321))))

val term1 : term =
    Amb(
        Add(
            Amb(count_term0, out_term0),
            Amb(failamb_term0, out_term0)),
        Add(term0, count_term1))

val testTerms : list<term> =
    [term0, count_term0, count_term1, error_term0, failamb_term0, failamb_term1, out_term0, term1]

// We can test our composite handlers modularly just like the simple ones.
val tests : list<(term -> <pure, ndet> string, string)> =
    [
        (test_maybe_string, "maybe_string"),
        (test_list_list, "list_list"),
        (test_random_nolog, "random_nolog")
    ]

// EXERCISE: write more handlers for the interpreter and test them. Try different
// orders (like, count the number of steps even in there's an error).
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