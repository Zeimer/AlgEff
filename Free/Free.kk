import std/os/readline

// The syntax for importing stuff is apparently the/path/to/the/imported/thing
// Apparently, because sadly Koka is a very badly documented language.

// With a single definition of a single effect in Koka we can achieve the same
// thing for which in Haskell we needed an algebraic data type, its functor
// instance, a type alias and some boilerplate. This is a huge win!
effect console
{
    fun putStrLn(str : string) : ()
    fun getLine() : string
}

// Writing the echo function is just as simple.
fun echo() : console ()
{
    putStrLn("Write something!")
    putStrLn("You wrote: " + getLine())
}

// We can handle our effect by interpreting it using the standard library's way
// of doing IO. This definition is conceptually simpler than the free monad one.
val run = handler
{
    return x -> x
    putStrLn(str) -> {println(str); resume(())}
    getLine -> resume(readline())
}

// Our Haskell program was 24 lines of code long (not including comments and spaces).
// In Koka this is 21 lines, but including the omnipresent evil curly braces.
fun main()
{
    run {echo()}
}