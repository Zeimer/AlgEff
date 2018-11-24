effect teletype
{
    fun putStrLn(str : string) : ()
    fun getLine() : string
    fun exit() : ()
}

val run = handler
{
    return x -> x
    putStrLn(str) -> {println(str); resume(())}
    getLine -> resume("Wydmuszka")
    exit() -> ()
}

fun main()
{
    run {putStrLn("Hello wurst!")}
}