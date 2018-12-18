effect console
{
    fun putStrLn(str : string) : ()
    fun getLine() : string
}

val run = handler
{
    return x -> x
    putStrLn(str) -> {println(str); resume(())}
    getLine -> resume("Wydmuszka")
}

fun main()
{
    run {putStrLn("Hello wurst!")}
}