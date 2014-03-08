package benchtest

class Benches{
    def stopWatch[A](name: String)(arg: => A): A = {
    val start = System.currentTimeMillis
    val retval: A = arg
    val end = System.currentTimeMillis
    println(name + " took " + (end - start).toString + " ms")
    retval
  }
}