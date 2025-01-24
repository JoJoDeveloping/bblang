fun fib(n) locals i, j, t =
    set i = 0;
    set j = 1;
    while 0 < n do
        set t = i+j;
        set i = j;
        set j = t;
        set n = n - 1
    done;
    i
fun main() = fib(40)
