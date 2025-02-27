spec
    def fib: int -> int = rec fib n => match le n 1 with True => n | False => add (fib (sub n 1)) (fib (sub n 2)) end
end

fun fib(n) spec
  pre : 
    def foo : int = match le 0 (asint n) with True => 0 | False => fail Bool::True end;
  post :
    def bar : int = match eqint (asint result) (fib (asint n)) with True => 0 | False => fail Bool::True end
end locals i, j, t =
    set i = 0;
    set j = 1;
    while 0 < n do
        set t = i+j;
        set i = j;
        set j = t;
        set n = n - 1
    done;
    i
fun main() = fib(10)
