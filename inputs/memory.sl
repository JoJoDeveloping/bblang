spec
    def fib: int -> int = rec fib n => match le n 1 with True => n | False => add (fib (sub n 1)) (fib (sub n 2)) end
end

fun mkcell(n) spec 
  pre: ;
  post: def foo : int = match eqint (asint n) (asint (Owned (asptr result))) with True => 0 | False => fail Bool::True end
end locals lc =
    set lc = alloc 1;
    lc <- n;
    lc


fun inccell(pa) spec 
  pre: def inccell_initial: int = asint (Owned (asptr pa));
  post: def foo : int = match eqint (add inccell_initial 1) (asint (Owned (asptr pa))) with True => 0 | False => fail Bool::True end
end =
    pa <- *pa + 1; 0

fun popcell(pa) spec
  pre: def val: int = asint (Owned (asptr pa));
  post: def foo : int = match eqint (asint result) val with True => 0 | False => fail Bool::True end
end locals lc =
  set lc = *pa;
  free pa;
  lc


fun main() locals foo = 
  set foo = mkcell(42);
  inccell(foo);
  inccell(foo);
  inccell(foo);
  inccell(foo);
  inccell(foo);
  popcell(foo)
