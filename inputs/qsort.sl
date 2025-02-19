/*
pred List(ptr) : intlist = if ptr then Cons(AsInt(Owned(ptr)), List(Owned(ptr + 1))) else Nil()
*/

/*
spec: nil() =
    ensure (List(result) == Nil())
*/
fun nil() = nullptr

/*
spec: cons(hd, tl) =
    let tl = List(tl);
    ensure (List(result) == Cons(hd, tl))
*/
fun cons(hd, tl) locals lc =
    set lc = alloc 2;
    lc <- hd;
    lc + 1 <- tl;
    lc


/*
spec: empty(lst) =
    let v = List(lst);
    ensure result <-> v == Nil();
    ensure v == List(lst) //the list remains unchanged
*/
fun empty(lst) = lst == nullptr

/*
spec: head(lst) =
    let v = List(lst);
    match v with Nil() => assert False | Cons(hd, tl) =>
        ensure result == hd;
        ensure v == List(lst) //the list remains unchanged
    end
*/
fun head(lst) = *lst

fun tail(lst) = *tailptr(lst)

fun tailptr(lst) = lst + 1

fun app(lst1, lst2) locals tmp =
    if empty(lst1) then lst2 else
    set tmp = lst1;
    while !empty(tail(lst1)) do
        set lst1 = tail(lst1)
    done;
    tailptr(lst1) <- lst2;
    tmp

fun qsort(lst) locals pivot, left, right, rest, tmp =
    if empty(lst) then lst else
    set pivot = head(lst); set rest = tail(lst);
    if empty(rest) then lst else
    set left = nil(); set right = nil();
    free lst;
    while !empty(rest) do
        (if head(rest) < pivot
        then set left = cons(head(rest), left)
        else set right = cons(head(rest), right));
        set tmp = rest;
        set rest = tail(rest);
        free tmp
    done;
    set left = qsort(left);
    set right = qsort(right);
    app(left, cons(pivot, right))

fun printall(lst) locals tmp =
    while !empty(lst) do
        putchar(head(lst));
        set tmp = lst;
        set lst = tail(lst);
        free tmp
    done;
    putchar(10)

fun helloworldlist() = cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, nil())))))))))))))

fun main() =
    printall(helloworldlist());
    printall(qsort(helloworldlist()))
