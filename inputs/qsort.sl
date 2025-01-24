fun nil() = nullptr
fun cons(hd, tl) locals ptr =
    set ptr = alloc 2;
    ptr <- hd;
    ptr + 1 <- tl;
    ptr
fun empty(lst) = lst == nullptr
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
    set tmp = cons(pivot, right);
    set tmp = app(left, tmp);
    tmp
fun printall(lst) locals tmp =
    while !empty(lst) do
        putchar(head(lst));
        set tmp = lst;
        set lst = tail(lst);
        free tmp
    done;
    putchar(10)
fun helloworldlist() = cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, nil())))))))))))))
fun main() = printall(helloworldlist()); printall(qsort(helloworldlist()))
