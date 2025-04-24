spec
    def List: Val -> List<int> = rec List v => 
        pred List(v) =>
          match asoptr v with
            None => List::Nil
            | Some(p) => 
                let blk = Block p 2 in
                let head = asint (Owned p) in
                let tail = List (Owned (ptradd p 1)) in
                    List::Cons(head, tail)
                end
    end

    def eqintlist : List<int> -> List<int> -> Bool = eqlist eqint

    def splitpivot : int -> List<int> -> Pair<List<int>, List<int>> = fun pivot => rec split lst => match lst : List with
        Nil => Pair::Pair(List::Nil, List::Nil)
    | Cons(x, xs) => match split xs : Pair with Pair(lstle, lstgt) => match le x pivot with
            True => Pair::Pair(List::Cons(x, lstle), lstgt)
        | False => Pair::Pair(lstle, List::Cons(x, lstgt)) end end end

    def split : List<int> -> Pair<List<int>, List<int>> = fun lst => match lst : List with Nil => Pair::Pair(List::Nil, List::Nil) 
        | Cons(pivot, lst) => match splitpivot pivot lst with Pair(lstle, lstgt) => Pair::Pair(List::Cons(pivot, lstle), lstgt) end end

    def qsort_spec : List<int> -> List<int> = rec qsort lst => match lst : List with Nil => List::Nil | Cons(pivot, lst) => match lst with Nil => List::Cons(pivot, lst)
        | Cons(x, y) => match splitpivot pivot lst with Pair(lst1, lst2) => app (qsort lst1) (List::Cons(pivot, qsort lst2)) end end end
end



fun nil() spec 
  pre: ;
  post: def foo : int = assert (eqintlist (List::Nil) (List result))
end = nullptr


fun cons(hd, tl) spec 
  pre: def hdi : int = asint hd
       def tli : List<int> = List tl;
  post: def foo : int = assert (eqintlist (List::Cons (hdi, tli)) (List result))
end locals lc =
    set lc = alloc 2;
    lc <- hd;
    lc + 1 <- tl;
    lc


fun empty(lst) spec 
  pre: def listi : List<int> = List lst;
  post:
    def listi2 : List<int> = List lst
    def foo : int = assert (eqbool (asbool result) (isempty listi))
    def nochhange : int = assert (eqintlist listi listi2)
end = lst == nullptr


fun head(lst) spec 
  pre: def listi : List<int> = List lst
       def nonempty : int = assert (not (isempty listi));
  post:
    def listi2 : List<int> = List lst
    def nochange : int = assert (eqintlist listi listi2)
    def foo : int = match listi with Nil => assert Bool::False | 
        Cons(lh, lr) => assert (eqint lh (asint result)) end
end = *lst

// specifying magic wands is hard
fun tail(lst) spec 
  pre: def listi : List<int> = List lst
       def listin : Pair<int, List<int>> = match listi with 
        Nil => fail Unit::Unit
       | Cons(hd, tl) => Pair::Pair(hd, tl) end
       def hdin : int = fst listin
       def tlin : List<int> = snd listin;
  post:
    def listres : List<int> = List result
    def istail : int = assert (eqintlist listres tlin)
    def blk : Unit = Block (asptr lst) 2
    def ishead : int = assert (eqint (asint (Owned (asptr lst))) hdin)
    def istail2 : int = assert (eqoption eqptr (asoptr (Owned (ptradd (asptr lst) 1))) (asoptr result))
end = *tailptr(lst)

fun tailptr(lst) = lst + 1

fun app(lst1, lst2) spec 
  pre: def lst1i : List<int> = List lst1
       def lst2i : List<int> = List lst2;
  post: def foo : int = assert (eqintlist (app lst1i lst2i) (List result))
end locals tmp =
    if empty(lst1) then lst2 else
    set tmp = lst1;
    while !empty(tail(lst1)) do
        set lst1 = tail(lst1)
    done;
    tailptr(lst1) <- lst2;
    tmp

fun qsort(lst) spec 
  pre: def lsti : List<int> = List lst;
  post: def foo : int = assert (eqintlist (qsort_spec lsti) (List result))
end locals pivot, left, right, rest, tmp =
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

fun printall(lst) spec 
  pre: def lsti : List<int> = List lst;
  post: 
end locals tmp =
    while !empty(lst) do
        putchar(head(lst));
        set tmp = lst;
        set lst = tail(lst);
        free tmp
    done;
    putchar(10)


fun helloworldlist() spec pre: ; post:
    def foo: List<int> = List result
    // we could say it has to be precisely that one list but meh
    def len_ok: int = assert (eqint (len foo) 13)
end = cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, nil())))))))))))))

fun verylonglist() spec pre: ; post:
    def foo: List<int> = List result
    // we could say it has to be precisely that one list but meh
    def len_ok: int = assert (eqint (len foo) 39)
end = cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, nil())))))))))))))))))))))))))))))))))))))))

fun main() spec pre: ; post: end =
    printall(verylonglist());
    printall(qsort(verylonglist()));
    42
