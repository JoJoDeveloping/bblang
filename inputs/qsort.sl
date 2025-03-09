spec
    def List: Val -> List<int> = rec List v => match v : Val with
        Int(i) => fail Bool::True
      | Ptr(p) => match p with
            None => List::Nil
          | Some(p) => 
            let head = asint (Owned p) in
            let tail = List (Owned (ptradd p 1)) in
                List::Cons(head, tail)
            end end
          end
    end

    def eqlist : forall A, (A -> A -> Bool) -> List<A> -> List<A> -> Bool =
        fun eqa => rec eqlist a => fun b => match a : List with
            Nil => match b : List with
                Nil => Bool::True
              | Cons (bh, br) => Bool::False end
          | Cons (ah, ar) => match b : List with
                Nil => Bool::False
              | Cons (bh, br) => and (eqa ah bh) (eqlist ar br) end
        end

    def eqintlist : List<int> -> List<int> -> Bool = eqlist eqint

    def app : forall A, List<A> -> List<A> -> List<A> = rec app l1 => fun l2 => match l1 : List with Nil => l2 | Cons(x, xr) => List::Cons(x, app xr l2) end


    def nil : forall A, List<A> = List::Nil
    def cons : forall A, A -> List<A> -> List<A> = fun x => fun xs => List::Cons(x, xs)
    def splitpivot : int -> List<int> -> Pair<List<int>, List<int>> = fun pivot => rec split lst => match lst : List with
        Nil => Pair::Pair(List::Nil, List::Nil)
    | Cons(x, xs) => match split xs : Pair with Pair(lstle, lstgt) => match le x pivot with
            True => Pair::Pair(cons x lstle, lstgt)
        | False => Pair::Pair(lstle, cons x lstgt) end end end
    def split : List<int> -> Pair<List<int>, List<int>> = fun lst => match lst : List with Nil => Pair::Pair(List::Nil, List::Nil) 
        | Cons(pivot, lst) => match splitpivot pivot lst with Pair(lstle, lstgt) => Pair::Pair(cons pivot lstle, lstgt) end end

    def qsort : List<int> -> List<int> = rec qsort lst => match lst : List with Nil => nil | Cons(pivot, lst) => match lst with Nil => cons pivot lst
        | Cons(x, y) => match splitpivot pivot lst with Pair(lst1, lst2) => app (qsort lst1) (cons pivot (qsort lst2)) end end end
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
        Nil => fail Bool::True
       | Cons(hd, tl) => Pair::Pair(hd, tl) end
       def hdin : int = fst listin
       def tlin : List<int> = snd listin;
  post:
    def listres : List<int> = List result
    def istail : int = assert (eqintlist listres tlin)
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
  post: def foo : int = assert (eqintlist (qsort lsti) (List result))
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
end= cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, nil())))))))))))))

fun main() spec pre: ; post: end =
    printall(helloworldlist());
    printall(qsort(helloworldlist()))
