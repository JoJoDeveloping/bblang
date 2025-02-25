inductive Nat = Zero | Succ(Nat)
inductive List<T> = Nil | Cons(T, List<T>)
inductive Bool = True | False
inductive Pair<A, B> = Pair(A, B)
inductive Empty = !

def add : Nat -> Nat -> Nat = rec add n => fun m => match n : Nat with Zero => m | Succ(n') => Nat::Succ(add n' m) end
def mul : Nat -> Nat -> Nat = rec mul n => fun m => match n : Nat with Zero => Nat::Zero | Succ(n') => add m (mul n' m) end
def le : Nat -> Nat -> Bool = rec le n => fun m => match n : Nat with Zero => Bool::True | Succ(n') => match m : Nat with Zero => Bool::False | Succ(m') => le n' m' end end
def and : Bool -> Bool -> Bool = fun b1 => fun b2 => match b1 : Bool with False => Bool::False | True => b2 end
def eq : Nat -> Nat -> Bool = fun n1 => fun n2 => and (le n1 n2) (le n2 n1)
def nil : forall A, List<A> = List::Nil
def cons : forall A, A -> List<A> -> List<A> = fun x => fun xs => List::Cons(x, xs)
def splitpivot : Nat -> List<Nat> -> Pair<List<Nat>, List<Nat>> = fun pivot => rec split lst => match lst : List with
    Nil => Pair::Pair(List::Nil, List::Nil)
  | Cons(x, xs) => match split xs : Pair with Pair(lstle, lstgt) => match le x pivot with
        True => Pair::Pair(cons x lstle, lstgt)
      | False => Pair::Pair(lstle, cons x lstgt) end end end
def split : List<Nat> -> Pair<List<Nat>, List<Nat>> = fun lst => match lst : List with Nil => Pair::Pair(List::Nil, List::Nil) 
    | Cons(pivot, lst) => match splitpivot pivot lst with Pair(lstle, lstgt) => Pair::Pair(cons pivot lstle, lstgt) end end
def app : forall A, List<A> -> List<A> -> List<A> = rec app l1 => fun l2 => match l1 : List with Nil => l2 | Cons(x, xr) => List::Cons(x, app xr l2) end
def len : forall A, List<A> -> Nat = rec len lst => match lst : List with Nil => Nat::Zero | Cons(x, xr) => Nat::Succ(len xr) end
def O : Nat = Nat::Zero
def S : Nat -> Nat = fun x => Nat::Succ(x)

def qsort : List<Nat> -> List<Nat> = rec qsort lst => match lst : List with Nil => nil | Cons(pivot, lst) => match lst with Nil => cons pivot lst
    | Cons(x, y) => match splitpivot pivot lst with Pair(lst1, lst2) => app (qsort lst1) (cons pivot (qsort lst2)) end end end

def two : Nat = S (S O)
def three : Nat = S two
def six : Nat = mul two three
def seven : Nat = S six
def fourtytwo : Nat = mul six seven
def test1 : Bool = le two fourtytwo
def test2 : Bool = le fourtytwo two
def test3 : Bool = eq two fourtytwo
def test4 : Bool = eq fourtytwo fourtytwo

def examplelist : List<Nat> = cons three (cons six (cons O (cons seven (cons two nil))))
def sortedlist : List<Nat> = qsort examplelist
def diverge : forall A B, A -> B = rec foo bar => foo bar

def numtest : int = 42
