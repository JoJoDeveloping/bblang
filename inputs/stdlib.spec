inductive List<T> = Nil | Cons(T, List<T>)
inductive Bool = True | False
inductive Pair<A, B> = Pair(A, B)
inductive Empty = !
inductive Option<A> = Some(A) | None
inductive Val = Ptr(Option<ptr>) | Int(int)
inductive Unit = Unit


/*
type int
type ptr

def add : int -> int -> int
def mul : int -> int -> int
def le  : int -> int -> Bool
def bitand : int -> int -> int
def neg : int -> int

def eq : forall A, A -> A -> Bool

def Owned : ptr -> Val
def Block : ptr -> int -> Unit
def Fail : Bool -> Empty

def ptradd : ptr -> int -> ptr
def ptrdiff : ptr -> ptr -> Option<int>
*/

def and : Bool -> Bool -> Bool = fun a : Bool => fun b => match a with True => b | False => Bool::False end

def sub : int -> int -> int = fun a => fun b => add a (mul b (neg 1))
def eqint : int -> int -> Bool = eq

def fail : forall A, Unit -> A = fun b => match Fail Bool::True with end
def fail_flipped : forall A, Unit -> A = fun b => match Fail Bool::False with end
def asint : Val -> int = fun n => match n : Val with Int(i) => i | Ptr(x) => fail Unit::Unit end
def asoptr : Val -> Option<ptr> = fun n => match n : Val with Ptr(p) => p | Int(i) => fail Unit::Unit end
def asptr : Val -> ptr = fun n => match n : Val with Ptr(p) => match p with Some(p) => p | None => fail Unit::Unit end | Int(i) => fail Unit::Unit end

def assert : Bool -> int = fun b => match b : Bool with True => 0 | False => fail Unit::Unit end

def not : Bool -> Bool = fun a => match a : Bool with
    True => Bool::False | False => Bool::True end

def eqbool : Bool -> Bool -> Bool = eq

def asbool : Val -> Bool = fun n => not (eqint (asint n) 0)

def isempty : forall A, List<A> -> Bool = fun a => match a : List with Nil => Bool::True | Cons(x, y) => Bool::False end

def fst : forall A B, Pair<A, B> -> A = fun a => match a : Pair with Pair(a,b) => a end
def snd : forall A B, Pair<A, B> -> B = fun a => match a : Pair with Pair(a,b) => b end

def eqptr : ptr -> ptr -> Bool = eq


def eqoption : forall A, (A -> A -> Bool) -> Option<A> -> Option<A> -> Bool = fun eqa => fun a => fun b => match a : Option with None => match b : Option with None => Bool::True | Some(x) => Bool::False end
| Some(ax) => match b : Option with None => Bool::False | Some(bx) => eqa ax bx end end


def len : forall A, List<A> -> int = rec len lst => match lst : List with Nil => 0 | Cons(x, xr) => add 1 (len xr) end



def eqlist : forall A, (A -> A -> Bool) -> List<A> -> List<A> -> Bool =
    fun eqa => rec eqlist a => fun b => match a : List with
        Nil => match b : List with
            Nil => Bool::True
            | Cons (bh, br) => Bool::False end
        | Cons (ah, ar) => match b : List with
            Nil => Bool::False
            | Cons (bh, br) => and (eqa ah bh) (eqlist ar br) end
    end


def app : forall A, List<A> -> List<A> -> List<A> = rec app l1 => fun l2 => match l1 : List with Nil => l2 | Cons(x, xr) => List::Cons(x, app xr l2) end
