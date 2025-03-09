inductive List<T> = Nil | Cons(T, List<T>)
inductive Bool = True | False
inductive Pair<A, B> = Pair(A, B)
inductive Empty = !
inductive Option<A> = Some(A) | None
inductive Val = Ptr(Option<ptr>) | Int(int)


/*
type int
type ptr

def add : int -> int -> int
def mul : int -> int -> int
def le  : int -> int -> Bool
def bitand : int -> int -> int
def neg : int -> int

def Owned : ptr -> Val
def Fail : Bool -> Empty

def ptradd : ptr -> int -> ptr
def ptrdiff : ptr -> ptr -> Option<int>
*/

def and : Bool -> Bool -> Bool = fun a : Bool => fun b => match a with True => b | False => Bool::False end

def sub : int -> int -> int = fun a => fun b => add a (mul b (neg 1))
def eqint : int -> int -> Bool = fun a => fun b => and (le a b) (le b a)

def fail : forall A, Bool -> A = fun b => match Fail b with end
def asint : Val -> int = fun n => match n : Val with Int(i) => i | Ptr(x) => fail Bool::True end
def asoptr : Val -> Option<ptr> = fun n => match n : Val with Ptr(p) => p | Int(i) => fail Bool::True end
def asptr : Val -> ptr = fun n => match n : Val with Ptr(p) => match p with Some(p) => p | None => fail Bool::True end | Int(i) => fail Bool::True end

def assert : Bool -> int = fun b => match b : Bool with True => 0 | False => fail Bool::True end

def not : Bool -> Bool = fun a => match a : Bool with
    True => Bool::False | False => Bool::True end

def eqbool : Bool -> Bool -> Bool = fun a => fun b =>
    match a : Bool with
        True => b
      | False => not b end

def asbool : Val -> Bool = fun n => not (eqint (asint n) 0)

def isempty : forall A, List<A> -> Bool = fun a => match a : List with Nil => Bool::True | Cons(x, y) => Bool::False end

def fst : forall A B, Pair<A, B> -> A = fun a => match a : Pair with Pair(a,b) => a end
def snd : forall A B, Pair<A, B> -> B = fun a => match a : Pair with Pair(a,b) => b end

def eqptr : ptr -> ptr -> Bool = fun a => fun b => match ptrdiff a b with
  None => Bool::False
| Some(x) => eqint x 0 end


def eqoption : forall A, (A -> A -> Bool) -> Option<A> -> Option<A> -> Bool = fun eqa => fun a => fun b => match a : Option with None => match b : Option with None => Bool::True | Some(x) => Bool::False end
| Some(ax) => match b : Option with None => Bool::False | Some(bx) => eqa ax bx end end
