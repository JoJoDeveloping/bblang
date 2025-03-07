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

def ptroff : ptr -> int -> ptr
def ptrdiff : ptr -> ptr -> Option<int>
*/

def and : Bool -> Bool -> Bool = fun a : Bool => fun b => match a with True => b | False => Bool::False end

def sub : int -> int -> int = fun a => fun b => add a (mul b (neg 1))
def eqint : int -> int -> Bool = fun a => fun b => and (le a b) (le b a)

def fail : forall A, Bool -> A = fun b => match Fail b with end
def asint : Val -> int = fun n => match n : Val with Int(i) => i | Ptr(x) => fail Bool::True end
def asptr : Val -> ptr = fun n => match n : Val with Ptr(p) => match p with Some(p) => p | None => fail Bool::True end | Int(i) => fail Bool::True end
