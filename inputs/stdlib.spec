inductive List<T> = Nil | Cons(T, List<T>)
inductive Bool = True | False
inductive Pair<A, B> = Pair(A, B)
inductive Empty = !
inductive Val = Ptr(ptr) | Int(int)
inductive Option<A> = Some(A) | None


/*
type int
type ptr

def add : int -> int -> int
def mul : int -> int -> int
def le  : int -> int -> Bool
def bitand : int -> int -> int

def Owned : ptr -> Val

def ptroff : ptr -> int -> ptr
def ptrdiff : ptr -> ptr -> Option<int>
*/
