inductive Nat = Zero | Succ(nat)
inductive List<T> = Nil | Cons(T, List<T>)

def add : Nat -> Nat -> Nat = rec add n => fun m => match n : Nat with Zero => M | Succ(n') => Nat::Succ(add n' m) end
def mul : Nat -> Nat -> Nat = rec mul n => fun m => match n : Nat with Zero => Nat::Zero | Succ(n') => add m (mul n' m) end
