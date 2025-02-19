// type int, ptr
inductive empty = E(empty)
inductive val = Int(int) | Ptr(ptr)
inductive sign = Neg | Zero | Pos
intrinsic Fail() -> empty
intrinsic Owned(ptr) -> val

