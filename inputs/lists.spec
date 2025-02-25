// type ptr
fun isnull : ptr -> Bool,
fun Owned : ptr -> Value,
fun ptroff : ptr -> int -> ptr
fun ptrdiff : ptr -> ptr -> Option<int>

// type int
// actually let's make this a usable type at language level
fn eq, le
fn add, sub, mul
fn neg
fn nthbit


inductive Value = Ptr(ptr) | Int(int)
