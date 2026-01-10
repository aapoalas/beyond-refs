# SharedMutableRef

This is a funky counter-example to how one might think borrows must chain: it's a shared reference
through which you can write.

Writing ability is justified by the `!Send` and `!Sync` impls: this can only be used on a single
thread.

```rust
struct SharedMutableRef<'a, T>(NonNull<T>, PhantomData<&'a mut T>);
impl !Send for SharedMutableRef<'a, T> {}
impl !Sync for SharedMutableRef<'a, T> {}

// Supports: local T -> SharedMutableRef<T> exclusive borrow
// Supports: &mut T -> SharedMutableRef<T> exclusive reborrow
// Supports: SharedMutableRef<T> -> SharedMutableRef<T> *shared* reborrow
// Supports: read, write
// Importantly doesn't support &T or &mut T reborrows
```

Example usage:
```rust
let mut x = 42;
let weird = @SharedMutableRef x;
let reborrow1 = @SharedMutableRef *weird;
let reborrow2 = @SharedMutableRef *weird; // look it's a shared ref
*reborrow1 += 1; // hehe
*reborrow2 += 1;
// `x` is inaccessible while any of the above refs are live
```
