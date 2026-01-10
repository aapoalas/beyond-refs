# UniqueArc

`UniqueArc` is an `Arc` while it is known to be uniquely owned. Typically used for initialization,
after which it can be turned into a normal `Arc`.

```rust
// Safety: we're the unique pointer to this `T`.
// While this is live, the strong count is 0 so that we can give out other `Weak` pointers to this
// and prevent them from being upgraded.
pub struct UniqueArc<T>(Weak<T>);

// Supports: read, write, borrow fields as `&`, `&mut`

impl<T> UniqueArc<T> {
  pub fn new(x: T) -> Self { .. }
  pub fn downgrade(&self) -> Weak<T> { .. }
  // This sets the strong count to 1.
  pub fn into_arc(self) -> Arc<T> { .. }
}
```

It would be nice to be able to borrow its fields somehow. I do not know how to manage the refcounts
for that to work, plz fix if anyone knows.
```rust
/// A subplace of a `UniqueArc`.
// I have no idea how to manage the reference counts in a way that works.
pub struct UniqueArcMap<T> {
  /// Pointer to the reference counts.
  header: NonNull<ArcHeader>,
  /// Pointer to the subplace we care about.
  val: NonNull<T>,
}

// Intended permissions: read, write, borrow fields as `&`, `&mut`, reborrow fields as `UniqueArcMap`
// The original `UniqueArc::into_arc` should only be allowed after all the `UniqueArcMap`s have
// expired.
```
