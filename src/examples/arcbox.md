# ArcBox

An `ArcBox` is a `Box` backed with a reference-counted storage.
It supports field projections, on top of everything else that `Box` should have.
```rust
/// A `Box` backed with a reference-counted storage. Supports field projection.
// Safety: we're the unique pointer to this `T`.
// If any field is borrowed by an `ArcBoxMap`, then this value is no longer safe, obviously.
pub struct ArcBox<T>(Arc<T>);

/// A subplace of an `ArcBox`.
pub struct ArcBoxMap<T> {
  /// Pointer to the reference counts. This keeps the backing storage allocated while any
  /// `ArcBoxMap` to it exists.
  header: NonNull<ArcHeader>,
  /// Pointer to the subplace we care about.
  val: NonNull<T>,
}

// Both support: read, write, move out, drop, borrow fields as `&`, `&mut`
// Both also support projecting to `ArcBoxMap`.

impl<T> ArcBox<T> {
  pub fn new(x: T) -> Self { .. }
  // Implemented trivially, since we can only get a full `self` if none of the fields are borrowed
  // by an `ArcBoxMap`.
  pub fn into_arc(self) -> Arc<T> { .. }
}

// `ArcBox` has no need for a `Drop` impl, it reuses the one from `Arc`.
// Drops the pointed-to value, decrements the refcount.
impl Drop for ArcBoxMap { .. }
```

Example usage:
```rust
let x: ArcBox<Foo> = ...;
let field = @ArcBoxMap x.field;
do_something(&mut x.other_field);
do_something(&mut field.whatever);
// at end of scope:
// - `field` drops normally;
// - dropck must drop `x.other_field`;
// - dropck must dispose of the empty `ArcBox` (that's not its normal `Drop`).
```

