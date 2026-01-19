# In-place initialization

Initializing values in-place without copying them. This eliminates unnecessary copies and allows for self-referential datastructures.

## Range of use cases

TODO: Cover the range of use cases like

* Pinned vs unpinned
* Constructor functions
* Fallible

## Approaches

* [Init expressions](./init-exprs.md)
* [Out pointers](./out-pointers.md)
* [Placing](./placing.md)
* [Guaranteed value emplacement](./guaranteed-emplacement.md)

## Potential design axioms

Here we propose design axioms that proposals should be evaluated against.
Some criteria in this list might be contradicting each other or mutually exclusive.
Nevertheless they are used to understand the design space and measure trade-offs of various
solutions.

The list is presented not in the order of priority.

* Work with fallibility in mind.
  * A successful proposal should demonstrate that during the initialisation journey, how errors
    can be handled and initialisation process could be interrupted gracefully.
  * A litmus test is a demonstration of an emplacing function that returns a `Result` or an `Option`.
* Minimal surprise - drop semantics
  * There are implicit structure in Rust program semantics such as the concept of RAII.
  * When applicable, a successful proposal should demonstrate that a novice user could use intuition
    developed from the rest of the Rust language identify locations in a user code with the
    following program semantics,
    with little help from language servers or regular expression that is specially engineered with
    effort.
    * The location where the initialisation is complete.
    * The location where the variable is considered initialised.
    * The location where the drop obligation of the values is activated.
  * A litmus test should be a test on the following code templated with the respective proposals.

    ```rust
    // This notation is only to introduce a goal for emplacement.
    // Respective proposals should fill in their code with their proposed syntax.
    let x: BigStructWithThousandFields;
    // Goal 1. Initialise `field1`, whose type has a destructor
    <expr>;
    if random() {
      panic!()
    }
    // Snap quiz to the tester: what is the state of `field1` at this location?
    // Goal 2. Re-initialise `field1`
    <expr>;
    // Snap quiz to the tester: what is the state of `field1` at this location?
    // Goal 3. Initialise the other 999 `field<n>`s; abbreviation is encouraged.
    // Note that the expressions may be interleaved with `do_something()`
    // **and unpredictable re-initialisation of fields**.
    <exprs>;
    do_something();
    <exprs, possibly re-initialisations>;
    // Snap quiz to the tester: what is the state of `x` at this location? Does `x` get dropped
    // when do_something panics?
    do_something();
    // Snap quiz to the tester: what is the state of `x` at this location? Does `x` get dropped
    // when do_something panics?
    // Goal 4. When applicable, finalise the emplacement.
    <finalisation>;
    // Snap quiz to the tester: what is the state of `x` at this location?
    ```

* No or minimal `panic` in the generated code.
  * A solution should be used even in no-panic platforms.
  * A succesful proposal should demonstrate that no panics can arise from an emplacement process, or
    how a failure could be externalised.
    Also refer to the criterion `Work with fallibility in mind`.
* Panic safety.
  * A successful proposal should demonstrate that it admits panic safety- destructors are properly
    called and all live references remain valid on every possible unwind paths.
  * A litmus test would be, taking emplacement example code under a proposal, insert an explicit
    panic at each possible program point and check that the panic safety is upheld at this panic
    point.
* Support abstraction across function boundaries.
  * A successful proposal should demonstrate how the information of state of emplacement can
    propagate across function calls, which is crucial to write subroutines and coroutines that
    collectively complete an in-place initialisation.
* Composability of emplacing functions.
  * A successful proposal should demonstrate how one writes emplacing function combinators so that
    they can be used together to complete the entire emplacement operation.
    As an example, the in-place initialiser of a container like `Mutex<_>` can be viewed as
    a combinator that turns an emplacing initialiser of a type `MyStruct` into an emplacing
    initialiser of a type `Mutex<MyStruct>`.
* Good composibility with any control-flow.
  * This covers control-flow constructs such as early returns via the try `?` operator,
    coroutine suspension `yield` and `async` future suspension `await` as well as the
    `break`, `continue` keywords usable in loops
* No or minimal `unsafe`.
  * A successful proposal should demonstrate that the compiler can apply analysis to the maximal
    degree and provide maximal safety without resorting to requiring user assertions by marking
    code `unsafe`.
* Minimal changes to smart pointer and container APIs for emplacement.
  * Ideally we have seamless support by `Box` and `Vec` as a starting point.
  * A successful proposal should answer satisfiably how standard library containers can support
    emplacement, in best cases extending existing functions without breaking changes.
* Explicit syntatical signal to emplace values in-place
  * When the emplacement intention is signaled through syntax, the emplacement must take place with
    a given memory slot.
  * A successful proposal should guarantee compile error if emplacement is impossible or illegal.
* Support for blanket initialisation traits
  * A litmus test is a version of [`Zeroable` trait] under the respective proposals.
    This is a trait that is implemented on types with all-zeros as a valid bit-pattern.
  * A successful proposal should allow `Zeroable` trait to signal a specific way to fallback during
    emplacement, in case a `struct` field is left unspecified.
    In [`pin-init` crate syntax] the trait is used at a symbolic `..` struct base expression,
    so that the unmentioned fields are filled with zeros through the `Zeroable::init_zeroed()`
    method.
* Composability with various function modifiers or flavours
  * It is also loosely connected the the notion of "effects" in the following themes.
    * Fallibility, functions whose return types implements `Try` such as `Result`, `Option`
      and `Poll`
    * `const` and in association the host effect under the theme of constant-time evaluation
    * `async`, `async gen` and `gen` under the theme of coroutines
    * `Pin`ed-ness in the theme of self-referential data
    * ... and combination of all function flavours above
  * A successful proposal should demonstrate good synergy with the mentioned "effects" or at least
    prospect of future extensions to support "effects" and "modifications."
* Backward-compatibility
  * Existing functions in traits definitions are automatically emplacing without any syntactical
    rewrites.
  * A succesful proposal should allow, among all traits, `impl From`s to be automatically emplacing
    without any SemVer breaking change that warrants a major version bump across the Rust ecosystem.

## Resources

[#t-lang/in-place-init](https://rust-lang.zulipchat.com/#narrow/channel/528918-t-lang.2Fin-place-init)

[In-place initialization - Rust Project Goals](https://rust-lang.github.io/rust-project-goals/2025h2/in-place-initialization.html), Fall 2025

* [Design meeting 2025-07-30: In-place initialization - HackMD](https://hackmd.io/XXuVXH46T8StJB_y0urnYg)

[#t-lang/in-place-init > in-place initialization: RfL design wishes - rust-lang - Zulip](https://rust-lang.zulipchat.com/#narrow/channel/528918-t-lang.2Fin-place-init/topic/in-place.20initialization.3A.20RfL.20design.20wishes/with/531905430)

[`Zeroable` trait]: https://rust.docs.kernel.org/kernel/prelude/trait.Zeroable.html
[`pin-init` crate syntax]: https://rust.docs.kernel.org/pin_init/macro.pin_init.html#syntax
