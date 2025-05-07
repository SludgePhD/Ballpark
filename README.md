# *Ballpark* – Approximate Comparisons

This crate provides approximate equality comparisons for floating-point values
and structures composed of floating-point values.

## Features

- Lightweight: no mandatory dependencies, fast compiles by default.
- Stable: no unstable public dependencies (even optional ones).
  - The goal is to reach 1.0 Soon™ and stay there ideally forever.
- Ergonomic: familiar syntax with IDE support.
- Flexible: `#![no_std]` support, no `alloc` needed (just disable the default features).

## Non-Features

This crate does ***not*** aim to provide the following features:

- Comparing integers.
  - If the integers are converted from floats, try to keep the data as floats instead.
  - Converting the integers to floats and operating on those can also be a useful solution.
  - Otherwise, some domain-specific solution is needed, and this crate would likely not help anyways.
- Approximate comparison of image data, or other data subject to perceptive biases.
  - You *can* use this library as long as the data consists of floats or can be reinterpreted as such, but a domain-specific algorithm will likely be more useful.
  - For image data, you can use a perceptual comparison algorithm like [ꟻLIP] instead.
  - For audio data, computing the SNR and asserting that the noise is inaudible is a good starting point.
- Implementations for types from third-party crates.
  - This crate only enables approximate comparisons for types in the core language and standard library.
  - Instead, external crates should depend on `ballpark` themselves and provide their own implementations (potentially behind a Cargo feature).
  - This avoids adding potentially dozens of optional unstable dependencies to this crate and pushing all the maintenance burden on me.

[ꟻLIP]: https://github.com/gfx-rs/nv-flip-rs

## Examples

Perform assertions with the `assert_approx_eq!` macro:

```rust
use ballpark::assert_approx_eq;

// Infamously, 0.1 + 0.2 == 0.30000000000000004 when using double precision.
assert_ne!(0.1 + 0.2, 0.3);

// But they are *approximately* equal.
assert_approx_eq!(0.1 + 0.2, 0.3);

// Like `assert_eq!`, you can provide your own panic message:
let tolerance = 0.25;
assert_approx_eq!(1.0, 1.1, "not within tolerance of {tolerance}").abs(tolerance);

// The type of comparison, and threshold value, can be configured by calling methods on
// the returned assertion guard.
// 3 types of comparison are supported:

// Absolute difference threshold: the difference between the values must be at most X
assert_approx_eq!(0.9, 1.0).abs(0.1);

// Relative difference threshold: the difference must be within a fraction of the bigger input.
assert_approx_eq!(99.0, 100.0).rel(0.01);  // Difference must be within 1% of the bigger input.

// Units in the last place: there must be at most N distinct float values between the numbers.
assert_approx_eq!(1.0, 1.0 + f32::EPSILON).ulps(1); // 1 float apart

// If none of these methods are called, a "default" comparison is performed: a `ulps` comparison
// with a threshold of 4 ULPs.
```

Perform comparisons with `approx_eq`:

```rust
use ballpark::approx_eq;

// The `approx_eq` function can be used to perform a comparison *without* an assertion:
if !approx_eq(1.0, 1.0) {
    eprintln!("something's wrong!");
    std::process::abort();
}
let input = 1e-8;
if *approx_eq(input, 0.0).abs(1e-7) {
    return; // close enough
}
```

For full API documentation, please refer to <https://docs.rs/ballpark>.

## Rust Support

This library targets the latest Rust version.

Older Rust versions are supported by equally older versions of this crate. For example, to use a
version of Rust that was succeeded 6 months ago, you'd also use an at least 6 month old version of
this library.

Compatibility with older Rust versions may be provided on a best-effort basis.
