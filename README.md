# *Ballpark* – Approximate Comparisons

This crate provides approximate equality comparisons for floating-point values
and structures composed of floating-point values.

```rust
use ballpark::assert_approx_eq;

// Infamously, 0.1 + 0.2 == 0.30000000000000004 when using double precision.
assert_ne!(0.1 + 0.2, 0.3);

// But they are *approximately* equal.
assert_approx_eq!(0.1 + 0.2, 0.3);

// In fact, the result is within one unit in the last place (ULP) of the correct one:
assert_approx_eq!(0.1 + 0.2, 0.3).ulps(1);
```

Refer to the [API Documentation] for more examples like these.

[API Documentation]: https://docs.rs/ballpark

## Features

- Lightweight: no mandatory dependencies, fast compiles by default.
- Stable: no unstable public dependencies (even optional ones).
  - The goal is to reach 1.0 Soon™ and stay there ideally forever.
- Ergonomic: familiar syntax with good IDE support.
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

## Rust Support

This library targets the latest Rust version.

Older Rust versions are supported by equally older versions of this crate. For example, to use a
version of Rust that was succeeded 6 months ago, you'd also use an at least 6 month old version of
this library.

Compatibility with older Rust versions may be provided on a best-effort basis.
