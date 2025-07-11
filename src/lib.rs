//! Approximate equality.
//!
//! This crate provides mechanisms for comparing floating-point numbers and structures containing
//! floating-point numbers for *approximate equality* in the presence of rounding and measurement
//! errors.
//!
//! # Comparisons
//!
//! The [`approx_eq`] function can be used to compare two values for *approximate equality*.
//!
//! [`approx_eq`] returns a [`Comparison`], which has to be dereferenced to obtain the comparison
//! result as a [`bool`]. It can also be inverted to get the inverted result.
//!
//! ```
//! use ballpark::approx_eq;
//!
//! if !approx_eq(1.0, 1.0 + f64::EPSILON) {
//!     panic!("these are only a single `f64` apart, so they should be approximately equal");
//! }
//! if *approx_eq(10.0, 11.0) {
//!     panic!("10 and 11 are pretty far apart, so they should not be considered equal");
//! }
//! ```
//!
//! [`Comparison`] allows configuring the specific type of comparison and tolerance threshold with
//! the [`Comparison::abs`], [`Comparison::rel`], and [`Comparison::ulps`] methods.
//!
//! If none of these methods are used to customize the comparison, a *default comparison* is
//! performed, which is a [`Comparison::ulps`] comparison with a tolerance of 4 ULPs.
//!
//! # Assertions
//!
//! One of the most common use cases of this crate is to assert that two values are *almost equal*,
//! up to some tolerance value.
//! This can be done via the [`assert_approx_eq!`] and [`assert_approx_ne!`] macros, which work
//! similarly to [`assert_eq!`] and [`assert_ne!`], respectively.
//!
//! ```
//! use ballpark::assert_approx_eq;
//!
//! // Basic usage:
//! let a = 10.0;
//! let b = 1.0 / (1.0 / a);
//! assert_approx_eq!(a, b);
//!
//! // Like `assert_eq!`, it supports a custom panic message:
//! assert_approx_eq!(a, b, "inverting {} twice should give the same result", a);
//! ```
//!
//! Unlike [`assert_eq!`] and [`assert_ne!`], which evaluate to `()`, the assertion macros in
//! *ballpark* return an [`Assertion`] object that can be used to set custom comparison thresholds:
//!
//! ```
//! use ballpark::{assert_approx_eq, assert_approx_ne};
//!
//! // These values are too far away for the default comparison to consider them equal:
//! let a = 10.0;
//! let b = 10.1;
//!
//! assert_approx_ne!(a, b);
//!
//! // The values are less than 0.25 apart, so an *absolute difference* comparison with a tolerance
//! // of 0.25 will treat them as equal:
//! assert_approx_eq!(a, b).abs(0.25);
//!
//! // They are also within 1% of each other, so a relative comparison with a tolerance of 0.01
//! // also treats them as equal:
//! assert_approx_eq!(a, b).rel(0.01);
//! ```
//!
//! # Custom Types
//!
//! User-defined types can implement the [`ApproxEq`] trait to become compatible with this crate.
//! All methods of [`ApproxEq`] should forward the operation to all contained fields that
//! contribute to a type's [`PartialEq`] result and `&&` the results.
//!
//! ```
//! use ballpark::ApproxEq;
//!
//! pub struct Vec2<T> {
//!     x: T,
//!     y: T,
//! }
//!
//! impl<T: ApproxEq> ApproxEq for Vec2<T> {
//!     type Tolerance = T::Tolerance;
//!
//!     fn abs_eq(&self, other: &Self, abs_tolerance: Self::Tolerance) -> bool {
//!         self.x.abs_eq(&other.x, abs_tolerance) &&
//!             self.y.abs_eq(&other.y, abs_tolerance)
//!     }
//!
//!     fn rel_eq(&self, other: &Self, rel_tolerance: Self::Tolerance) -> bool {
//!         self.x.rel_eq(&other.x, rel_tolerance) &&
//!             self.y.rel_eq(&other.y, rel_tolerance)
//!     }
//!
//!     fn ulps_eq(&self, other: &Self, ulps_tolerance: u32) -> bool {
//!         self.x.ulps_eq(&other.x, ulps_tolerance) &&
//!             self.y.ulps_eq(&other.y, ulps_tolerance)
//!     }
//! }
//! ```
//!
//! # Cargo Features
//!
//! This library exposes the following Cargo features:
//!
//! * **`std`** (enabled by default): implements [`ApproxEq`] for some types in `libstd`.
//! * **`alloc`** (enabled by default): implements [`ApproxEq`] for data structures and collections
//!   from the `alloc` crate.
//! * **`f16`**: implements [`ApproxEq`] for Rust's unstable [`f16`] type. Requires a nightly
//!   compiler.
//! * **`f128`**: implements [`ApproxEq`] for Rust's unstable [`f128`] type. Requires a nightly
//!   compiler.
//!
//! Turning off the **`std`** feature makes this library `#![no_std]`, and with **`alloc`** also
//! turned off, it becomes usable in environments that don't have a `#[global_allocator]`.

#![cfg_attr(not(any(test, feature = "std")), no_std)]
#![cfg_attr(feature = "f16", feature(f16))]
#![cfg_attr(feature = "f128", feature(f128))]

#[doc = include_str!("../README.md")]
mod readme {}

mod impls;

#[cfg(any(test, feature = "alloc"))]
mod alloc;
#[cfg(any(test, feature = "std"))]
mod std;

#[cfg(test)]
mod tests;

use core::{
    fmt,
    ops::{self, Deref},
    panic::Location,
};

/// Implementation details used by macros. Semver-exempt. Do not use.
#[doc(hidden)]
pub mod __internal {
    use core::fmt;
    use core::panic::Location;

    use super::{ApproxEq, Assertion};

    pub use ::core;

    #[track_caller]
    pub fn assertion<'a, T, U>(
        left: &'a T,
        right: &'a U,
        assert_eq: bool,
        defused: bool,
        msg: Option<fmt::Arguments<'a>>,
    ) -> Assertion<'a, T, U>
    where
        T: ApproxEq<U> + fmt::Debug,
        U: fmt::Debug,
    {
        Assertion {
            defused,
            comp: super::Comparison::new(left, right),
            assert_eq,
            location: Location::caller(),
            msg,
        }
    }
}

/// Types that can be compared for *approximate equality*.
///
/// Compound types implementing this trait are considered *equal* if all of their fields are.
///
/// # Implementing
///
/// [`ApproxEq`] can be implemented for custom data structures by simply forwarding to the
/// implementation of all the constituent types, similar to how [`PartialEq`] would be implemented.
/// However, it does not support structures that mix different primitive types like [`f32`] and
/// [`f64`].
pub trait ApproxEq<Rhs: ?Sized = Self> {
    /// Type representing the tolerance for absolute and relative comparisons.
    ///
    /// This is almost always either [`f32`] or [`f64`], depending on which one is the underlying
    /// primitive type being compared.
    type Tolerance: Copy;

    /// Performs an *absolute comparison* of `self` and `other`.
    ///
    /// If the absolute difference of the compared values is less than or equal to `abs`, the values
    /// are considered to be equal.
    ///
    /// # Panics
    ///
    /// It is an error to call this method with a negative or NaN `abs_tolerance`. Implementations
    /// are allowed to panic when that happens.
    fn abs_eq(&self, other: &Rhs, abs_tolerance: Self::Tolerance) -> bool;

    /// Performs a *relative comparison* of `self` and `other`.
    ///
    /// If the absolute difference of the compared values is less than or equal to the largest of
    /// the two values times `rel_tolerance`, the values are considered to be equal.
    ///
    /// # Panics
    ///
    /// It is an error to call this method with a negative or NaN `rel_tolerance`. Implementations
    /// are allowed to panic when that happens.
    fn rel_eq(&self, other: &Rhs, rel_tolerance: Self::Tolerance) -> bool;

    /// Performs a comparison of `self` and `other` by counting the number of
    /// [*units in the last place*] (ULPs) between the values.
    ///
    /// If the distance in ULPs between the two values is at most `ulps_tolerance`, they are
    /// considered to be equal.
    /// For purposes of the comparison, the distance between `-0.0` and `+0.0` is considered to be
    /// 0.
    ///
    /// `NaN` is never considered equal to anything.
    ///
    /// # [`u32`] for tolerances
    ///
    /// This type of comparison uses a fixed [`u32`] tolerance, even though an [`f64`] (or larger
    /// type) can have more than 2³² ULPs between different values.
    /// The rationale for using [`u32`] regardless of the underlying floating-point type is:
    ///
    /// * It keeps the [`ApproxEq`] trait and its implementations simpler, by not requiring another
    ///   associated type.
    /// * It makes usage from generic code easier, by not requiring such users to constrain this
    ///   type.
    /// * A difference of billions of ULPs is *much* better expressed with the other types of
    ///   comparisons.
    ///
    /// The ULP-based comparison is intended for computations that accumulate small rounding errors
    /// of a few ULPs, not for computations that are subject to [catastrophic cancellation] or that
    /// contain significant amount of sensor noise or other interference and uncertainty.
    ///
    /// [*units in the last place*]: https://en.wikipedia.org/wiki/Unit_in_the_last_place
    /// [catastrophic cancellation]: https://en.wikipedia.org/wiki/Catastrophic_cancellation
    fn ulps_eq(&self, other: &Rhs, ulps_tolerance: u32) -> bool;
}

/// Tolerance in ULPs used by the *default comparison*.
///
/// The same value is used for all floating-point types, since the accuracy of computations is
/// nearly always constant when measured in ULPs, no matter what floating-point type ([`f32`] or
/// [`f64`]) is used.
const DEFAULT_TOLERANCE_ULPS: u32 = 4;

/// Compares two values using approximate equality.
///
/// Returns a [`Comparison`], which allows specifying the tolerance values to use and the exact type
/// of comparison to perform.
/// [`Comparison`] can either be dereferenced (`*`) to obtain its result as a [`bool`], or inverted
/// (`!`) to get the negated result.
///
/// If none of [`Comparison::abs`], [`Comparison::rel`] or [`Comparison::ulps`] is called to request
/// a specific type of comparison, a *default comparison* is performed.
/// This *default comparison* is a [`Comparison::ulps`] comparison with a tolerance of 4 ULPs.
///
/// If more than one comparison is requested, the values will be considered equal if *any*
/// comparison considers them equal (ie. the results are ORed together).
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// # use ballpark::*;
/// # let input = 0.0;
/// if *approx_eq(input, 0.0) {
///     println!("roughly nothing!");
/// }
/// if !approx_eq(input, 0.0) {
///     println!("roughly something!");
/// }
/// ```
///
/// Custom tolerance:
///
/// ```
/// # use ballpark::*;
/// if *approx_eq(10.0, 10.5).abs(0.6) {
///     println!(r"close enough ¯\_(ツ)_/¯");
/// }
/// ```
pub fn approx_eq<T, U>(left: T, right: U) -> Comparison<T, U>
where
    T: ApproxEq<U>,
{
    Comparison::new(left, right)
}

/// Comparator for approximate equality of values.
///
/// Returned by the [`approx_eq`] function.
///
/// This type can be dereferenced with `*comp` to obtain the result of the comparison. It can also
/// be negated via `!comp` to obtain the negated result.
///
/// If no method is called to configure the specific type of comparison to perform, a *default
/// comparison* will be performed.
/// This *default comparison* is a [`Comparison::ulps`] comparison with a tolerance of 4 ULPs.
#[derive(Clone, Copy)]
pub struct Comparison<T, U>
where
    T: ApproxEq<U>,
{
    left: T,
    right: U,
    abs: Option<T::Tolerance>,
    rel: Option<T::Tolerance>,
    ulps: Option<u32>,
}

impl<T, U> Deref for Comparison<T, U>
where
    T: ApproxEq<U>,
{
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        match self.get() {
            true => &true,
            false => &false,
        }
    }
}

/// [`Comparison`] can be inverted to get the opposite result compared to deferencing.
impl<T, U> ops::Not for Comparison<T, U>
where
    T: ApproxEq<U>,
{
    type Output = bool;

    fn not(self) -> Self::Output {
        !self.get()
    }
}

impl<T, U> Comparison<T, U>
where
    T: ApproxEq<U>,
{
    fn new(left: T, right: U) -> Self {
        Self {
            left,
            right,
            abs: None,
            rel: None,
            ulps: None,
        }
    }

    /// Perform an *absolute comparison* of the values with the given tolerance.
    ///
    /// If the absolute difference of the compared values is less than or equal to `abs`, the values
    /// are considered to be equal.
    ///
    /// This type of comparison is typically a good choice when comparing values that are relatively
    /// close to zero but have a large amount of relative error (for example due to
    /// [catastrophic cancellation]).
    ///
    /// It can also be used "ad-hoc", when a more rigid error model is deemed unnecessary and a
    /// simple estimate of the error is sufficient (a *ballpark* estimate, if you will).
    ///
    /// [catastrophic cancellation]: https://en.wikipedia.org/wiki/Catastrophic_cancellation
    ///
    /// # Examples
    ///
    /// ```
    /// use ballpark::approx_eq;
    ///
    /// if !approx_eq(1.0, 1.25).abs(0.5) {
    ///     panic!("should be equal: values are only 0.25 apart, tolerance is 0.5");
    /// }
    /// if *approx_eq(1.0, 1.25).abs(0.1) {
    ///     panic!("should not be equal: values are 0.25 apart, tolerance is 0.1");
    /// }
    /// ```
    pub fn abs(mut self, abs: T::Tolerance) -> Self {
        self.abs = Some(abs);
        self
    }

    /// Perform a *relative comparison* of the values with the given tolerance.
    ///
    /// If the absolute difference of the compared values is less than or equal to the largest of
    /// the two values times `rel`, the values are considered to be equal.
    ///
    /// This type of comparison is a good choice for numbers that aren't very close to zero, but
    /// have a larger error than what a [`ulps`][Comparison::ulps] comparison can reasonably be used
    /// for.
    /// For numbers close to zero, a very large relative tolerance might be required and
    /// [`Comparison::abs`] might be a better choice.
    ///
    /// # Examples
    ///
    /// ```
    /// use ballpark::approx_eq;
    ///
    /// // 1% of 100.0 is 1.0, so this comparison is identical to an absolute comparison with a
    /// // tolerance value of 1.0.
    /// if !approx_eq(99.0, 100.0).rel(0.01) {
    ///     panic!("should be considered equal");
    /// }
    /// ```
    pub fn rel(mut self, rel: T::Tolerance) -> Self {
        self.rel = Some(rel);
        self
    }

    /// Perform a comparison by counting the number of [*units in the last place*] (ULPs) between
    /// the values.
    ///
    /// If the distance in ULPs between the two values is at most `ulps`, they are considered to
    /// be equal.
    ///
    /// This type of comparison has the nice property of respecting the uneven distribution of
    /// floating-point numbers: for example, floats are much denser between 1.0 and 2.0 than between
    /// 1001.0 and 1002.0.
    ///
    /// [*units in the last place*]: https://en.wikipedia.org/wiki/Unit_in_the_last_place
    ///
    /// A ULPs comparison is used as the default comparison, if none of the [`abs`], [`rel`] and
    /// [`ulps`] methods are called by the user. The default comparison uses a tolerance of 4 ULPs.
    ///
    /// [`abs`]: Comparison::abs
    /// [`rel`]: Comparison::rel
    /// [`ulps`]: Comparison::ulps
    ///
    /// # Examples
    ///
    /// ```
    /// use ballpark::approx_eq;
    ///
    /// // `1.0` and `1.0 + epsilon` are always precisely one ULP apart.
    /// if !approx_eq(1.0, 1.0 + f64::EPSILON).ulps(1) {
    ///     panic!("should be considered equal");
    /// }
    /// ```
    pub fn ulps(mut self, ulps: u32) -> Self {
        self.ulps = Some(ulps);
        self
    }

    fn get(&self) -> bool {
        let &Comparison {
            abs, rel, mut ulps, ..
        } = self;

        if let (None, None, None) = (abs, rel, ulps) {
            ulps = Some(DEFAULT_TOLERANCE_ULPS);
        }

        if let Some(abs) = abs {
            if T::abs_eq(&self.left, &self.right, abs) {
                return true;
            }
        }
        if let Some(rel) = rel {
            if T::rel_eq(&self.left, &self.right, rel) {
                return true;
            }
        }
        if let Some(ulps) = ulps {
            if T::ulps_eq(&self.left, &self.right, ulps) {
                return true;
            }
        }

        false
    }
}

/// Assertion guard returned by [`assert_approx_eq!`][crate::assert_approx_eq]
/// and [`assert_approx_ne!`][crate::assert_approx_ne].
///
/// This type will check the assertion when dropped, and has methods that allow configuring the
/// comparison method and tolerances to use. It supports 3 ways of comparing values that can be
/// enabled by calling the appropriate methods:
///
/// - [`Assertion::abs`] for comparing the value's *absolute difference* via
///   [`ApproxEq::abs_eq`].
/// - [`Assertion::rel`] for comparing the value's *relative difference* via
///   [`ApproxEq::rel_eq`].
/// - [`Assertion::ulps`] for comparing the values by checking how many other values can fit between
///   them via [`ApproxEq::ulps_eq`].
///
/// If more than one of these methods is called, the values will be considered equal if *any*
/// comparison considers them equal (ie. the results are ORed together).
///
/// # Panics in [`Drop`] and `#![no_std]`
///
/// If a thread is panicking, and another panic happens in a [`Drop`] implementation, the process
/// will be terminated.
///
/// To prevent this, the [`Drop`] implementation of [`Assertion`] will check
/// [`thread::panicking`][::std::thread::panicking] to see if the thread is currently unwinding,
/// but **only if the `std` feature is enabled**, since Rust offers no way to detect whether this
/// symbol is available otherwise.
///
/// Note that this is not really a problem when this library is used properly:
/// When there is a simple sequence of [`assert_approx_eq!`] family macro invocations, there will
/// only ever be a single [`Assertion`] object that gets constructed, checked, and discarded before
/// the next one is created.
///
/// Therefore, it is best to follow a few simple rules: Don't store [`Assertion`]s in local
/// variables, don't pass them as arguments, and don't put them in compound types like `struct`s or
/// tuples.
///
/// These consideration do not apply to [`Comparison`], since that does not have a destructor.
pub struct Assertion<'a, T, U = T>
where
    T: ApproxEq<U> + fmt::Debug,
    U: fmt::Debug,
{
    defused: bool,
    comp: Comparison<&'a T, &'a U>,
    assert_eq: bool,
    location: &'static Location<'static>,
    msg: Option<fmt::Arguments<'a>>,
}

impl<T, U> Assertion<'_, T, U>
where
    T: ApproxEq<U> + fmt::Debug,
    U: fmt::Debug,
{
    /// Perform an *absolute comparison* of the values with the given tolerance.
    ///
    /// See [`Comparison::abs`] for information on when to pick this type of comparison.
    ///
    /// # Examples
    ///
    /// ```
    /// use ballpark::{assert_approx_eq, assert_approx_ne};
    ///
    /// // The absolute difference between -0.25 and 0.5 is 0.75, so a tolerance of 0.75 or higher
    /// // will make them compare "approximately equal"...
    /// assert_approx_eq!(-0.25, 0.5).abs(0.75);
    /// assert_approx_eq!(0.5, -0.25).abs(0.75);
    /// assert_approx_eq!(-0.25, 0.5).abs(1.0);
    ///
    /// // ...but tolerances below 0.75 won't
    /// assert_approx_ne!(0.5, -0.25).abs(0.74);
    /// ```
    pub fn abs(mut self, abs: T::Tolerance) -> Self {
        self.comp = self.comp.abs(abs);
        self
    }

    /// Perform a *relative comparison* of the values with the given tolerance.
    ///
    /// See [`Comparison::rel`] for information on when to pick this type of comparison.
    ///
    /// # Examples
    ///
    /// ```
    /// use ballpark::{assert_approx_eq, assert_approx_ne};
    ///
    /// // 1% of 100.0 is 1.0, so this comparison is identical to an absolute comparison with a
    /// // tolerance value of 1.0.
    /// assert_approx_eq!(99.0, 100.0).rel(0.01);
    /// assert_approx_eq!(99.0, 100.0).abs(1.0);
    ///
    /// assert_approx_ne!(99.0, 100.0).rel(0.0099);
    /// assert_approx_ne!(99.0, 100.0).abs(0.99);
    /// ```
    pub fn rel(mut self, rel: T::Tolerance) -> Self {
        self.comp = self.comp.rel(rel);
        self
    }

    /// Perform a comparison by counting the number of *units in the last place* (ULPs) between the
    /// values.
    ///
    /// See [`Comparison::ulps`] for information on when to pick this type of comparison.
    ///
    /// # Examples
    ///
    /// ```
    /// use ballpark::{assert_approx_eq, assert_approx_ne};
    ///
    /// // `1.0` and `1.0 + epsilon` are always precisely one ULP apart.
    /// assert_approx_eq!(1.0, 1.0 + f64::EPSILON).ulps(1);
    ///
    /// // Infamously, 0.1 + 0.2 == 0.30000000000000004 when computed with doubles, off by one ULP
    /// // from the double closest to 0.3.
    /// assert_ne!(0.1 + 0.2, 0.3);
    /// assert_eq!(0.1 + 0.2, 0.30000000000000004);
    /// assert_approx_eq!(0.1 + 0.2, 0.3).ulps(1);
    ///
    /// // `inf` comes right after `f32::MAX`.
    /// assert_approx_eq!(f32::MAX, f32::INFINITY).ulps(1);
    /// ```
    pub fn ulps(mut self, ulps: u32) -> Self {
        self.comp = self.comp.ulps(ulps);
        self
    }

    /// Checks the assertions, panicking if it fails.
    ///
    /// It is typically not necessary to call this, as the [`Assertion`] will check itself when
    /// dropped. This method may result in a better panic location than dropping the [`Assertion`].
    ///
    /// If the assertion was created by [`debug_assert_approx_eq!`] or [`debug_assert_approx_ne!`],
    /// and debug assertions are disabled, neither this method nor dropping the [`Assertion`] will
    /// perform the assertion.
    #[track_caller]
    pub fn check(mut self) {
        self.check_inner();
    }

    /// Discards the [`Assertion`] without checking it.
    ///
    /// By default, [`Assertion`]s are checked when dropped, panicking if the assertion fails.
    /// This method can be called to discard an [`Assertion`] without doing so.
    pub fn ignore(mut self) {
        self.defused = true;
    }

    #[track_caller]
    fn check_inner(&mut self) {
        if self.defused {
            return;
        }

        self.defused = true;
        let equal = *self.comp;
        if equal != self.assert_eq {
            assert_failed_inner(
                self.comp.left,
                self.comp.right,
                self.assert_eq,
                self.location,
                self.msg,
            );
        }
    }
}

impl<T, U> Drop for Assertion<'_, T, U>
where
    T: ApproxEq<U> + fmt::Debug,
    U: fmt::Debug,
{
    // FIXME: the largest UX issue is that `#[track_caller]` does not work correctly on destructors
    // (the location of `ptr::drop_in_place` is blamed instead of the user code dropping the value)
    // https://github.com/rust-lang/rust/issues/116942
    //#[track_caller]
    fn drop(&mut self) {
        // NOTE: This check is only present when the `std` feature is enabled; it is best-effort.
        // `Assertion`s should typically not be stored as local variables in the first place.
        #[cfg(feature = "std")]
        if ::std::thread::panicking() {
            return;
        }

        /*

        If the panic output takes you here, you've probably clicked on the wrong location.

        `#[track_caller]` doesn't work correctly on `drop`, so we manually print the correct
        location of the assertion.

        */
        self.check_inner();
    }
}

#[track_caller]
fn assert_failed_inner(
    left: &dyn fmt::Debug,
    right: &dyn fmt::Debug,
    assert_eq: bool,
    location: &Location<'_>,
    args: Option<fmt::Arguments<'_>>,
) -> ! {
    let op = match assert_eq {
        true => "==",
        false => "!=",
    };
    match args {
        Some(args) => panic!(
            r#"assertion `left {op} right` failed at {location}: {args}
  left: {left:?}
 right: {right:?}"#
        ),
        None => panic!(
            r#"assertion `left {op} right` failed at {location}
  left: {left:?}
 right: {right:?}"#
        ),
    }
}

/// Asserts that two expressions are approximately equal to each other (using [`ApproxEq`]).
///
/// This macro functions identically to [`assert_eq!`], except in that it uses the [`ApproxEq`]
/// trait to perform an approximate comparison.
///
/// The macro expands to an expression that evaluates to an [`Assertion`] object that can be used to
/// configure the exact type of comparison, as well as the tolerance values to use:
///
/// - [`Assertion::abs`] compares the value's *absolute difference* via [`ApproxEq::abs_eq`].
/// - [`Assertion::rel`] compares the value's *relative difference* via [`ApproxEq::rel_eq`].
/// - [`Assertion::ulps`] compares the values by checking how many other values can fit between
///   them via [`ApproxEq::ulps_eq`].
///
/// Also see [`assert_approx_ne!`][crate::assert_approx_ne].
///
/// # Examples
///
/// Default approximate comparison:
///
/// ```
/// # use ballpark::*;
/// let one = (0..10).fold(0.0, |acc, _| acc + 0.1);
/// assert_approx_eq!(one, 1.0);
/// ```
///
/// Perform absolute and relative comparisons with custom tolerance values:
///
/// ```
/// # use ballpark::*;
/// assert_approx_eq!(100.0, 99.0).abs(1.0);
/// assert_approx_eq!(100.0, 99.0).rel(0.01);
/// ```
///
/// Compare values via ULPs, based on the number of floats that fit between them:
///
/// ```
/// # use ballpark::*;
/// assert_approx_eq!(1.0, 1.0 + f64::EPSILON).ulps(1);
/// ```
#[macro_export]
macro_rules! assert_approx_eq {
    ($lhs:expr, $rhs:expr $(,)?) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            true,  /* eq */
            false, /* defused */
            $crate::__internal::core::option::Option::None,
        )
    };
    ($lhs:expr, $rhs:expr, $($arg:tt)+) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            true,  /* eq */
            false, /* defused */
            $crate::__internal::core::option::Option::Some($crate::__internal::core::format_args!($($arg)+))
        )
    };
}

/// Asserts that two expressions are *not* approximately equal to each other (using [`ApproxEq`]).
///
/// This macro functions identically to [`assert_ne!`], except in that it uses the [`ApproxEq`]
/// trait to perform an approximate comparison, and returns an [`Assertion`] that can be used to
/// configure the exact type of comparison, as well as the tolerance values to use.
///
/// Also see [`assert_approx_eq!`].
///
/// # Examples
///
/// Perform absolute and relative comparisons with custom tolerance values:
///
/// ```
/// # use ballpark::*;
/// assert_approx_ne!(100.0, 99.0).abs(0.5);
/// assert_approx_ne!(100.0, 99.0).rel(0.005);
/// ```
///
/// Compare values via ULPs, based on the number of floats that fit between them:
///
/// ```
/// # use ballpark::*;
/// assert_approx_ne!(1.0, 1.0 + f64::EPSILON + f64::EPSILON).ulps(1);
/// ```
#[macro_export]
macro_rules! assert_approx_ne {
    ($lhs:expr, $rhs:expr $(,)?) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            false, /* eq */
            false, /* defused */
            $crate::__internal::core::option::Option::None
        )
    };
    ($lhs:expr, $rhs:expr, $($arg:tt)+) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            false, /* eq */
            false, /* defused */
            $crate::__internal::core::option::Option::Some($crate::__internal::core::format_args!($($arg)+))
        )
    };
}

/// Debug-asserts that two expressions are approximately equal to each other.
///
/// This macro functions identically to [`assert_approx_eq!`], except the assertion is only checked
/// when debug assertions are enabled.
#[macro_export]
macro_rules! debug_assert_approx_eq {
    ($lhs:expr, $rhs:expr $(,)?) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            true,  /* eq */
            !cfg!(debug_assertions), /* defused */
            $crate::__internal::core::option::Option::None,
        )
    };
    ($lhs:expr, $rhs:expr, $($arg:tt)+) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            true,  /* eq */
            !cfg!(debug_assertions), /* defused */
            $crate::__internal::core::option::Option::Some($crate::__internal::core::format_args!($($arg)+))
        )
    };
}

/// Debug-asserts that two expressions are *not* approximately equal to each other.
///
/// This macro functions identically to [`assert_approx_ne!`], except the assertion is only checked
/// when debug assertions are enabled.
#[macro_export]
macro_rules! debug_assert_approx_ne {
    ($lhs:expr, $rhs:expr $(,)?) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            false, /* eq */
            !cfg!(debug_assertions), /* defused */
            $crate::__internal::core::option::Option::None
        )
    };
    ($lhs:expr, $rhs:expr, $($arg:tt)+) => {
        $crate::__internal::assertion(
            &$lhs,
            &$rhs,
            false, /* eq */
            !cfg!(debug_assertions), /* defused */
            $crate::__internal::core::option::Option::Some($crate::__internal::core::format_args!($($arg)+))
        )
    };
}
