//! Implementations for `libcore` types and builtin types.

use core::cell::{Cell, OnceCell, RefCell};

use super::ApproxEq;

macro_rules! base_impls {
    ($($float:ty),+) => { $(
        impl ApproxEq for $float {
            type Tolerance = Self;

            fn abs_eq(&self, other: &Self, abs_tolerance: Self::Tolerance) -> bool {
                assert!(abs_tolerance >= 0.0, "comparison tolerance must be non-negative and not NaN");

                if self.is_nan() || other.is_nan() {
                    // Ensure that `nan != X` and `X != nan`.
                    return false;
                }
                if !self.is_finite() && !other.is_finite() && self == other {
                    // Ensure that `inf == inf` and `-inf == -inf`.
                    return true;
                }

                let diff = (self - other).abs();
                diff <= abs_tolerance
            }

            fn rel_eq(&self, other: &Self, rel_tolerance: Self::Tolerance) -> bool {
                assert!(rel_tolerance >= 0.0, "comparison tolerance must be non-negative and not NaN");

                if self.is_nan() || other.is_nan() {
                    // Ensure that `nan != X` and `X != nan`.
                    return false;
                }
                if !self.is_finite() && !other.is_finite() && self == other {
                    // Ensure that `inf == inf` and `-inf == -inf`.
                    return true;
                }

                let abs_diff = (self - other).abs();
                let abs_self = self.abs();
                let abs_other = other.abs();
                let largest = Self::max(abs_self, abs_other);

                abs_diff <= largest * rel_tolerance
            }

            #[allow(irrefutable_let_patterns)]
            fn ulps_eq(&self, other: &Self, ulps_tolerance: u32) -> bool {
                if self.is_nan() || other.is_nan() {
                    // Ensure that `nan != X` and `X != nan`.
                    return false;
                }

                // Compute the distances to 0.0
                let self_diff = self.abs().to_bits().abs_diff(0);
                let other_diff = other.abs().to_bits().abs_diff(0);

                let diff = if self.is_sign_negative() == other.is_sign_negative() {
                    // If both values have the same sign, the distances to 0.0 cancel out.
                    self_diff.abs_diff(other_diff)
                } else {
                    // If the values have opposing signs, the distances to 0.0 add up.
                    match self_diff.checked_add(other_diff) {
                        Some(diff) => diff,
                        None => return false,
                    }
                };

                let Ok(diff) = u32::try_from(diff) else {
                    return false;
                };
                diff <= ulps_tolerance
            }
        }
    )+ };
}

base_impls!(f32, f64);

#[cfg(feature = "f16")]
base_impls!(f16);

#[cfg(feature = "f128")]
base_impls!(f128);

////////////////////////////////////
// Implementations for References //
////////////////////////////////////

impl<T: ApproxEq<U> + ?Sized, U: ?Sized> ApproxEq<&U> for &T {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &&U, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(self, *other, abs_tolerance)
    }

    fn rel_eq(&self, other: &&U, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(self, *other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &&U, ulps_tolerance: u32) -> bool {
        T::ulps_eq(self, *other, ulps_tolerance)
    }
}

impl<T: ApproxEq<U> + ?Sized, U: ?Sized> ApproxEq<&U> for &mut T {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &&U, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(self, *other, abs_tolerance)
    }

    fn rel_eq(&self, other: &&U, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(self, *other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &&U, ulps_tolerance: u32) -> bool {
        T::ulps_eq(self, *other, ulps_tolerance)
    }
}

impl<T: ApproxEq<U> + ?Sized, U: ?Sized> ApproxEq<&mut U> for &T {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &&mut U, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(self, *other, abs_tolerance)
    }

    fn rel_eq(&self, other: &&mut U, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(self, *other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &&mut U, ulps_tolerance: u32) -> bool {
        T::ulps_eq(self, *other, ulps_tolerance)
    }
}

impl<T: ApproxEq<U> + ?Sized, U: ?Sized> ApproxEq<&mut U> for &mut T {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &&mut U, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(self, *other, abs_tolerance)
    }

    fn rel_eq(&self, other: &&mut U, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(self, *other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &&mut U, ulps_tolerance: u32) -> bool {
        T::ulps_eq(self, *other, ulps_tolerance)
    }
}

///////////////////////////////////////////
// Implementations for Slices and Arrays //
///////////////////////////////////////////

impl<T: ApproxEq<U>, U> ApproxEq<[U]> for [T] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &[U], abs_tolerance: Self::Tolerance) -> bool {
        for (a, b) in self.iter().zip(other) {
            if !T::abs_eq(a, b, abs_tolerance) {
                return false;
            }
        }
        true
    }

    fn rel_eq(&self, other: &[U], rel_tolerance: Self::Tolerance) -> bool {
        for (a, b) in self.iter().zip(other) {
            if !T::rel_eq(a, b, rel_tolerance) {
                return false;
            }
        }
        true
    }

    fn ulps_eq(&self, other: &[U], ulps_tolerance: u32) -> bool {
        for (a, b) in self.iter().zip(other) {
            if !T::ulps_eq(a, b, ulps_tolerance) {
                return false;
            }
        }
        true
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<&[U]> for [T; N] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &&[U], abs_tolerance: Self::Tolerance) -> bool {
        self.as_slice().abs_eq(*other, abs_tolerance)
    }

    fn rel_eq(&self, other: &&[U], rel_tolerance: Self::Tolerance) -> bool {
        self.as_slice().rel_eq(*other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &&[U], ulps_tolerance: u32) -> bool {
        self.as_slice().ulps_eq(*other, ulps_tolerance)
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<&mut [U]> for [T; N] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &&mut [U], abs_tolerance: Self::Tolerance) -> bool {
        self.as_slice().abs_eq(*other, abs_tolerance)
    }

    fn rel_eq(&self, other: &&mut [U], rel_tolerance: Self::Tolerance) -> bool {
        self.as_slice().rel_eq(*other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &&mut [U], ulps_tolerance: u32) -> bool {
        self.as_slice().ulps_eq(*other, ulps_tolerance)
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<[U; N]> for &[T] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &[U; N], abs_tolerance: Self::Tolerance) -> bool {
        (**self).abs_eq(other.as_slice(), abs_tolerance)
    }

    fn rel_eq(&self, other: &[U; N], rel_tolerance: Self::Tolerance) -> bool {
        (**self).rel_eq(other.as_slice(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &[U; N], ulps_tolerance: u32) -> bool {
        (**self).ulps_eq(other.as_slice(), ulps_tolerance)
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<[U; N]> for &mut [T] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &[U; N], abs_tolerance: Self::Tolerance) -> bool {
        (**self).abs_eq(other.as_slice(), abs_tolerance)
    }

    fn rel_eq(&self, other: &[U; N], rel_tolerance: Self::Tolerance) -> bool {
        (**self).rel_eq(other.as_slice(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &[U; N], ulps_tolerance: u32) -> bool {
        (**self).ulps_eq(other.as_slice(), ulps_tolerance)
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<[U; N]> for [T] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &[U; N], abs_tolerance: Self::Tolerance) -> bool {
        (*self).abs_eq(other.as_slice(), abs_tolerance)
    }

    fn rel_eq(&self, other: &[U; N], rel_tolerance: Self::Tolerance) -> bool {
        (*self).rel_eq(other.as_slice(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &[U; N], ulps_tolerance: u32) -> bool {
        (*self).ulps_eq(other.as_slice(), ulps_tolerance)
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<[U; N]> for [T; N] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &[U; N], abs_tolerance: Self::Tolerance) -> bool {
        self.as_slice().abs_eq(other.as_slice(), abs_tolerance)
    }

    fn rel_eq(&self, other: &[U; N], rel_tolerance: Self::Tolerance) -> bool {
        self.as_slice().rel_eq(other.as_slice(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &[U; N], ulps_tolerance: u32) -> bool {
        self.as_slice().ulps_eq(other.as_slice(), ulps_tolerance)
    }
}

impl<T: ApproxEq<U>, U, const N: usize> ApproxEq<[U]> for [T; N] {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &[U], abs_tolerance: Self::Tolerance) -> bool {
        self.as_slice().abs_eq(other, abs_tolerance)
    }

    fn rel_eq(&self, other: &[U], rel_tolerance: Self::Tolerance) -> bool {
        self.as_slice().rel_eq(other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &[U], ulps_tolerance: u32) -> bool {
        self.as_slice().ulps_eq(other, ulps_tolerance)
    }
}

////////////////////////////////
// Implementations for Tuples //
////////////////////////////////

macro_rules! last_type {
    ($a:ident,) => { $a };
    ($a:ident, $($rest_a:ident,)+) => { last_type!($($rest_a,)+) };
}

macro_rules! tuple_impls {
    ($T:ident $idx1:tt) => {
        tuple_impls!(@impl $T $idx1);
    };
    ($T:ident $idx1:tt $( $U:ident $idx2:tt )+) => {
        tuple_impls!($($U $idx2)+);
        tuple_impls!(@impl $T $idx1 $($U $idx2)+);
    };
    (@impl $($T:ident $idx:tt)+) => {
        impl<TOL: Copy, $($T: ApproxEq<Tolerance = TOL>),+> ApproxEq for ($($T,)+)
        where
            last_type!($($T,)+): ?Sized
        {
            type Tolerance = TOL;

            fn abs_eq(&self, other: &( $($T,)+ ), abs_tolerance: Self::Tolerance) -> bool {
                $( ApproxEq::abs_eq(&self.$idx, &other.$idx, abs_tolerance) )&&+
            }

            fn rel_eq(&self, other: &( $($T,)+ ), rel_tolerance: Self::Tolerance) -> bool {
                $( ApproxEq::rel_eq(&self.$idx, &other.$idx, rel_tolerance) )&&+
            }

            fn ulps_eq(&self, other: &( $($T,)+ ), ulps_tolerance: u32) -> bool {
                $( ApproxEq::ulps_eq(&self.$idx, &other.$idx, ulps_tolerance) )&&+
            }
        }
    };
}
tuple_impls!(E 11 D 10 C 9 B 8 A 7 Z 6 Y 5 X 4 W 3 V 2 U 1 T 0);

///////////////////////////////////////
// Implementations for libcore types //
///////////////////////////////////////

impl<T: ApproxEq + Copy> ApproxEq for Cell<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Cell<T>, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(&self.get(), &other.get(), abs_tolerance)
    }

    fn rel_eq(&self, other: &Cell<T>, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(&self.get(), &other.get(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &Cell<T>, ulps_tolerance: u32) -> bool {
        T::ulps_eq(&self.get(), &other.get(), ulps_tolerance)
    }
}

impl<T: ApproxEq + ?Sized> ApproxEq for RefCell<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &RefCell<T>, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(&*self.borrow(), &*other.borrow(), abs_tolerance)
    }

    fn rel_eq(&self, other: &RefCell<T>, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(&*self.borrow(), &*other.borrow(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &RefCell<T>, ulps_tolerance: u32) -> bool {
        T::ulps_eq(&*self.borrow(), &*other.borrow(), ulps_tolerance)
    }
}

impl<T: ApproxEq> ApproxEq for Option<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Option<T>, abs_tolerance: Self::Tolerance) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => T::abs_eq(a, b, abs_tolerance),
            (None, None) => true,
            _ => false,
        }
    }

    fn rel_eq(&self, other: &Option<T>, rel_tolerance: Self::Tolerance) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => T::rel_eq(a, b, rel_tolerance),
            (None, None) => true,
            _ => false,
        }
    }

    fn ulps_eq(&self, other: &Option<T>, ulps_tolerance: u32) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => T::ulps_eq(a, b, ulps_tolerance),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T: ApproxEq> ApproxEq for OnceCell<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Self, abs_tolerance: Self::Tolerance) -> bool {
        self.get().abs_eq(&other.get(), abs_tolerance)
    }

    fn rel_eq(&self, other: &Self, rel_tolerance: Self::Tolerance) -> bool {
        self.get().rel_eq(&other.get(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &Self, ulps_tolerance: u32) -> bool {
        self.get().ulps_eq(&other.get(), ulps_tolerance)
    }
}
