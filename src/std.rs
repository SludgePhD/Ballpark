//! Implementations for `libstd`-only types.

use std::sync::OnceLock;

use crate::ApproxEq;

extern crate std;

impl<T: ApproxEq> ApproxEq for OnceLock<T> {
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
