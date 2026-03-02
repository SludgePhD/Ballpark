//! Implementations for `liballoc`-only types.

extern crate alloc;

use core::iter::zip;

use alloc::{
    boxed::Box,
    collections::{LinkedList, VecDeque},
    rc::Rc,
    sync::Arc,
    vec::Vec,
};

use super::ApproxEq;

impl<T: ApproxEq + ?Sized> ApproxEq for Box<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Box<T>, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(&**self, &**other, abs_tolerance)
    }

    fn rel_eq(&self, other: &Box<T>, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(&**self, &**other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &Box<T>, ulps_tolerance: u32) -> bool {
        T::ulps_eq(&**self, &**other, ulps_tolerance)
    }
}

impl<T: ApproxEq + ?Sized> ApproxEq for Rc<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Rc<T>, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(&**self, &**other, abs_tolerance)
    }

    fn rel_eq(&self, other: &Rc<T>, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(&**self, &**other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &Rc<T>, ulps_tolerance: u32) -> bool {
        T::ulps_eq(&**self, &**other, ulps_tolerance)
    }
}

impl<T: ApproxEq + ?Sized> ApproxEq for Arc<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Arc<T>, abs_tolerance: Self::Tolerance) -> bool {
        T::abs_eq(&**self, &**other, abs_tolerance)
    }

    fn rel_eq(&self, other: &Arc<T>, rel_tolerance: Self::Tolerance) -> bool {
        T::rel_eq(&**self, &**other, rel_tolerance)
    }

    fn ulps_eq(&self, other: &Arc<T>, ulps_tolerance: u32) -> bool {
        T::ulps_eq(&**self, &**other, ulps_tolerance)
    }
}

impl<T: ApproxEq> ApproxEq for Vec<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Vec<T>, abs_tolerance: Self::Tolerance) -> bool {
        self.as_slice().abs_eq(other.as_slice(), abs_tolerance)
    }

    fn rel_eq(&self, other: &Vec<T>, rel_tolerance: Self::Tolerance) -> bool {
        self.as_slice().rel_eq(other.as_slice(), rel_tolerance)
    }

    fn ulps_eq(&self, other: &Vec<T>, ulps_tolerance: u32) -> bool {
        self.as_slice().ulps_eq(other.as_slice(), ulps_tolerance)
    }
}

impl<T: ApproxEq> ApproxEq for VecDeque<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Self, abs_tolerance: Self::Tolerance) -> bool {
        if self.len() != other.len() {
            return false;
        }

        zip(self.iter(), other.iter()).all(|(a, b)| a.abs_eq(b, abs_tolerance))
    }

    fn rel_eq(&self, other: &Self, rel_tolerance: Self::Tolerance) -> bool {
        if self.len() != other.len() {
            return false;
        }

        zip(self.iter(), other.iter()).all(|(a, b)| a.rel_eq(b, rel_tolerance))
    }

    fn ulps_eq(&self, other: &Self, ulps_tolerance: u32) -> bool {
        if self.len() != other.len() {
            return false;
        }

        zip(self.iter(), other.iter()).all(|(a, b)| a.ulps_eq(b, ulps_tolerance))
    }
}

impl<T: ApproxEq> ApproxEq for LinkedList<T> {
    type Tolerance = T::Tolerance;

    fn abs_eq(&self, other: &Self, abs_tolerance: Self::Tolerance) -> bool {
        if self.len() != other.len() {
            return false;
        }

        zip(self.iter(), other.iter()).all(|(a, b)| a.abs_eq(b, abs_tolerance))
    }

    fn rel_eq(&self, other: &Self, rel_tolerance: Self::Tolerance) -> bool {
        if self.len() != other.len() {
            return false;
        }

        zip(self.iter(), other.iter()).all(|(a, b)| a.rel_eq(b, rel_tolerance))
    }

    fn ulps_eq(&self, other: &Self, ulps_tolerance: u32) -> bool {
        if self.len() != other.len() {
            return false;
        }

        zip(self.iter(), other.iter()).all(|(a, b)| a.ulps_eq(b, ulps_tolerance))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{LinkedList, VecDeque};

    use crate::{assert_approx_eq, assert_approx_ne};

    #[test]
    fn vec() {
        assert_approx_eq!(vec![1.0, 2.0], vec![1.0 + f64::EPSILON, 2.0]).ulps(1);
        assert_approx_ne!(vec![1.0, 2.0], vec![1.0 + 1e-6, 2.0]).ulps(1);
        assert_approx_ne!(vec![1.0, 2.0], vec![1.0]);
    }

    #[test]
    fn vecdeque() {
        let mut a = VecDeque::from_iter([0.0, 1.0]);
        a.rotate_left(1);

        let mut b = VecDeque::from_iter([0.0, 1.0]);
        b.rotate_right(1);

        assert_approx_eq!(a, b);

        assert_approx_ne!(a, VecDeque::from_iter([0.0]));
        assert_approx_ne!(a, VecDeque::from_iter([1.0]));
    }

    #[test]
    fn linked_list() {
        assert_approx_eq!(
            LinkedList::from_iter([0.0, 1.0]),
            LinkedList::from_iter([0.0, 1.0 + f64::EPSILON])
        );

        assert_approx_ne!(
            LinkedList::from_iter([1.0, 0.0]),
            LinkedList::from_iter([1.0 + f64::EPSILON])
        );
    }
}
