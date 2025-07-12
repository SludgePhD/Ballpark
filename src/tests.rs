use crate::{assert_approx_eq, assert_approx_ne, debug_assert_approx_eq, debug_assert_approx_ne};

#[test]
#[should_panic(expected = "assertion `left == right` failed")]
fn fail_eq() {
    assert_approx_eq!(1.0, 2.0,);
}

#[test]
#[should_panic(expected = "my message 123")]
fn fail_eq_msg() {
    assert_approx_eq!(1.0, 2.0, "my message {}", 123,);
}

#[test]
#[cfg_attr(
    debug_assertions,
    should_panic(expected = "assertion `left == right` failed")
)]
fn debug_fail_eq() {
    debug_assert_approx_eq!(1.0, 2.0,);
}

#[test]
#[cfg_attr(debug_assertions, should_panic(expected = "my message 123"))]
fn debug_fail_eq_msg() {
    debug_assert_approx_eq!(1.0, 2.0, "my message {}", 123,);
}

#[test]
#[should_panic(expected = "assertion `left != right` failed")]
fn fail_ne() {
    assert_approx_ne!(1.0, 1.0,);
}

#[test]
#[should_panic(expected = "my message 123")]
fn fail_ne_msg() {
    assert_approx_ne!(1.0, 1.0, "my message {}", 123,);
}

#[test]
#[cfg_attr(
    debug_assertions,
    should_panic(expected = "assertion `left != right` failed")
)]
fn debug_fail_ne() {
    debug_assert_approx_ne!(1.0, 1.0,);
}

#[test]
#[cfg_attr(debug_assertions, should_panic(expected = "my message 123"))]
fn debug_fail_ne_msg() {
    debug_assert_approx_ne!(1.0, 1.0, "my message {}", 123,);
}

#[test]
#[should_panic(expected = "my message: test")]
fn assertion_message_fmt_move() {
    let msg = "test";
    assert_approx_eq!(1.0, 2.0, "my message: {msg}")
        .abs(0.5)
        .ulps(1);
}

#[test]
fn ignore() {
    assert_approx_eq!(0.0, 1.0).ignore();
}

#[test]
fn rel() {
    assert_approx_eq!(1.0, 1.001).rel(0.01);
    assert_approx_eq!(1.0, -1.0).rel(2.0);
    assert_approx_eq!(0.0, 0.00001).rel(1.0);
}

#[test]
fn around_zero() {
    let nearly_zero = f32::from_bits(1);
    assert!(nearly_zero > 0.0);
    assert!(nearly_zero < 1e-40);

    let neg_nearly_zero = f32::from_bits((-0.0_f32).to_bits() | 1);
    assert!(neg_nearly_zero < 0.0);
    assert!(neg_nearly_zero > -1e-40);

    assert_approx_eq!(nearly_zero, neg_nearly_zero).abs(1e-44);
    assert_approx_eq!(nearly_zero, neg_nearly_zero).ulps(2);
    assert_approx_ne!(nearly_zero, neg_nearly_zero).ulps(1);
    assert_approx_eq!(0.0, neg_nearly_zero).ulps(1);
    assert_approx_ne!(0.0, neg_nearly_zero).ulps(0);
}

#[test]
fn epsilon() {
    assert_approx_eq!(1.0, 1.0 + f32::EPSILON);
    assert_approx_eq!(1.0, 1.0 + f32::EPSILON).ulps(1);
    assert_approx_ne!(1.0, 1.0 + f32::EPSILON).ulps(0);
}

#[test]
fn negative() {
    assert_approx_ne!(1.0, -1.0);
    assert_approx_ne!(1.0, -1.0).abs(1.0);
    assert_approx_eq!(1.0, -1.0).abs(2.0);
    assert_approx_eq!(-1.0, -1.0).abs(0.0);
    assert_approx_eq!(-1.0, -1.0).rel(0.0);
    assert_approx_eq!(-1.0, -1.0).ulps(0);
}

#[test]
fn nan() {
    assert_approx_ne!(f32::NAN, f32::NAN).abs(0.0);
    assert_approx_ne!(f32::NAN, f32::NAN).rel(0.0);
    assert_approx_ne!(f32::NAN, f32::NAN).ulps(0);
    assert_approx_ne!(f32::NAN, f32::NAN).abs(1.0);
    assert_approx_ne!(f32::NAN, f32::NAN).rel(1.0);
    assert_approx_ne!(f32::NAN, f32::NAN).ulps(100);

    assert_approx_ne!(f32::NAN, 0.0).abs(0.0);
    assert_approx_ne!(f32::NAN, 0.0).rel(0.0);
    assert_approx_ne!(f32::NAN, 0.0).ulps(0);
    assert_approx_ne!(f32::NAN, 0.0).abs(1.0);
    assert_approx_ne!(f32::NAN, 0.0).rel(1.0);
    assert_approx_ne!(f32::NAN, 0.0).ulps(100);
}

#[test]
fn inf() {
    assert_approx_eq!(f32::INFINITY, f32::INFINITY).abs(0.0);
    assert_approx_eq!(f32::INFINITY, f32::INFINITY).rel(0.0);
    assert_approx_eq!(f32::INFINITY, f32::INFINITY).ulps(0);
    assert_approx_ne!(f32::INFINITY, f32::MAX).abs(10000.0);
    assert_approx_ne!(f32::INFINITY, f32::MAX).abs(f32::MAX);
    assert_approx_eq!(f32::INFINITY, f32::MAX).rel(10000.0);
    assert_approx_ne!(f32::MAX, f32::INFINITY).abs(f32::MAX);
    assert_approx_eq!(f32::MAX, f32::INFINITY).abs(f32::INFINITY);
    assert_approx_eq!(f32::MAX, f32::INFINITY).rel(f32::MAX);
    assert_approx_eq!(f32::MAX, f32::INFINITY).rel(f32::INFINITY);

    // `f32::MAX * 1.000001 == inf`.
    // `MAX` and `INFINITY` are exactly one ULP apart.
    assert_approx_ne!(f32::MAX, f32::INFINITY).ulps(0);
    assert_approx_eq!(f32::MAX, f32::INFINITY).ulps(1);
    assert_approx_eq!(f32::MAX, f32::INFINITY).ulps(u32::MAX);

    assert_approx_eq!(f64::INFINITY, f64::INFINITY).abs(0.0);
    assert_approx_eq!(f64::INFINITY, f64::INFINITY).rel(0.0);
    assert_approx_eq!(f64::INFINITY, f64::INFINITY).ulps(0);
    assert_approx_ne!(f64::INFINITY, f64::MAX).abs(10000.0);
    assert_approx_eq!(f64::INFINITY, f64::MAX).rel(10000.0);
    assert_approx_ne!(f64::MAX, f64::INFINITY).abs(f64::MAX);
    assert_approx_eq!(f64::MAX, f64::INFINITY).abs(f64::INFINITY);
    assert_approx_eq!(-f64::MAX, f64::INFINITY).abs(f64::INFINITY);
    assert_approx_eq!(f64::MAX, -f64::INFINITY).abs(f64::INFINITY);
    assert_approx_eq!(-f64::MAX, -f64::INFINITY).abs(f64::INFINITY);

    assert_approx_eq!(-f64::INFINITY, f64::INFINITY).abs(f64::INFINITY);
    assert_approx_eq!(f64::INFINITY, -f64::INFINITY).abs(f64::INFINITY);

    assert_approx_eq!(-f64::INFINITY, f64::INFINITY).rel(f64::INFINITY);
    assert_approx_eq!(f64::INFINITY, -f64::INFINITY).rel(f64::INFINITY);

    assert_approx_eq!(f64::MAX, f64::INFINITY).rel(f64::MAX);
    assert_approx_eq!(f64::MAX, f64::INFINITY).rel(f64::INFINITY);
    assert_approx_ne!(f64::MAX, f64::INFINITY).ulps(0);
    assert_approx_eq!(f64::MAX, f64::INFINITY).ulps(1);
    assert_approx_eq!(f64::MAX, f64::INFINITY).ulps(u32::MAX);
}

#[test]
fn temporary() {
    // Test for the behavior in https://github.com/brendanzab/approx/issues/71
    let x = &[0.0];
    assert_eq!(vec![0.0].as_slice(), x);
    assert_approx_eq!(vec![0.0].as_slice(), x);
}

#[test]
fn option() {
    assert_approx_eq!(None::<f32>, None::<f32>).abs(0.0);
    assert_approx_eq!(None::<f32>, None::<f32>).rel(0.0);
    assert_approx_eq!(None::<f32>, None::<f32>).ulps(0);
    assert_approx_eq!(Some(0.0), Some(0.0)).abs(0.0);
    assert_approx_eq!(Some(0.0), Some(0.0)).rel(0.0);
    assert_approx_eq!(Some(0.0), Some(0.0)).ulps(0);

    assert_approx_ne!(None::<f32>, Some(0.0)).rel(0.0);
    assert_approx_ne!(Some(0.0), None::<f32>).rel(0.0);
}

#[test]
fn tuples() {
    assert_approx_eq!((0.0,), (0.0,)).rel(0.0);
    assert_approx_eq!((0.0, 1.0), (0.0, 1.0)).rel(0.0);

    assert_approx_eq!(
        (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
        (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
    )
    .rel(0.0);
    assert_approx_ne!(
        (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
        (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.1),
    )
    .rel(0.0);
    assert_approx_ne!(
        (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
        (0.1, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
    )
    .rel(0.0);
}
