# Changelog

## v1.0.0

### Breaking Changes

- Prevent `Assertion` objects from being assigned to `let` bindings without obstacles.
  - Only one `Assertion` is meant to exist at a time in any given scope.

### Enhancements

- `ApproxEq` is now implemented for `LinkedList`.
- Documentation has been improved a bit.

## v0.1.1

### Improvements

- Specify `rust-version` in `Cargo.toml`, and run CI against that version.
- Improve docs a bit, and shorten the readme to better surface information.

## v0.1.0

Initial release.
