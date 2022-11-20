#[cfg(not(feature = "nightly"))] // Only test in stable to avoid changing error messages.
#[test]
fn test() {
    let t = trybuild::TestCases::new();

    t.compile_fail("tests/ui/*.rs");
}
