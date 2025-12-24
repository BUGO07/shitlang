use crate::interpreter::InterpretValue;

macro_rules! impl_test {
    ($name:ident, $matcher:expr) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            let ctrl = $crate::run_file(concat!("res/tests/", stringify!($name), ".lang"))?;

            match ctrl {
                $crate::interpreter::ControlFlow::Return(x) => assert!($matcher(x)),
                _ => panic!("Unexpected control flow: {ctrl:?}"),
            }

            Ok(())
        }
    };
}

impl_test!(binary_ops, |x| { matches!(x, InterpretValue::I32(56)) });
impl_test!(chained_functions, |x| {
    assert_eq!(x, InterpretValue::F64(6.7));
    true
});
