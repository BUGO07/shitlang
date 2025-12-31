macro_rules! impl_test {
    ($name:ident, $matcher:expr) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::run_file(concat!("tests/", stringify!($name), ".lang"))?;
            let result =
                ::std::process::Command::new(concat!("build/", stringify!($name))).output()?;
            anyhow::ensure!(
                result.status.success(),
                "Exited with non-zero status: {}",
                result.status
            );
            let stdout = String::from_utf8(result.stdout)?;
            assert!($matcher(&stdout));

            Ok(())
        }
    };
    ($name:ident, $input:expr, $matcher:expr) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::run_file(concat!("tests/", stringify!($name), ".lang"))?;
            let mut process = ::std::process::Command::new(concat!("build/", stringify!($name)))
                .stdin(::std::process::Stdio::piped())
                .stdout(::std::process::Stdio::piped())
                .spawn()?;
            let mut stdin = process.stdin.take().unwrap();
            ::std::thread::spawn(move || {
                use ::std::io::Write;
                stdin.write_all($input.as_bytes()).unwrap();
            });
            let result = process.wait_with_output()?;
            anyhow::ensure!(
                result.status.success(),
                "Exited with non-zero status: {}",
                result.status
            );
            let stdout = String::from_utf8(result.stdout)?;
            assert!($matcher(&stdout));

            Ok(())
        }
    };
}

impl_test!(while_loop, |x| {
    matches!(x, "i = 1\ni = 3\ni = 5\ni = 7\ni = 9\n")
});

impl_test!(calculator, "5 + 2", |x| {
    matches!(x, "Enter an expression: 5 + 2 = 7\n")
});
