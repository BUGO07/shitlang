use crate::interpreter::InterpretValue;

pub fn print(args: Vec<InterpretValue>) -> anyhow::Result<InterpretValue> {
    match args.len() {
        0 => {
            println!();
        }
        1 => {
            println!("{}", args[0]);
        }
        _ => match args[0] {
            InterpretValue::String(ref first) => {
                let mut output = first.clone();
                for arg in &args[1..] {
                    output = output.replacen("{}", &arg.to_string(), 1);
                }
                println!("{}", output);
            }
            _ => {
                for arg in args {
                    print!("{} ", arg);
                }
                println!();
            }
        },
    }
    Ok(InterpretValue::Void)
}

pub fn exit(args: Vec<InterpretValue>) -> anyhow::Result<InterpretValue> {
    let code = if args.is_empty() {
        0
    } else {
        args[0].as_integer() as i32
    };
    std::process::exit(code);
}
