use calculator::Calculator;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    let calculator = Calculator::new(input()).unwrap();

    dbg!(calculator.calculate().inspect_err(|err| {
        dbg!(err.backtrace());
    }).unwrap());
}

fn input() -> String {
    let mut string_buf = String::new();

    std::io::stdin().read_line(&mut string_buf).unwrap();

    string_buf.trim().to_string()
}
