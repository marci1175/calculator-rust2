use calculator::Calculator;

fn main() {
    let calculator = Calculator::new(input()).unwrap();

    dbg!(calculator.calculate());
}

fn input() -> String {
    let mut string_buf = String::new();

    std::io::stdin().read_line(&mut string_buf).unwrap();

    string_buf.trim().to_string()
}
