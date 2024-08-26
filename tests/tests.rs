#[cfg(test)]
mod test {
    use calculator_lib::Expression;

    use calculator_lib::Calculator;

    #[test]
    fn brackets() {
        let calculator = Calculator::new("(2)(9)").unwrap();

        assert_eq!(
            calculator.into_inner(),
            vec![
                Expression::Bracket(vec![Expression::Number(2.0)]),
                Expression::Bracket(vec![Expression::Number(9.0)])
            ]
        )
    }

    #[test]
    fn operator_order() {
        let calculator = Calculator::new("5 + 2 * 10").unwrap();

        assert_eq!(calculator.calculate(None).unwrap(), 25.)
    }

    #[test]
    fn eq1() {
        let calculator = Calculator::new("(3^2 + 5^2) * (4^3 - 2^3) + 100/5 - 7").unwrap();

        assert_eq!(calculator.calculate(None).unwrap(), 1917.);
    }

    #[test]
    fn eq2() {
        let calculator = Calculator::new(
            "((5^3 + 4^2) * (12^2 - 6^3)) / (3^2 + 7) + (144/12 + 8^2) - (2^4 * 7) + 3^3",
        )
        .unwrap();

        assert_eq!(calculator.calculate(None).unwrap(), -643.5);
    }

    #[test]
    fn floating_point() {
        let calculator = Calculator::new(
            "6.2 - 5.2",
        )
        .unwrap();

        assert_eq!(calculator.calculate(None).unwrap(), 1.);
    }

    #[test]
    fn neg() {
        let calculator = Calculator::new(
            "-5.2",
        )
        .unwrap();

        assert_eq!(calculator.calculate(None).unwrap(), -5.2);
    }
}
