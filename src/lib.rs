use std::fmt::Display;

use anyhow::bail;
use thiserror::Error;
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Number(f64),
    Addition,
    Subtraction,
    Division,
    Multiplication,
    Power,

    /// Variable refers to ```X```, variables as in mathematical sense
    Variable,

    Bracket(Vec<Expression>),

    BracketOpen,
    BracketClose,
}

impl Expression {
    pub fn get_number(&self) -> anyhow::Result<f64> {
        if let Expression::Number(num) = self {
            return Ok(*num);
        } else {
            bail!("Expression is not a number.");
        }
    }
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Number(num) => num.to_string(),
            Expression::Addition => String::from("+"),
            Expression::Subtraction => String::from("-"),
            Expression::Division => String::from("/"),
            Expression::Multiplication => String::from("*"),
            Expression::Power => String::from("^"),
            Expression::Variable => String::from("x"),
            Expression::Bracket(inner) => inner
                .iter()
                .map(|expr| format!("{} ", expr.to_string()))
                .collect::<Vec<String>>()
                .concat(),
            Expression::BracketOpen => String::from("("),
            Expression::BracketClose => String::from(")"),
        }
    }
}

#[derive(Default, Debug)]
pub struct Calculator {
    inner: Vec<Expression>,
}

impl Calculator {
    /// This function creates a new ```Calculator``` instance, and automaticly parses the passed in string.
    pub fn new(input: impl ToString) -> anyhow::Result<Self> {
        let calculator_instance = Self {
            inner: Self::parse(input.to_string())?,
        };

        Ok(calculator_instance)
    }

    /// Returns the parsed exypressions from the ```Calculator``` instance
    pub fn into_inner(&self) -> Vec<Expression> {
        self.inner.clone()
    }

    /// Parses input string and outputs a vector of expressions
    fn parse(input: String) -> anyhow::Result<Vec<Expression>> {
        let unparsed_tokens = Self::tokenize(input)?;
        let parsed_tokens = Self::evaluate(unparsed_tokens)?;

        Ok(parsed_tokens)
    }

    /// Tokenizes the string
    fn tokenize(input: String) -> anyhow::Result<Vec<Expression>> {
        let mut tokens: Vec<Expression> = vec![];

        let mut num_buffer: String = String::new();

        for (idx, char) in input.chars().enumerate() {
            let expression = match char.to_ascii_lowercase() {
                '+' => Expression::Addition,
                '-' => Expression::Subtraction,
                '*' => Expression::Multiplication,
                '/' | ':' => Expression::Division,
                '^' => Expression::Power,
                ' ' => {
                    if !num_buffer.is_empty() {
                        tokens.push(Expression::Number(num_buffer.parse()?));

                        num_buffer.clear();
                    }

                    continue;
                },
                'x' => Expression::Variable,
                '(' => Expression::BracketOpen,
                ')' => Expression::BracketClose,
                _ => {
                    if let Some(digit) = char.to_digit(10) {
                        num_buffer.push_str(&digit.to_string());

                        continue;
                    } else {
                        bail!(CalculatorError::new(
                            String::from(
                                "If you meant to define an unknown, only 'X' is available"
                            ),
                            input,
                            idx
                        ));
                    }
                }
            };

            if !num_buffer.is_empty() {
                tokens.push(Expression::Number(num_buffer.parse::<f64>()?));

                num_buffer.clear();
            }

            tokens.push(expression);
        }

        if !num_buffer.is_empty() {
            tokens.push(Expression::Number(num_buffer.parse::<f64>()?));
        }

        Ok(tokens)
    }
 
    /// Evaluates (Parses) the tokens
    /// This function adds the ```Expression::Bracket``` enum.
    fn evaluate(mut input: Vec<Expression>) -> anyhow::Result<Vec<Expression>> {
        let mut eval_buf: Vec<Expression> = vec![];
        let mut equation_buf: Vec<Expression> = vec![];

        let mut bracket_counter = 0;
        let mut bracket_pos: (usize, usize) = (0, 0);

        let mut iter_idx = 0;

        while input.len() > iter_idx {
            let expr = input[iter_idx].clone();

            match &expr {
                Expression::BracketOpen => {
                    if bracket_counter == 0 {
                        bracket_pos.0 = iter_idx;
                    }
                    else {
                        equation_buf.push(Expression::BracketOpen);
                    }
                    
                    bracket_counter += 1;
                }
                Expression::BracketClose => {
                    bracket_counter -= 1;
                    

                    if bracket_counter == 0 {
                        bracket_pos.1 = iter_idx;

                        input.drain(bracket_pos.0..bracket_pos.1);

                        eval_buf.insert(
                            bracket_pos.0,
                            Expression::Bracket(Self::evaluate(equation_buf.clone())?),
                        );

                        iter_idx = bracket_pos.0;

                        equation_buf.clear();
                    }
                    else {
                        equation_buf.push(Expression::BracketClose);
                    }
                }
                _ => {
                    if bracket_counter > 0 {
                        equation_buf.push(expr.clone());
                    } else {
                        eval_buf.push(expr.clone());
                    }
                }
            }

            iter_idx += 1;
        }

        Ok(eval_buf)
    }

    /// Calculates the result based on the parsed string. The reason this function is separate, is that you can use different variable values for calculations, without reparsing the data. 
    pub fn calculate(&self, variable_value: Option<f64>) -> anyhow::Result<f64> {
        return Self::__calc(self.inner.clone(), variable_value);
    }

    /// Private function for calculating
    fn __calc(mut input: Vec<Expression>, variable_value: Option<f64>) -> anyhow::Result<f64> {
        let mut iter_idx = 0;
        let mut last_expr: Option<Expression> = None;

        while input.len() > iter_idx {
            let expr = input[iter_idx].clone();

            match expr {
                Expression::Bracket(ref inner) => {
                    input.remove(iter_idx);
                    input.insert(iter_idx, Expression::Number(Self::__calc(inner.to_vec(), variable_value)?));

                    if matches!(last_expr, Some(Expression::Bracket(_))) || matches!(last_expr, Some(Expression::Number(_))) {
                        input.insert(iter_idx, Expression::Multiplication);
                        last_expr = None;
                    }

                    continue;
                }
                Expression::Variable => {
                    let variable_value = &variable_value.ok_or_else(|| {
                        CalculatorError::from_expression_list(
                            String::from("A variable was used in the equation, but a default value has not been set."),
                            input.clone(),
                            iter_idx
                        )
                    })?;

                    input[iter_idx] = Expression::Number(*variable_value);

                    continue;
                },
                Expression::BracketOpen => unreachable!(),
                Expression::BracketClose => unreachable!(),
                Expression::Number(_) => (),
                _ => {
                    let lhs = match input.clone().get(iter_idx - 1).ok_or(
                        CalculatorError::from_expression_list(
                            String::from("Expression not found."),
                            input.clone(),
                            iter_idx,
                        ),
                    )? {
                        Expression::Number(inner) => inner.clone(),
                        _ => {
                            bail!(CalculatorError::from_expression_list(
                                String::from("Expression can not be turned into a number."),
                                input.clone(),
                                iter_idx
                            ))
                        }
                    };

                    let input_clone = input.clone();
                    let rhs = match input_clone.get(iter_idx + 1).ok_or(
                        CalculatorError::from_expression_list(
                            String::from("Expression not found."),
                            input.clone(),
                            iter_idx,
                        ),
                    )? {
                        Expression::Number(inner) => inner,
                        _ => {
                            bail!(CalculatorError::from_expression_list(
                                String::from("Expression can not be turned into a number."),
                                input.clone(),
                                iter_idx
                            ))
                        }
                    };

                    if expr == Expression::Multiplication {
                        input.drain(iter_idx - 1..=iter_idx + 1);
                        input.insert(iter_idx - 1, Expression::Number(lhs * rhs));
                        iter_idx -= 1;
                    } else if expr == Expression::Division {
                        input.drain(iter_idx - 1..=iter_idx + 1);
                        input.insert(iter_idx - 1, Expression::Number(lhs / rhs));
                        iter_idx -= 1;
                    } else if expr == Expression::Power {
                        input.drain(iter_idx - 1..=iter_idx + 1);
                        input.insert(iter_idx - 1, Expression::Number(lhs.powf(*rhs)));
                        iter_idx -= 1;
                    }
                }
            }

            last_expr = Some(expr);
            iter_idx += 1;
        }

        iter_idx = 0;
        
        while input.len() > iter_idx {
            let expr = input[iter_idx].clone();

            match expr {
                Expression::Bracket(ref inner) => {
                    input.remove(iter_idx);
                    input.insert(iter_idx, Expression::Number(Self::__calc(inner.to_vec(), variable_value)?));

                    if matches!(last_expr, Some(Expression::Bracket(_))) || matches!(last_expr, Some(Expression::Number(_))) {
                        input.insert(iter_idx, Expression::Multiplication);
                        last_expr = None;
                    }

                    continue;
                }
                Expression::Variable => {
                    let variable_value = &variable_value.ok_or_else(|| {
                        CalculatorError::from_expression_list(
                            String::from("A variable was used in the equation, but a default value has not been set."),
                            input.clone(),
                            iter_idx
                        )
                    })?;

                    input[iter_idx] = Expression::Number(*variable_value);

                    continue;
                },
                Expression::BracketOpen => unreachable!(),
                Expression::BracketClose => unreachable!(),
                Expression::Number(_) => (),
                _ => {
                    let lhs = match input.clone().get(iter_idx - 1).ok_or(
                        CalculatorError::from_expression_list(
                            String::from("Expression not found."),
                            input.clone(),
                            iter_idx,
                        ),
                    )? {
                        Expression::Bracket(inner) => {
                            input.remove(iter_idx - 1);
                            input.insert(
                                iter_idx - 1,
                                Expression::Number(Self::__calc(inner.to_vec(), variable_value)?),
                            );

                            continue;
                        }
                        Expression::Number(inner) => inner.clone(),
                        _ => {
                            bail!(CalculatorError::from_expression_list(
                                String::from("Expression can not be turned into a number."),
                                input.clone(),
                                iter_idx
                            ))
                        }
                    };

                    let input_clone = input.clone();
                    let rhs = match input_clone.get(iter_idx + 1).ok_or(
                        CalculatorError::from_expression_list(
                            String::from("Expression not found."),
                            input.clone(),
                            iter_idx,
                        ),
                    )? {
                        Expression::Bracket(inner) => {
                            input.remove(iter_idx + 1);
                            input.insert(
                                iter_idx + 1,
                                Expression::Number(Self::__calc(inner.to_vec(), variable_value)?),
                            );

                            continue;
                        }
                        Expression::Number(inner) => inner,
                        _ => {
                            bail!(CalculatorError::from_expression_list(
                                String::from("Expression can not be turned into a number."),
                                input.clone(),
                                iter_idx
                            ))
                        }
                    };

                    if expr == Expression::Addition {
                        input.drain(iter_idx - 1..=iter_idx + 1);
                        input.insert(iter_idx - 1, Expression::Number(lhs + rhs));
                        iter_idx -= 1;
                    } else if expr == Expression::Subtraction {
                        input.drain(iter_idx - 1..=iter_idx + 1);
                        input.insert(iter_idx - 1, Expression::Number(lhs - rhs));
                        iter_idx -= 1;
                    } else if expr == Expression::Variable {
                        input.drain(iter_idx - 1..=iter_idx + 1);
                        input.insert(iter_idx - 1, Expression::Number(0.0));
                        iter_idx -= 1;
                    }

                }
            }

            last_expr = Some(expr);
            iter_idx += 1;
        }

        if input.is_empty() || input.len() != 1 {
            bail!(CalculatorError::from_expression_list(String::from("Empty equation or invalid equation."), input.clone(), 1));
        }

        Ok(input[0].get_number()?)
    }
}

#[derive(Error, Debug)]
pub struct CalculatorError {
    message: String,

    equation: String,

    error_idx: usize,
}

impl Display for CalculatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "Syntax error!\nMessage: {}\n{}\n{}",
            self.message,
            self.equation,
            {
                let mut indentation = String::new();

                for _ in 0..self.error_idx {
                    indentation.push(' ');
                }

                indentation.push('^');

                indentation
            }
        ))
    }
}

impl CalculatorError {
    pub fn new(message: String, equation: String, error_idx: usize) -> Self {
        Self {
            message,
            equation,
            error_idx,
        }
    }

    pub fn from_expression_list(
        message: String,
        equation: Vec<Expression>,
        error_idx: usize,
    ) -> Self {
        let mut equation_string = String::new();

        for expr in equation {
            equation_string.push_str(&format!("{} ", expr.to_string()));
        }

        Self {
            message,
            equation: equation_string,
            error_idx: error_idx * 2 - 1,
        }
    }
}
