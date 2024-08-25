### A simple calculator library written for rust.

## Example
```rust
// Create calculator instance
let calculator = Calculator::new("((5^3 + 4^2) * (12^2 - 6^3)) / (3^2 + 7) + (144/12 + 8^2) - (2^4 * 7) + 3^3").unwrap();
// Calculate
let result = calculator.calculate(None);
. . .
```
