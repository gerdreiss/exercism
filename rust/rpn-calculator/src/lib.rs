#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack: Vec<i32> = Vec::new();
    for input in inputs {
        match input {
            CalculatorInput::Value(v) => {
                stack.push(*v);
            }
            _ => {
                let curr = stack.pop();
                let prev = stack.pop();
                match (prev, curr) {
                    (Some(x), Some(y)) => {
                        match input {
                            CalculatorInput::Add => stack.push(x + y),
                            CalculatorInput::Subtract => stack.push(x - y),
                            CalculatorInput::Multiply => stack.push(x * y),
                            CalculatorInput::Divide => stack.push(x / y),
                            _ => {}
                        }
                    }
                    _ => return None
                }
            }
        }
    }
    if stack.len() > 1 {
        None
    } else {
        stack.pop()
    }
}
