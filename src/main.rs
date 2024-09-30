use std::collections::HashMap;

fn main() {
    println!("Svete Basic");
    let mut scope: HashMap<String, Type> = HashMap::new();
    run_program(
        r#"
Let x = 0
If x Then
    Print "Ok!"
Else
    Print "No!"
End
        "#
        .to_string(),
        &mut scope,
    );
}

fn run_program(source: String, scope: &mut HashMap<String, Type>) {
    run_block(parse_program(source).unwrap(), scope);
}

fn run_block(block: Block, scope: &mut HashMap<String, Type>) {
    for mut line in block {
        dbg!(&line, &scope);
        line.run(scope);
    }
}

fn parse_program(source: String) -> Option<Block> {
    let mut result: Block = vec![];
    let mut block: Block = vec![];
    let mut nest = 0;
    let mut temp: Option<Statement> = None;
    let mut is_else = false;

    for line in source.split("\n") {
        let mut line = line.trim().to_string();
        if nest == 0 {
            if line.starts_with("Print") {
                line = line.replacen("Print", "", 1);
                result.push(Statement::Print(parse_expr(line.trim().to_string())?));
            } else if line.starts_with("Let") && line.contains("=") {
                let line: Vec<String> = line
                    .replacen("Let", "", 1)
                    .split("=")
                    .map(|s| s.to_string())
                    .collect();
                result.push(Statement::Let(
                    line.get(0)?.trim().to_string(),
                    parse_expr(line.get(1..)?.join("=").trim().to_string())?,
                ));
            } else if line.starts_with("If") && line.ends_with("Then") {
                temp = Some(Statement::If(
                    parse_expr(
                        line.replacen("If", "", 1)
                            .get(0..line.len() - 6)?
                            .trim()
                            .to_string(),
                    )?,
                    vec![],
                    None,
                ));
                nest += 1
            }
        } else {
            if line == "End".to_string() {
                match temp.clone()? {
                    Statement::If(expr, true_code, _) => {
                        if is_else {
                            result.push(Statement::If(expr, true_code, Some(block.clone())));
                        } else {
                            result.push(Statement::If(expr, block.clone(), None));
                        }
                        nest -= 1;
                    }
                    _ => {}
                }
            } else if line == "Else".to_string() {
                match temp.clone()? {
                    Statement::If(expr, _, _) => {
                        if is_else {
                            return None;
                        } else {
                            temp = Some(Statement::If(expr, block.clone(), None));
                        }
                        is_else = true;
                    }
                    _ => {}
                }
            } else {
                block.push(parse_program(line)?[0].clone());
            }
        }
    }
    Some(result)
}

fn tokenize_expr(input: String) -> Option<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' if !in_quote => {
                current_token.push(c);
                if in_parentheses > 0 {
                    in_parentheses -= 1;
                } else {
                    eprintln!("Error! there's duplicate end of the parentheses");
                    return None;
                }
            }
            ' ' | '　' | '\t' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    // Syntax error check
    if in_quote {
        eprintln!("Error! there's not end of the quote");
        return None;
    }
    if in_parentheses != 0 {
        eprintln!("Error! there's not end of the parentheses");
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Some(tokens)
}

fn parse_expr(soruce: String) -> Option<Expr> {
    let tokens: Vec<String> = {
        let mut tokens: Vec<String> = tokenize_expr(soruce)?;
        tokens.reverse();
        tokens
    };

    let value0 = tokens.get(0)?.trim().to_string();
    let value0 = if let Ok(n) = value0.parse::<f64>() {
        Expr::Value(Type::Number(n))
    } else if value0.starts_with('"') && value0.ends_with('"') {
        let value0 = {
            let mut value0 = value0.clone();
            value0.remove(0);
            value0.remove(value0.len() - 1);
            value0
        };
        Expr::Value(Type::String(value0.to_string()))
    } else if value0.starts_with('(') && value0.ends_with(')') {
        let value0 = {
            let mut value0 = value0.clone();
            value0.remove(0);
            value0.remove(value0.len() - 1);
            value0
        };
        parse_expr(value0)?
    } else {
        Expr::Value(Type::Symbol(value0))
    };

    if let Some(operator) = tokens.get(1) {
        let operator = match operator.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (value0, parse_expr(tokens.get(2..)?.to_vec().join(" "))?),
        })))
    } else {
        return Some(value0);
    }
}

#[derive(Debug, Clone)]
enum Type {
    Number(f64),
    Symbol(String),
    String(String),
    Bool(bool),
}

impl Type {
    fn get_number(&self) -> f64 {
        match self {
            Type::Number(n) => n.to_owned(),
            Type::String(s) | Type::Symbol(s) => s.trim().parse().unwrap_or(0.0),
            Type::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
        }
    }

    fn get_bool(&self) -> bool {
        match self {
            Type::Number(n) => *n != 0.0,
            Type::String(s) | Type::Symbol(s) => s.trim().parse().unwrap_or(false),
            Type::Bool(b) => *b,
        }
    }

    fn get_string(&self) -> String {
        match self {
            Type::String(s) | Type::Symbol(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Bool(b) => b.to_string(),
        }
    }
}

type Block = Vec<Statement>;
#[derive(Debug, Clone)]
enum Statement {
    Print(Expr),
    Let(String, Expr),
    If(Expr, Block, Option<Block>),
}

impl Statement {
    fn run(&mut self, scope: &mut HashMap<String, Type>) {
        match &self {
            Statement::Print(expr) => {
                println!("{}", expr.eval(scope.clone()).get_string())
            }
            Statement::Let(name, expr) => {
                scope.insert(name.to_string(), expr.eval(scope.clone()));
            }
            Statement::If(expr, code_true, code_false) => {
                if expr.eval(scope.clone()).get_bool() {
                    run_block(code_true.to_vec(), scope);
                } else {
                    if let Some(code_false) = code_false {
                        run_block(code_false.to_vec(), scope);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    Value(Type),
}

impl Expr {
    fn eval(&self, scope: HashMap<String, Type>) -> Type {
        match self {
            Expr::Infix(infix) => (*infix).eval(scope),
            Expr::Value(value) => {
                if let Type::Symbol(name) = value {
                    if let Some(refer) = scope.get(name.as_str()) {
                        refer.clone()
                    } else {
                        value.clone()
                    }
                } else {
                    value.clone()
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Infix {
    operator: Operator,
    values: (Expr, Expr),
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Infix {
    fn eval(&self, scope: HashMap<String, Type>) -> Type {
        let value0 = self.values.0.eval(scope.clone());
        let value1 = self.values.1.eval(scope);
        match self.operator {
            Operator::Add => Type::Number(value0.get_number() + value1.get_number()),
            Operator::Sub => Type::Number(value0.get_number() - value1.get_number()),
            Operator::Mul => Type::Number(value0.get_number() * value1.get_number()),
            Operator::Div => Type::Number(value0.get_number() / value1.get_number()),
        }
    }
}
