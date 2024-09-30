use clap::Parser;
use std::{collections::HashMap, fs::read_to_string};

const VERSION: &str = "0.1.0";

#[derive(Parser, Debug)]
#[command(
    name = "Svete Basic",
    version = VERSION,
    author = "梶塚太智, kajizukataichi@outlook.jp",
    about = "Basic dialect that support structured programming",
)]
struct Cli {
    /// Script file to be running
    #[arg(index = 1)]
    file: Option<String>,
}

fn main() {
    let mut scope: HashMap<String, Type> = HashMap::new();
    let cli = Cli::parse();
    if let Some(path) = cli.file {
        if let Ok(code) = read_to_string(path) {
            run_program(code.to_string(), &mut scope);
        }
    }
}

fn run_program(source: String, scope: &mut HashMap<String, Type>) {
    let program = parse_program(source).unwrap();
    dbg!(&program);
    run_block(program, scope);
}

fn run_block(block: Block, scope: &mut HashMap<String, Type>) {
    for mut line in block {
        line.run(scope);
    }
}

fn parse_program(source: String) -> Option<Block> {
    let mut result: Block = vec![];
    let mut block: String = String::new();
    let mut nest = 0;
    let mut temp: Option<Statement> = None;
    let mut is_else = false;

    for line in source.split("\n") {
        let mut line = line.trim().to_string();
        if line.is_empty() {
            continue;
        }

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
                            .get(0..line.len() - "IfThen".len())?
                            .trim()
                            .to_string(),
                    )?,
                    vec![],
                    None,
                ));
                nest += 1
            } else if line.starts_with("While") && line.ends_with("Loop") {
                temp = Some(Statement::While(
                    parse_expr(
                        line.replacen("While", "", 1)
                            .get(0..line.len() - "WhileLoop".len())?
                            .trim()
                            .to_string(),
                    )?,
                    vec![],
                ));
                nest += 1
            }
        } else {
            if line == "End".to_string() {
                if nest == 1 {
                    match temp.clone()? {
                        Statement::If(expr, true_code, _) => {
                            if is_else {
                                result.push(Statement::If(
                                    expr,
                                    true_code,
                                    Some(parse_program(block.clone())?),
                                ));
                            } else {
                                result.push(Statement::If(
                                    expr,
                                    parse_program(block.clone())?,
                                    None,
                                ));
                            }
                            block.clear();
                        }
                        Statement::While(expr, _) => {
                            result.push(Statement::While(expr, parse_program(block.clone())?));
                            block.clear();
                        }
                        _ => {}
                    }
                } else {
                    block += &format!("{line}\n");
                }
                nest -= 1;
            } else if line == "Else".to_string() {
                if nest == 1 {
                    match temp.clone()? {
                        Statement::If(expr, _, _) => {
                            if is_else {
                                return None;
                            } else {
                                temp =
                                    Some(Statement::If(expr, parse_program(block.clone())?, None));
                                block.clear();
                            }
                            is_else = true;
                        }
                        _ => {}
                    }
                } else {
                    block += &format!("{line}\n");
                }
            } else if line.starts_with("If") && line.ends_with("Then") {
                nest += 1;
                block += &format!("{line}\n");
            } else if line.starts_with("While") && line.ends_with("Loop") {
                nest += 1;
                block += &format!("{line}\n");
            } else {
                block += &format!("{line}\n");
            }
            dbg!(&block);
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
    let tokens: Vec<String> = tokenize_expr(soruce)?;

    let value0 = tokens.last()?.trim().to_string();
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

    if let Some(operator) = {
        let mut tokens = tokens.clone();
        tokens.reverse();
        tokens
    }
    .get(1)
    {
        let operator = match operator.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "=" => Operator::Equal,
            "<" => Operator::LessThan,
            ">" => Operator::GreaterThan,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (
                parse_expr(tokens.get(..tokens.len() - 2)?.to_vec().join(" "))?,
                value0,
            ),
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
    While(Expr, Block),
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
            Statement::While(expr, code) => {
                while expr.eval(scope.clone()).get_bool() {
                    run_block(code.to_vec(), scope);
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
    Mod,
    Equal,
    LessThan,
    GreaterThan,
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
            Operator::Mod => Type::Number(value0.get_number() % value1.get_number()),
            Operator::Equal => Type::Bool(value0.get_string() == value1.get_string()),
            Operator::LessThan => Type::Bool(value0.get_number() < value1.get_number()),
            Operator::GreaterThan => Type::Bool(value0.get_number() > value1.get_number()),
        }
    }
}
