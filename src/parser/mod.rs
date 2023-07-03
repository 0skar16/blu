
use std::process::exit;
use std::sync::atomic::AtomicU8;

use anyhow::{Result, bail};
use colored::Colorize;
use nom::bytes::complete::tag;
use nom::character::is_digit;
use nom::error::ErrorKind;
use nom::{*, character::complete::char};
use nom::branch::alt;
use crate::{s, pss};
use self::ast::{AST, Statement, Block, Literal, Operation, TableEntry, BluIterator, LoopOp};
pub mod ast;
use nom::sequence::delimited;
use nom::multi::{separated_list0, many0};
use nom::bytes::complete::take_while1;
use nom::bytes::complete::take_until;
pub fn parse_blu(mut code: &str, filename: String) -> Result<AST> {
    let mut statements = vec![];
    let line_num = code.to_string().clone().matches("\n").count();
    while let Ok((c, statement)) = parse_standalone_statement(code) {
        code = c;
        statements.push(statement);
    }
    if let Err(e) = parse_standalone_statement(code) {
        match e {
            Err::Incomplete(_) => {},
            Err::Error(e) => {
                let until: Vec<&str> = code.split(" ").collect();
                
                let current_line_num = code.matches("\n").count();

                let code = match e.code {
                    ErrorKind::Tag => format!("Couldn't recognize tag [{}]", until[0]),
                    _ => return Ok(AST { statements }),

                };
                println!("{}: Parsing errored at {}@{}, `{}`", "error".red().bold(), filename, line_num-current_line_num+1, code);
                exit(1);
            },
            Err::Failure(_) => bail!(e.to_string()),
        }
    }
    Ok(AST {
        statements
    })
}

fn parse_semi_ended_statement(code: &str) -> IResult<&str, Statement> {
    let (code, statement) = pss!(alt((
        parse_return,
        parse_loop_operation,
        parse_call,
        parse_let,
        parse_global,
        parse_assignment,
    )))(code)?;
    let (code, _) = pss!(tag(";"))(code)?;
    
    Ok((code, statement))
}
fn parse_standalone_statement(code: &str) -> IResult<&str, Statement> {
    let code = skip_comment(code);
    let (code, mut statement) = pss!(alt((
        parse_if,
        parse_while,
        parse_loop,
        parse_func,
        parse_for,
        parse_semi_ended_statement,
)   ))(code)?;
    match &mut statement {
        Statement::Call(_, _, standalone) => *standalone = true,
        Statement::Function(_, _, _, standalone) => *standalone = true,
        _ => {}
    }
    Ok((code, statement))
}


fn parse_statement(code: &str) -> IResult<&str, Statement> {
    let code = skip_comment(code);
    let (code, statement) = pss!(alt((
            parse_func,
            
            parse_op,
            parse_table,
            parse_index,
            parse_call,
            parse_method,
            parse_paren,
            parse_nil,
            parse_literal,
            
            parse_child,
            parse_get,
    )))(code)?;
    Ok((code, statement))
}

fn parse_for(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("for"))(code)?;
    let (code, iter) = alt((
        |code| {
            let (code, iterative) = s!(pss!(parse_ident))(code)?;
            let (code, _) = tag("in")(code)?;
            let (code, start) = pss!(parse_non_op_statement)(code)?;
            let (code, _) = tag("..")(code)?;
            let (code, end) = pss!(parse_non_op_statement)(code)?;
            let (code, step) = if let Ok((_code, step)) = parse_step(code) {
                (_code, Some(Box::new(step)))
            }else{
                (code, None)
            };
            Ok((code, BluIterator::Numerical(iterative, Box::new(start), Box::new(end), step)))
        },
        |code| {
            let (code, iteratives) = nom::multi::separated_list1(tag(","), s!(pss!(parse_ident)))(code)?;
            let (code, _) = pss!(tag("in"))(code)?;
            let (code, iter) = pss!(parse_statement)(code)?;
            Ok((code, BluIterator::Each(iteratives, Box::new(iter))))
        },
        |code| {
            let (code, iteratives) = nom::multi::separated_list1(tag(","), s!(pss!(parse_ident)))(code)?;
            let (code, _) = pss!(tag("iter"))(code)?;
            let (code, iter) = pss!(parse_statement)(code)?;
            Ok((code, BluIterator::Iterator(iteratives, Box::new(iter))))
        }
    ))(code)?;
    let (code, block) = parse_block(code)?;
    Ok((code, Statement::For(iter, block)))
}
fn parse_step(code: &str) -> IResult<&str, Statement> {
    let (code, _): (&str, &str) = tag(",")(code)?;
    let (code, step) = parse_non_op_statement(code)?;
    Ok((code, step)) 
}
fn parse_while(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("while"))(code)?;
    let (code, statement) = pss!(delimited(char('('), pss!(parse_statement), char(')')))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    Ok((code, Statement::While(Box::new(statement), block)))
}
fn parse_loop(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("loop"))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    let (code, _) = pss!(tag("until"))(code)?;
    let (code, statement) = pss!(delimited(char('('), pss!(parse_statement), char(')')))(code)?;
    Ok((code, Statement::Loop(Box::new(statement), block)))
}
fn parse_loop_operation(code: &str) -> IResult<&str, Statement> {
    let (code, op) = pss!(alt((
        tag("break"),
        tag("continue"),
    )))(code)?;
    let op = match op {
        "break" => LoopOp::Break,
        "continue" => LoopOp::Continue,
        _ => unreachable!()
    };
    Ok((code, Statement::LoopOperation(op)))
}
fn parse_child(code: &str) -> IResult<&str, Statement> {
    let (code, parent) = pss!(parse_non_child_statement)(code)?;
    let (code, _) = tag(".")(code)?;
    let (code, child) = pss!(alt((
        parse_child,
        parse_get
    )))(code)?;
    
    Ok((code, Statement::Child(Box::new(parent), Box::new(child))))
}

fn parse_non_child_statement(code: &str) -> IResult<&str, Statement> {
    let code = skip_comment(code);
    let (code, statement) = pss!(alt((
        parse_paren,
        parse_get,
        parse_literal,
  )))(code)?;
  Ok((code, statement))
}
fn parse_call(code: &str) -> IResult<&str, Statement> {
    let (code, subject) = pss!(parse_non_call_statement)(code)?;
    let (code, args) = pss!(delimited(char('('), separated_list0(char(','), pss!(parse_statement)), char(')')))(code)?;
    Ok((code, Statement::Call(Box::new(subject), args, false)))
}
fn parse_non_call_statement(code: &str) -> IResult<&str, Statement> {
    let code = skip_comment(code);
    let (code, statement) = pss!(alt((
        parse_paren,
        parse_method,
        parse_child,
        parse_get,
  )))(code)?;
  Ok((code, statement))
}
fn parse_method(code: &str) -> IResult<&str, Statement> {
    let (code, parent) = pss!(alt((
        parse_literal,
        parse_paren,
        parse_child,
        parse_get,
    )))(code)?;
    let (code, _) = tag(":")(code)?;
    let (code, method) = s!(pss!(parse_ident))(code)?;
    Ok((code, Statement::Method(Box::new(parent), method)))
}
fn parse_non_op_statement(code: &str) -> IResult<&str, Statement> {
    let code = skip_comment(code);
    let (code, statement) = pss!(alt((
        parse_index,
        parse_paren,
        parse_literal,
        parse_call,
        parse_child,
        parse_get,
  )))(code)?;
  Ok((code, statement))
}
fn parse_if(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("if"))(code)?;
    let (code, statement) = pss!(delimited(char('('), pss!(parse_statement), char(')')))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    let (mut code, elseifs) = many0(pss!(parse_elseif))(code)?;
    let mut _else = None;
    if let Ok((_code, __else)) = parse_else(code) {
        code = _code;
        _else = Some(__else);
    }
    Ok((code, Statement::If(Box::new(statement), block, elseifs, _else)))
}

fn parse_elseif(code: &str) -> IResult<&str, (Statement, Block)> {
    let (code, _) = pss!(tag("else if"))(code)?;
    let (code, statement) = pss!(delimited(char('('), pss!(parse_statement), char(')')))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    Ok((code, (statement, block)))
}
fn parse_else(code: &str) -> IResult<&str, Block> {
    let (code, _) = pss!(tag("else"))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    Ok((code, block))
}
fn parse_index(code: &str) -> IResult<&str, Statement> {
    let (code, indexed) = pss!(parse_non_index_statement)(code)?;
    let (code, _) = pss!(char('['))(code)?;
    let (code, indexer) = parse_statement(code)?;
    let (code, _) = pss!(char(']'))(code)?;
    Ok((code, Statement::Index(Box::new(indexed), Box::new(indexer))))
}
fn parse_non_index_statement(code: &str) -> IResult<&str, Statement> {
    let code = skip_comment(code);
    let (code, statement) = pss!(alt((
        parse_paren,
        parse_literal,
        parse_call,
        parse_child,
        parse_get,
  )))(code)?;
  Ok((code, statement))
}
fn parse_op(code: &str) -> IResult<&str, Statement> {
    let (code, operand) = if let Ok((code, operand)) = pss!(parse_non_op_statement)(code) {
        (code, Some(operand))
    }else{
        (code, None)
    };
    let (code, op) = pss!(alt((
        tag("=="),
        tag("!="),
        tag(">"),
        tag("<"),
        tag(">="),
        tag("<="),
        tag("%"),
        tag("+"),
        tag("-"),
        tag("/"),
        tag("*"),
        tag("^"),
        tag("!"),
        tag(".."),
        tag("&&"),
        tag("||"),
    )))(code)?;
    let op = match op {
        "==" => Operation::Equal,
        "!=" => Operation::NotEqual,
        ">" => Operation::Greater,
        "<" => Operation::Lesser,
        ">=" => Operation::GE,
        "<=" => Operation::LE,
        "%" => Operation::Mod,
        "+" => Operation::Add,
        "-" => Operation::Sub,
        "/" => Operation::Div,
        "*" => Operation::Mul,
        "^" => Operation::Exp,
        "!" => Operation::Not,
        ".." => Operation::StringAdd,
        "&&" => Operation::And,
        "||" => Operation::Or,
        _ => unreachable!(),
    };
    let (code, (operand, operator)) = if operand.is_none() {
        let (code, statement) = pss!(parse_statement)(code)?;
        
        (code, (statement, None))
    }else {
        let (code, operator) = pss!(parse_statement)(code)?;
        (code, (operand.unwrap(), Some(Box::new(operator))))
    };
    Ok((code, Statement::Operation(Box::new(operand), op, operator)))
}
fn parse_paren(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(char('('))(code)?;
    let (code, stmt) = parse_statement(code)?;
    let (code, _) = pss!(char(')'))(code)?;
    Ok((code, Statement::Paren(Box::new(stmt))))
}
fn parse_get(code: &str) -> IResult<&str, Statement> {
    let (code, ident) = s!(parse_ident)(code)?;
    Ok((code, Statement::Get(ident)))
}
fn parse_let(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("let"))(code)?;
    let (mut code, name) = s!(pss!(parse_ident))(code)?;
    let mut value = None;
    if let Ok((_code, _value)) = parse_possible_assign(code) {
        code = _code;
        value = _value;
    }
    Ok((code, Statement::Let(name, value)))
}
fn parse_global(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("global"))(code)?;
    let (mut code, name) = s!(pss!(parse_ident))(code)?;
    let mut value = None;
    if let Ok((_code, _value)) = parse_possible_assign(code) {
        code = _code;
        value = _value;
    }
    Ok((code, Statement::Global(name, value)))
}
fn parse_possible_assign(code: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (code, _) = pss!(tag("="))(code)?;
    let (code, statement) = pss!(parse_statement)(code)?;
    Ok((code, Some(Box::new(statement))))
}
fn parse_number(code: &str) -> IResult<&str, f64> {
    let (code, num): (&str, &str) = pss!(take_while1(is_number()))(code)?;
    let num = num.parse().map_err(|_| {
        nom::Err::Error(error::make_error("Couldn't parse number", ErrorKind::Digit))
    })?;
    Ok((code, num))
}
fn parse_int(code: &str) -> IResult<&str, i32> {
    let (code, num): (&str, &str) = pss!(take_while1(|c| is_digit(c as u8) || c == '-'))(code)?;
    let num = num.parse().map_err(|_| {
        nom::Err::Error(error::make_error("Couldn't parse number", ErrorKind::Digit))
    })?;
    Ok((code, num))
}
fn parse_literal(code: &str) -> IResult<&str, Statement> {
    let (code, lit) = pss!(alt((
        |code| {
            let (code, num) = parse_number(code)?; 
            Ok((code, Literal::Number(num)))
        },
        |code| {
            let (code, num) = parse_int(code)?; 
            Ok((code, Literal::Number(num.try_into().unwrap())))
        },
        |code| {
            let (code, _) = char('"')(code)?;
            let (code, string) = s!(take_until("\""))(code)?;
            let (code, _) = char('"')(code)?;
            Ok((code, Literal::String(string)))
        },
        |code| {
            let (code, stmts) = pss!(delimited(
                char('['),
                pss!(separated_list0(char(','), parse_statement)),
                char(']'),
            ))(code)?;
            Ok((code, Literal::Slice(stmts)))
        },
        |code| {
            let (code, boolean): (&str, &str) = pss!(alt((tag("true"), tag("false"))))(code)?;
            Ok((code, Literal::Boolean(match boolean {
                "true" => true,
                "false" => false,
                _ => false,
            })))
        }
    )))(code)?;
    Ok((code, Statement::Literal(lit)))
}
fn parse_assignment(code: &str) -> IResult<&str, Statement> {
    let (code, target) = pss!(alt((
        parse_child,
        parse_get
    )))(code)?;
    let (code, _) = pss!(tag("="))(code)?;
    let (code, source) = pss!(parse_statement)(code)?;
    Ok((code, Statement::Assignment(Box::new(target), Box::new(source))))
}
fn parse_comment(code: &str) -> IResult<&str, Statement> {
    pss!(alt((
        |code| {
            let (code, _): (&str, _) = tag("//")(code)?;
            let (code, message) = take_until("\n")(code)?;
            dbg!(message);
            Ok((code, Statement::Comment(ast::CommentType::SingleLine, message.to_string())))
        },
        |code| {
            let (code, _): (&str, _) = tag("/*")(code)?;
            let (code, message) = take_until("*/")(code)?;
            let (code, _) = pss!(tag("*/"))(code)?;
            Ok((code, Statement::Comment(ast::CommentType::MultiLine, message.to_string())))
        }
    )))(code)
}
fn is_number() -> impl Fn(char) -> bool {
    let atomic = AtomicU8::new(0);

    move |c: char| {
        let i = atomic.load(std::sync::atomic::Ordering::SeqCst);
        atomic.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if i == 0 {
            c.is_numeric()
        }else{
            c.is_numeric() || c == '.' || c == '-' || c == 'e' || c == '+'
        }
    }
}
fn parse_nil(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(alt((
        tag("nil"),
        tag("null"),
        tag("nul"),
        tag("()"),
        tag("none")
    )))(code)?;
    Ok((code, Statement::Nil))
}
fn parse_func(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("fn"))(code)?;
    let (code, name) = if let Ok((code, name)) = s!(pss!(parse_func_ident))(code) {
        (code, Some(name))
    }else{
        (code, None)
    };
    let (code, args) = pss!(delimited(char('('), separated_list0(char(','), s!(pss!(alt((
        parse_ident,
        |code| tag("...")(code)
    ))))), char(')')))(code)?;
    let (code, block) = pss!(parse_block)(code)?;

    Ok((code, Statement::Function(name, args, block, false)))
}
fn parse_return(code: &str) -> IResult<&str, Statement> {
    let (code, _) = pss!(tag("return"))(code)?;
    let (code, stmt) = pss!(parse_statement)(code)?;
    Ok((code, Statement::Return(Box::new(stmt))))
}
fn parse_table(code: &str) -> IResult<&str, Statement> {
    let (code, entries) = pss!(delimited(char('{'), nom::multi::separated_list0(tag(","), pss!(parse_table_entry)), char('}')))(code)?;
    let entries = entries.into_iter().filter_map(|e| e).collect();
    Ok((code, Statement::Table(entries)))
}
fn parse_table_entry(code: &str) -> IResult<&str, Option<TableEntry>> {
    alt((
        |code| {
            let (code, index): (&str, Statement) = pss!(parse_literal)(code)?;
            let (code, _) = pss!(tag("="))(code)?;
            let (code, stmt) = pss!(parse_statement)(code)?;
            Ok((code, Some(TableEntry::LiteralIndex(index, stmt))))
        },
        |code| {
            let (code, index): (&str, String) = s!(pss!(parse_ident))(code)?;
            let (code, _) = pss!(tag("="))(code)?;
            let (code, stmt) = pss!(parse_statement)(code)?;
            Ok((code, Some(TableEntry::IdentIndex(index, stmt))))
        },
        |code| {
            let (code, stmt): (&str, Statement) = pss!(parse_statement)(code)?;
            Ok((code, Some(TableEntry::IndexLess(stmt))))
        },
        |code| {
            Ok((code, None))
        }
    ))(code)
}
fn parse_block(code: &str) -> IResult<&str, Block> {
    let (code, stmts) = pss!(delimited(char('{'), nom::multi::many0(parse_standalone_statement), char('}')))
    (code)?;
    Ok((code, Block(stmts)))
}
fn parse_ident(code: &str) -> IResult<&str, &str> {
    take_while1(|c| is_ident(c))(code)
}
fn is_ident(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}
fn parse_func_ident(code: &str) -> IResult<&str, &str> {
    take_while1(|c| is_func_ident(c))(code)
}
fn is_func_ident(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '.' || c == ':' || c == '-'
}
fn skip_comment(mut code: &str) -> &str {
    while let Ok((_code, _)) = pss!(parse_comment)(code) {
        code = _code;
    }
    code
}
#[macro_export]
macro_rules! pss {
    ($par:expr) => {
        nom::sequence::delimited(
            nom::character::complete::multispace0,
            $par,
            nom::character::complete::multispace0,
        )
    };
}
#[macro_export]
macro_rules! s {
    ($par:expr) => {
        |i| $par(i).map(|x: (&str, &str)| (x.0, x.1.to_string()))
    };
}