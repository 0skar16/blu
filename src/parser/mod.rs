use std::os::linux::raw::stat;

use anyhow::Result;
use nom::bytes::complete::tag;
use nom::{*, character::complete::char};
use nom::branch::alt;
use crate::{s, pss};
use self::ast::{AST, Statement, Block, Literal, Operation};
pub mod ast;
use nom::sequence::delimited;
use nom::multi::{separated_list0, many0};
use nom::bytes::complete::take_while1;
use nom::bytes::complete::take_until;
pub fn parse_blu(mut code: &str) -> Result<AST> {
    let mut statements = vec![];
    while let Ok((c, statement)) = parse_statement(code) {
        code = c;
        statements.push(statement);
    }
    Ok(AST {
        statements
    })
}

fn parse_statement(code: &str) -> IResult<&str, Statement> {
    let (code, statement) = pss!(alt((
            parse_if,
            parse_func,
            parse_let,
            //parse_op,
            parse_literal,
            parse_call,
            //parse_get,
    )))(code)?;
    Ok((code, statement))
}

fn parse_call(code: &str) -> IResult<&str, Statement> {
    let (code, subject) = pss!(parse_non_call_statement)(code)?;
    let (code, args) = pss!(delimited(char('('), separated_list0(char(','), pss!(parse_statement)), char(')')))(code)?;
    let (code, _) = pss!(alt((tag(";"), tag("\n"), tag("\r\n"))))(code)?;
    Ok((code, Statement::Call(Box::new(subject), args)))
}
fn parse_non_call_statement(code: &str) -> IResult<&str, Statement> {
    let (code, statement) = pss!(alt((
        parse_get,
  )))(code)?;
  Ok((code, statement))
}
fn parse_non_op_statement(code: &str) -> IResult<&str, Statement> {
    let (code, statement) = pss!(alt((
        parse_literal,
        parse_call,
        parse_get,
  )))(code)?;
  println!("??%#%: {:?}", statement);
  Ok((code, statement))
}
fn parse_if(code: &str) -> IResult<&str, Statement> {
    println!("GOT IT?");
    let (code, _) = pss!(tag("if"))(code)?;
    let (code, statement) = pss!(delimited(char('('), pss!(parse_statement), char(')')))(code)?;
    dbg!(&statement);
    let (code, block) = pss!(parse_block)(code)?;
    //let (mut code, elseifs) = many0(pss!(parse_elseif))(code)?;
    let elseifs = vec![];
    let mut _else = None;
    //if let Ok((_code, __else)) = parse_else(code) {
    //    code = _code;
    //    _else = Some(__else);
    //}
    Ok((code, Statement::If(Box::new(statement), block, elseifs, _else)))
}

fn parse_elseif(code: &str) -> IResult<&str, (Statement, Block)> {
    let (code, _) = pss!(tag("elseif"))(code)?;
    let (code, statement) = pss!(delimited(char('('), pss!(parse_statement), char(')')))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    Ok((code, (statement, block)))
}
fn parse_else(code: &str) -> IResult<&str, Block> {
    let (code, _) = pss!(tag("else"))(code)?;
    let (code, block) = pss!(parse_block)(code)?;
    Ok((code, block))
}

fn parse_op(code: &str) -> IResult<&str, Statement> {
    println!("??::");
    let (code, operand) = pss!(parse_non_op_statement)(code)?;
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
        "+" => Operation::Add,
        ">" => Operation::Greater,
        "&&" => Operation::And,
        "!" => Operation::Not,
        ".." => Operation::StringAdd,
        _ => unreachable!(),
    };
    let (code, operator) = pss!(parse_statement)(code)?;
    Ok((code, Statement::Operation(Box::new(operand), op, Box::new(operator))))
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
    let (code, _) = pss!(alt((tag(";"), tag("\n"), tag("\r\n"))))(code)?;
    Ok((code, Statement::Let(name, value)))
}
fn parse_possible_assign(code: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (code, _) = pss!(tag("="))(code)?;
    let (code, statement) = pss!(parse_statement)(code)?;
    Ok((code, Some(Box::new(statement))))
}
fn parse_literal(code: &str) -> IResult<&str, Statement> {
    let (code, lit) = pss!(alt((
        |code| {
            let (code, num): (&str, &str) = pss!(take_while1(is_number))(code)?;
            let num = num.parse().unwrap();
            Ok((code, Literal::Number(num)))
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
fn is_number(c:char) -> bool {
    c.is_numeric() || c == '.' || c == '-' || c == 'e'
}
fn parse_func(code: &str) -> IResult<&str, Statement> {
    println!("TF HAPPENED?");
    let (code, _) = pss!(tag("fn"))(code)?;
    let (code, name) = s!(pss!(parse_ident))(code)?;
    let (code, args) = pss!(delimited(char('('), separated_list0(char(','), s!(pss!(parse_ident))), char(')')))(code)?;
    let (code, block) = pss!(parse_block)(code)?;

    Ok((code, Statement::Function(name, args, block)))
}

fn parse_block(code: &str) -> IResult<&str, Block> {
    println!("parsing block");
    let (code, stmts) = pss!(delimited(char('{'), nom::multi::many0(parse_statement), char('}')))
    (code)?;
    Ok((code, Block(stmts)))
}
fn parse_ident(code: &str) -> IResult<&str, &str> {
    take_while1(|c| is_ident(c))(code)
}
fn is_ident(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
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