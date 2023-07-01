
#[derive(Debug, Clone)]
pub struct AST {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Call(Box<Statement>, Vec<Statement>),
    Get(String),
    Child(Box<Statement>, String),
    Let(String, Option<Box<Statement>>),
    Global(String, Box<Statement>),
    Assignment(String, Box<Statement>),
    Operation(Box<Statement>, Operation, Box<Statement>),
    Comment(String),
    Literal(Literal),
    Return(Box<Statement>),
    Table(Vec<TableEntry>),
    Function(String, Vec<String>, Block),
    For(BluIterator, Block),
    While(Box<Statement>, Block),
    If(Box<Statement>, Block, Vec<(Statement, Block)>, Option<Block>),
    Paren(Box<Statement>),
    Index(Box<Statement>, Box<Statement>),
    Nil,
}
#[derive(Debug, Clone)]
pub enum TableEntry {
    IndexLess(Box<Statement>),
    IdentIndex(String, Box<Statement>),
    NumericIndex(i128),
}
#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    Lesser,
    GE,
    LE,
    Exp,
    Not,
    StringAdd,
}
#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Slice(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub enum BluIterator {
    Numerical(String, f64, f64),
    Each(Vec<String>, Box<Statement>),
}