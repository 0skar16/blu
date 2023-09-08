use std::hash::Hash;


#[derive(Debug, Clone, Hash)]
pub struct AST {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Hash)]
pub enum Statement {
    Call(Box<Statement>, Vec<Statement>, bool),
    Get(String),
    Child(Box<Statement>, Box<Statement>),
    Method(Box<Statement>, String),
    Let(LetTarget, Option<Box<Statement>>),
    Global(String, Option<Box<Statement>>),
    Assignment(Box<Statement>, Box<Statement>),
    Operation(Box<Statement>, Operation, Option<Box<Statement>>),
    Comment(CommentType, String),
    Literal(Literal),
    Return(Vec<Statement>),
    Table(Vec<(TableIndex, Statement)>),
    Function(Option<Box<Statement>>, Vec<String>, Block, bool),
    For(BluIterator, Block),
    While(Box<Statement>, Block),
    Loop(Box<Statement>, Block),
    If(Box<Statement>, Block, Vec<(Statement, Block)>, Option<Block>),
    Paren(Box<Statement>),
    Index(Box<Statement>, Box<Statement>),
    LoopOperation(LoopOp),
    Nil,
}
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum LetTarget {
    ID(String),
    Unwrap(Vec<UnwrapTarget>),
}
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum UnwrapTarget {
    ID(String),
    Unwrap(Vec<UnwrapTarget>, String),
    Default(String),
}
#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum LoopOp {
    Break,
    Continue,
}
#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum CommentType {
    SingleLine,
    MultiLine
}
#[derive(Debug, Clone, Hash)]
pub enum TableIndex {
    None,
    Ident(String),
    Literal(Literal),
    Statement(Statement),
}
#[derive(Debug, Clone, Hash)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone, PartialEq, Hash)]
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

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::Number(n) => n.to_bits().hash(state),
            Literal::String(s) => s.hash(state),
            Literal::Boolean(b) => b.hash(state),
            Literal::Slice(s) => s.hash(state),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum BluIterator {
    Numerical(String, Box<Statement>, Box<Statement>, Option<Box<Statement>>),
    Each(Vec<String>, Box<Statement>),
    Iterator(Vec<String>, Box<Statement>),
} 