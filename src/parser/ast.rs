use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AST {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Statement {
    Call(Box<Statement>, Vec<Statement>, bool),
    Get(String),
    Child(Box<Statement>, Box<Statement>),
    Method(Box<Statement>, String),
    Let(Vec<LetTarget>, Option<Box<Statement>>),
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
    If(
        Box<Statement>,
        Block,
        Vec<(Statement, Block)>,
        Option<Block>,
    ),
    Paren(Box<Statement>),
    Index(Box<Statement>, Box<Statement>),
    LoopOperation(LoopOp),
    Import(ImportTarget, String),
    Export(Box<Statement>, String),
    Match(
        Box<Statement>,
        Vec<(Vec<Literal>, MatchOutput)>,
        Option<MatchOutput>,
        bool,
    ),
    Nil,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MatchOutput {
    Block(Block),
    Statement(Box<Statement>),
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum LetTarget {
    ID(String),
    Unwrap(Vec<UnwrapTarget>),
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ImportTarget {
    Default(String),
    Unwrap(Vec<UnwrapTarget>),
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum UnwrapTarget {
    ID(String),
    Unwrap(Vec<UnwrapTarget>, String),
    ReassignID(String, String),
}
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum LoopOp {
    Break,
    Continue,
}
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum CommentType {
    SingleLine,
    MultiLine,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TableIndex {
    None,
    Ident(String),
    Literal(Literal),
    Statement(Statement),
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block(pub Vec<Statement>);

impl DerefMut for Block {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Deref for Block {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
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
    Len,
    Arrow,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Slice(Vec<Statement>),
}

impl Eq for Literal {}

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BluIterator {
    Numerical(
        String,
        Box<Statement>,
        Box<Statement>,
        Option<Box<Statement>>,
    ),
    Each(Vec<String>, Box<Statement>),
    Iterator(Vec<String>, Box<Statement>),
}
