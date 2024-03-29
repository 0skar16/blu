use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AST {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Statement {
    Call(Box<Statement>, Vec<Statement>, bool),
    Get(Rc<str>),
    Child(Box<Statement>, Box<Statement>),
    Method(Box<Statement>, Rc<str>),
    Let(Vec<LetTarget>, Vec<Type>, Option<Box<Statement>>),
    Global(Vec<Rc<str>>, Vec<Type>, Option<Box<Statement>>),
    Assignment(Box<Statement>, Box<Statement>),
    Operation(Box<Statement>, Operation, Option<Box<Statement>>),
    Literal(Literal),
    Return(Vec<Statement>),
    Table(Vec<(TableIndex, Statement)>),
    Function(Option<Box<Statement>>, Vec<(Rc<str>, Type)>, Block, bool),
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
    Import(ImportTarget, Rc<str>),
    Export(Box<Statement>, Rc<str>),
    Match(
        Box<Statement>,
        Vec<(Vec<Literal>, MatchOutput)>,
        Option<MatchOutput>,
        bool,
    ),
    Null,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Str,
    Array(Rc<Type>),
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Custom(Rc<str>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MatchOutput {
    Block(Block),
    Statement(Box<Statement>),
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum LetTarget {
    ID(Rc<str>),
    Unwrap(Vec<UnwrapTarget>),
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ImportTarget {
    Default(Rc<str>),
    Unwrap(Vec<UnwrapTarget>),
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum UnwrapTarget {
    ID(Rc<str>),
    Unwrap(Vec<UnwrapTarget>, Rc<str>),
    ReassignID(Rc<str>, Rc<str>),
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
    Ident(Rc<str>),
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
    String(Rc<str>),
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
        Rc<str>,
        Box<Statement>,
        Box<Statement>,
        Option<Box<Statement>>,
    ),
    Each(Vec<Rc<str>>, Box<Statement>),
    Iterator(Vec<Rc<str>>, Box<Statement>),
}
