use std::{collections::HashMap, rc::Rc};

use crate::lexer::{Number, Punctuation, Token, TokenKind, TokenKindDesc};

use self::ast::{
    Block, BluIterator, ImportTarget, LetTarget, Literal, LoopOp, MatchOutput, Operation,
    Statement, TableIndex, Type, UnwrapTarget, AST,
};
pub mod ast;
#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedTokenEx(Token, TokenKindDesc),
    UnexpectedTokenExKind(Token, TokenKind),
    UnexpectedEos(u32, u32),
    UnexpectedEosEx(u32, u32, TokenKindDesc),
    UnexpectedEosExKind(u32, u32, TokenKind),
    DoubleDefaultCase(u32, u32),
    DefaultUnreassigned(u32, u32),
}
pub type Result<T> = std::result::Result<T, ParserError>;
pub struct Parser {
    pos: usize,
    token_stream: Rc<[Token]>,
}

impl Parser {
    pub fn new(token_stream: impl Into<Rc<[Token]>>) -> Self {
        Self {
            pos: 0,
            token_stream: token_stream.into(),
        }
    }
    pub fn parse(mut self) -> Result<AST> {
        let mut statements = vec![];
        while self.token_stream.len() - self.pos > 0 {
            statements.push(self.parse_standalone_statement(self.token_stream.len())?);
        }
        Ok(AST { statements })
    }
    pub fn parse_standalone_statement(&mut self, end: usize) -> Result<Statement> {
        let tok = self.peek(0, end)?;
        let pos = self.pos;
        if let Ok(assignment) = self.parse_assignment(end) {
            return Ok(assignment);
        } else {
            self.pos = pos;
        }
        if let Ok(call) = self.parse_call(end, true, None) {
            return Ok(call);
        } else {
            self.pos = pos;
        }
        Ok(match &tok.token {
            TokenKind::ID(id) => match &(**id) {
                "fn" => self.parse_function(end, true)?,
                "let" => self.parse_let(end)?,
                "if" => self.parse_if(end)?,
                "return" => self.parse_return(end)?,
                "for" => self.parse_for(end)?,
                "global" => self.parse_global(end)?,
                "while" => self.parse_while(end)?,
                "loop" => self.parse_loop(end)?,
                "break" | "continue" => self.parse_loopop(end)?,
                "import" => self.parse_import(end)?,
                "export" => self.parse_export(end)?,
                "match" => self.parse_match(end, true)?,
                _ => return Err(ParserError::UnexpectedToken(tok)),
            },
            _ => return Err(ParserError::UnexpectedTokenEx(tok, TokenKindDesc::ID)),
        })
    }

    pub fn parse_statement(&mut self, end: usize) -> Result<Statement> {
        let tok = self.peek(0, end)?;
        if tok.token == TokenKind::Punctuation(Punctuation::Not) {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Not))?;
            let st = self.parse_statement(end)?;
            return Ok(Statement::Operation(Box::new(st), Operation::Not, None));
        }
        if tok.token == TokenKind::Punctuation(Punctuation::Hash) {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Hash))?;
            let st = self.parse_statement(end)?;
            return Ok(Statement::Operation(Box::new(st), Operation::Len, None));
        }
        let mut stmt = match &tok.token {
            TokenKind::ID(id) => match &(**id) {
                "fn" => self.parse_function(end, false)?,
                "match" => self.parse_match(end, false)?,
                "false" | "true" => self.parse_literal(end)?,
                "nil" | "nul" | "null" | "none" => {
                    self.eat_ex(end, TokenKindDesc::ID)?;
                    Statement::Null
                },
                _ => {
                    self.eat_ex(end, TokenKindDesc::ID)?;
                    Statement::Get(id.clone())
                }
            },
            TokenKind::Number(_) | TokenKind::String(_) => self.parse_literal(end)?,
            TokenKind::Punctuation(p) => match *p {
                Punctuation::LeftSBracket => self.parse_slice(end)?,
                Punctuation::LeftBracket => self.parse_table(end)?,
                Punctuation::Sub => {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Sub))?;
                    let num = self.eat_ex(end, TokenKindDesc::Number)?;
                    match num.token {
                        TokenKind::Number(num) => Statement::Literal(Literal::Number(-match num {
                            Number::Float(f) => f,
                            Number::Int(i) => i as f64,
                            Number::Hex(h) => h as f64,
                        })),
                        _ => unreachable!(),
                    }
                }
                Punctuation::LeftParen => {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
                    let _end = self.isolate_block(end)?;
                    let stmt = self.parse_statement(_end)?;
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
                    Statement::Paren(Box::new(stmt))
                }
                _ => return Err(ParserError::UnexpectedToken(tok)),
            },
            _ => {
                return Err(ParserError::UnexpectedToken(tok));
            }
        };
        loop {
            if let Ok(tok) = self.peek(0, end) {
                stmt = match tok.token {
                    TokenKind::Punctuation(p) => {
                        if p.is_operation()
                            && !(p == Punctuation::Dot
                                && !self
                                    .peek(1, end)
                                    .map(|t| t.token == TokenKind::Punctuation(Punctuation::Dot))
                                    .unwrap_or(false))
                        {
                            if p == Punctuation::Equals && let Ok(tok) = self.peek(1, end) && tok.token != TokenKind::Punctuation(Punctuation::Equals) {
                            break
                        }
                            self.parse_operation(end, stmt)?
                        } else {
                            match p {
                                Punctuation::LeftParen => {
                                    self.parse_call(end, false, Some(stmt))?
                                }
                                Punctuation::LeftSBracket => self.parse_index(end, stmt)?,
                                Punctuation::Dot => self.parse_child(end, stmt)?,
                                Punctuation::Colon => self.parse_method(end, stmt)?,
                                _ => break,
                            }
                        }
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }
        Ok(stmt)
    }

    fn parse_loopop(&mut self, end: usize) -> Result<Statement> {
        let op = self.eat_ex(end, TokenKindDesc::ID)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        if let TokenKind::ID(id) = op.token.clone() {
            match &(*id) {
                "break" => return Ok(Statement::LoopOperation(LoopOp::Break)),
                "continue" => return Ok(Statement::LoopOperation(LoopOp::Continue)),
                _ => return Err(ParserError::UnexpectedToken(op)),
            }
        }

        unreachable!()
    }
    fn parse_loop(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("loop".into()))?;
        let block = self.parse_block(end)?;
        self.eat_ex_kind(end, TokenKind::ID("until".into()))?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
        let _end = self.isolate_block(end)?;
        let stmt = self.parse_statement(_end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
        Ok(Statement::Loop(Box::new(stmt), block))
    }
    fn parse_while(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("while".into()))?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
        let _end = self.isolate_block(end)?;
        let stmt = self.parse_statement(_end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
        let block = self.parse_block(end)?;
        Ok(Statement::While(Box::new(stmt), block))
    }
    fn parse_literal(&mut self, end: usize) -> Result<Statement> {
        Ok(match self.peek(0, end)?.token {
            TokenKind::Number(num) => {
                self.eat_ex(end, TokenKindDesc::Number)?;
                Statement::Literal(Literal::Number(match num {
                    Number::Float(f) => f,
                    Number::Int(i) => i as f64,
                    Number::Hex(h) => h as f64,
                }))
            }
            TokenKind::String(s) => {
                self.eat_ex(end, TokenKindDesc::String)?;
                Statement::Literal(Literal::String(s.clone()))
            }
            TokenKind::ID(id) => match &(*id) {
                "true" => {
                    self.eat_ex(end, TokenKindDesc::ID)?;
                    Statement::Literal(Literal::Boolean(true))
                }
                "false" => {
                    self.eat_ex(end, TokenKindDesc::ID)?;
                    Statement::Literal(Literal::Boolean(false))
                },
                _ => return Err(ParserError::UnexpectedToken(self.peek(0, end)?)),
            },
            _ => return Err(ParserError::UnexpectedToken(self.peek(0, end)?)),
        })
    }
    fn parse_index(&mut self, end: usize, indexed: Statement) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftSBracket))?;
        let _end = self.isolate_block(end)?;
        let index = self.parse_statement(_end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightSBracket))?;
        Ok(Statement::Index(Box::new(indexed), Box::new(index)))
    }
    fn parse_type(&mut self, end: usize) -> Result<Type> {
        let tok = self.eat(end)?;
        Ok(match tok.token {
            TokenKind::Punctuation(Punctuation::LeftSBracket) => {
                let _end = self.to_last(end, TokenKind::Punctuation(Punctuation::RightSBracket))?;
                let _type = self.parse_type(_end)?;
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightSBracket))?;
                Type::Array(Rc::new(_type))
            }
            TokenKind::ID(id) => match &(*id) {
                "any" => Type::Any,
                "str" => Type::Str,
                "f32" => Type::F32,
                "f64" => Type::F64,
                "i8" => Type::I8,
                "i16" => Type::I16,
                "i32" => Type::I32,
                "i64" => Type::I64,
                "u8" => Type::U8,
                "u16" => Type::U16,
                "u32" => Type::U32,
                "u64" => Type::U64,
                _ => Type::Custom(id),
            },
            _ => return Err(ParserError::UnexpectedToken(tok)),
        })
    }
    fn parse_global(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("global".into()))?;
        let mut ids = vec![];
        loop {
            ids.push(match self.eat_ex(end, TokenKindDesc::ID)?.token {
                TokenKind::ID(s) => s,
                _ => unreachable!(),
            });
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        let mut types = vec![];
        if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Colon) {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Colon))?;
            for i in 0..ids.len() {
                if i != 0 {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
                }
                let _end = self
                    .to_first(end, TokenKind::Punctuation(Punctuation::Comma))
                    .unwrap_or(end);
                types.push(self.parse_type(_end)?);
            }
        } else {
            for _ in 0..ids.len() {
                types.push(Type::Any);
            }
        }
        let assignment = {
            if let Ok(t) = self.peek(0, end) && t.token == TokenKind::Punctuation(Punctuation::Equals) {
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
                let end = self.to_first(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
                let statement = self.parse_statement(end)?;
                Some(Box::new(statement))
            }else{
                None
            }
        };

        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        Ok(Statement::Global(ids, types, assignment))
    }
    fn parse_table(&mut self, end: usize) -> Result<Statement> {
        let mut entries = vec![];
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftBracket))?;
        let _end = self.isolate_block(end)?;
        loop {
            let _end =
                self.to_first_minding_blocks(_end, TokenKind::Punctuation(Punctuation::Comma))?;
            if _end - self.pos <= 0 {
                break;
            }
            let __end =
                self.to_last_minding_blocks(_end, TokenKind::Punctuation(Punctuation::Equals))?;
            let index = if _end - __end <= 1 {
                TableIndex::None
            } else {
                let index = match self.peek(0, _end)?.token {
                    TokenKind::ID(id) => {
                        self.eat_ex(_end, TokenKindDesc::ID)?;
                        TableIndex::Ident(id)
                    }
                    TokenKind::Punctuation(Punctuation::LeftSBracket) => {
                        self.eat_ex_kind(_end, TokenKind::Punctuation(Punctuation::LeftSBracket))?;
                        let __end = self.isolate_block(_end)?;
                        let index = self.parse_statement(__end)?;
                        self.eat_ex_kind(_end, TokenKind::Punctuation(Punctuation::RightSBracket))?;
                        TableIndex::Statement(index)
                    }
                    _ => {
                        let lit = match self.parse_literal(_end)? {
                            Statement::Literal(lit) => lit,
                            _ => unreachable!(),
                        };
                        TableIndex::Literal(lit)
                    }
                };
                self.eat_ex_kind(_end, TokenKind::Punctuation(Punctuation::Equals))?;
                index
            };
            let value = self.parse_statement(_end)?;
            entries.push((index, value));
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightBracket))?;
        Ok(Statement::Table(entries))
    }
    fn parse_operation(&mut self, end: usize, mut operand: Statement) -> Result<Statement> {
        let mut mul_end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Mul))?;

        let div_end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Div))?;
        if div_end != self.pos && div_end < end {
            if mul_end != self.pos {
                mul_end = mul_end.min(div_end);
            } else {
                mul_end = div_end;
            }
        }

        let mod_end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Mod))?;
        if mod_end != self.pos && mod_end < end {
            if mul_end != self.pos {
                mul_end = mul_end.min(mod_end);
            } else {
                mul_end = mod_end;
            }
        }

        if mul_end != self.pos && mul_end < end {
            operand = self.parse_operation(mul_end, operand)?;
        }

        let mut add_end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Plus))?;

        let sub_end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Sub))?;
        if sub_end != self.pos && sub_end < end {
            if add_end != self.pos {
                add_end = add_end.min(sub_end);
            } else {
                add_end = sub_end;
            }
        }

        if add_end != self.pos && add_end < end {
            operand = self.parse_operation(add_end, operand)?;
        }

        let op = self.eat_ex(end, TokenKindDesc::Punctuation)?;
        let op = match op.token {
            TokenKind::Punctuation(punct) => match punct {
                Punctuation::Dot => {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot))?;
                    Operation::StringAdd
                }
                Punctuation::Mod => Operation::Mod,
                Punctuation::Mul => Operation::Mul,
                Punctuation::Sub => Operation::Sub,
                Punctuation::Div => Operation::Div,
                Punctuation::Plus => Operation::Add,
                Punctuation::Exp => Operation::Exp,
                Punctuation::And => Operation::And,
                Punctuation::Or => Operation::Or,
                Punctuation::Not => {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
                    Operation::NotEqual
                }
                Punctuation::LeftArrow => {
                    let tok = self.peek(0, end)?;
                    if tok.token == TokenKind::Punctuation(Punctuation::Equals) {
                        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
                        Operation::LE
                    } else {
                        Operation::Lesser
                    }
                }
                Punctuation::RightArrow => {
                    let tok = self.peek(0, end)?;
                    if tok.token == TokenKind::Punctuation(Punctuation::Equals) {
                        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
                        Operation::GE
                    } else {
                        Operation::Greater
                    }
                }
                Punctuation::Equals => {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
                    Operation::Equal
                }
                _ => return Err(ParserError::UnexpectedToken(op)),
            },
            _ => unreachable!(),
        };
        let operator = self.parse_statement(end)?;
        Ok(Statement::Operation(
            Box::new(operand),
            op,
            Some(Box::new(operator)),
        ))
    }
    fn parse_call(
        &mut self,
        end: usize,
        standalone: bool,
        called: Option<Statement>,
    ) -> Result<Statement> {
        let called = if let Some(called) = called {
            called
        } else {
            let _end = self.to_first(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
            self.parse_statement(_end)?
        };
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
        let _end = self.isolate_block(end)?;
        let mut args = vec![];
        loop {
            let _end = self.to_first_minding_blocks(_end, TokenKind::Punctuation(Punctuation::Comma))?;
            if _end - self.pos <= 0 {
                break;
            }
            args.push(self.parse_statement(_end)?);
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
        if standalone {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        }
        Ok(Statement::Call(Box::new(called), args, standalone))
    }
    fn parse_child(&mut self, end: usize, parent: Statement) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot))?;
        let id = self.eat_ex(end, TokenKindDesc::ID)?;
        let child = match id.token {
            TokenKind::ID(id) => Statement::Get(id),
            _ => unreachable!(),
        };
        Ok(Statement::Child(Box::new(parent), Box::new(child)))
    }
    fn parse_slice(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftSBracket))?;
        let _end = self.isolate_block(end)?;
        let mut elements = vec![];
        loop {
            let _end =
                self.to_first_minding_blocks(_end, TokenKind::Punctuation(Punctuation::Comma))?;
            if _end - self.pos <= 0 {
                break;
            }
            elements.push(self.parse_statement(_end)?);
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightSBracket))?;
        Ok(Statement::Literal(Literal::Slice(elements)))
    }
    fn parse_function(&mut self, end: usize, standalone: bool) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("fn".into()))?;
        let _end = self.to_first(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
        let name = if !standalone {
            if let Ok(stmt) = self.parse_statement(_end) {
                match stmt {
                    Statement::Child(parent, child) => Some(Statement::Child(parent, child)),
                    Statement::Method(parent, child) => Some(Statement::Method(parent, child)),
                    Statement::Get(id) => Some(Statement::Get(id)),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            let tk = self.peek(0, end)?;
            match self.parse_statement(_end)? {
                Statement::Child(parent, child) => Some(Statement::Child(parent, child)),
                Statement::Method(parent, child) => Some(Statement::Method(parent, child)),
                Statement::Get(id) => Some(Statement::Get(id)),
                _ => return Err(ParserError::UnexpectedToken(tk)),
            }
        };
        let name = name.map(|name| Box::new(name));

        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
        let mut args = vec![];
        loop {
            let mut arg = "".into();
            if let Ok(_arg) = self.eat_ex(end, TokenKindDesc::ID) {
                match _arg.token {
                    TokenKind::ID(_arg) => arg = _arg,
                    _ => unreachable!(),
                }
            } else if let Ok(_) = self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot)) {
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot))?;
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot))?;
                arg = "...".into();
            }
            let mut _type = Type::Any;
            if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Colon) {
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Colon))?;
                let _end = self
                    .to_first(end, TokenKind::Punctuation(Punctuation::Comma))
                    .unwrap_or(end);
                _type = self.parse_type(_end)?;
            }
            args.push((arg, _type));
            if self
                .eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))
                .is_err()
            {
                break;
            }
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
        let block = self.parse_block(end)?;
        Ok(Statement::Function(name, args, block, standalone))
    }
    fn parse_method(&mut self, end: usize, parent: Statement) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Colon))?;
        if let TokenKind::ID(id) = self.eat_ex(end, TokenKindDesc::ID)?.token {
            return Ok(Statement::Method(Box::new(parent), id));
        }
        unreachable!()
    }
    fn parse_let(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("let".into()))?;
        let mut targets = vec![];
        loop {
            targets.push(self.parse_let_target(end)?);
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        let mut types = vec![];
        if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Colon) {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Colon))?;
            for i in 0..targets.len() {
                if i != 0 {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
                }
                let _end = self
                    .to_first(end, TokenKind::Punctuation(Punctuation::Comma))
                    .unwrap_or(end);
                types.push(self.parse_type(_end)?);
            }
        } else {
            for _ in 0..targets.len() {
                types.push(Type::Any);
            }
        }
        let assignment = {
            if let Ok(t) = self.peek(0, end) && t.token == TokenKind::Punctuation(Punctuation::Equals) {
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
                let end = self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
                let statement = self.parse_statement(end)?;
                Some(Box::new(statement))
            }else{
                None
            }
        };

        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        Ok(Statement::Let(targets, types, assignment))
    }
    fn parse_let_target(&mut self, end: usize) -> Result<LetTarget> {
        if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::LeftBracket) {
            Ok(LetTarget::Unwrap(self.parse_unwrap(end, &HashMap::new())?))
        } else {
            Ok(LetTarget::ID(
                match self.eat_ex(end, TokenKindDesc::ID)?.token {
                    TokenKind::ID(id) => id,
                    _ => unreachable!(),
                },
            ))
        }
    }
    fn parse_import(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("import".into()))?;
        let target = self.parse_import_target(end)?;
        self.eat_ex_kind(end, TokenKind::ID("from".into()))?;
        let source = match self.eat_ex(end, TokenKindDesc::String)?.token {
            TokenKind::String(s) => s,
            _ => unreachable!(),
        };
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        Ok(Statement::Import(target, source))
    }
    fn parse_import_target(&mut self, end: usize) -> Result<ImportTarget> {
        if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::LeftBracket) {
            Ok(ImportTarget::Unwrap(self.parse_unwrap(
                end,
                &HashMap::from([("default".into(), "__default".into())]),
            )?))
        } else {
            Ok(ImportTarget::Default(
                match self.eat_ex(end, TokenKindDesc::ID)?.token {
                    TokenKind::ID(id) => id,
                    _ => unreachable!(),
                },
            ))
        }
    }
    fn parse_unwrap(
        &mut self,
        end: usize,
        change: &HashMap<Rc<str>, Rc<str>>,
    ) -> Result<Vec<UnwrapTarget>> {
        let mut entries = vec![];
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftBracket))?;
        let __end = self.isolate_block(end)?;

        loop {
            let _end =
                self.to_first_minding_blocks(__end, TokenKind::Punctuation(Punctuation::Comma))?;
            if _end - self.pos <= 0 {
                break;
            }
            entries.push(self.parse_unwrap_target(_end, change)?);
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightBracket))?;
        Ok(entries)
    }
    fn parse_unwrap_target(
        &mut self,
        end: usize,
        change: &HashMap<Rc<str>, Rc<str>>,
    ) -> Result<UnwrapTarget> {
        let mut id = match self.eat_ex(end, TokenKindDesc::ID)?.token {
            TokenKind::ID(id) => id,
            _ => unreachable!(),
        };
        if let Some(new_id) = change.get(&id) {
            id = new_id.clone();
        }
        let rid_tok = self.peek(0, end + 1)?;
        let replacement_id = if end > self.pos
            && self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Colon)
        {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Colon))?;
            if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::LeftBracket) {
                let unwrapped = self.parse_unwrap(end, change)?;
                return Ok(UnwrapTarget::Unwrap(unwrapped, id));
            }
            Some(match self.eat_ex(end, TokenKindDesc::ID)?.token {
                TokenKind::ID(id) => id,
                _ => unreachable!(),
            })
        } else {
            None
        };
        if id == "default".into() {
            if replacement_id.is_none() {
                return Err(ParserError::DefaultUnreassigned(rid_tok.line, rid_tok.col));
            }
        }
        Ok(if let Some(new_id) = replacement_id {
            UnwrapTarget::ReassignID(id, new_id)
        } else {
            UnwrapTarget::ID(id)
        })
    }
    fn parse_export(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("export".into()))?;
        let _end = self.to_first_minding_blocks(end, TokenKind::ID("as".into()))?;
        let source = self.parse_statement(_end)?;
        self.eat_ex_kind(end, TokenKind::ID("as".into()))?;
        let target = match self.eat_ex(end, TokenKindDesc::ID)?.token {
            TokenKind::ID(id) => id,
            _ => unreachable!(),
        };
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        Ok(Statement::Export(Box::new(source), target))
    }
    fn parse_if(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("if".into()))?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
        let _end = self.isolate_block(end)?;
        let stmt = self.parse_statement(_end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
        let block = self.parse_block(end)?;
        let mut elseifs = vec![];
        let mut _else = None;
        loop {
            if let Ok(t) = self.peek(0, end) && t.token == TokenKind::ID("else".into()) {
                self.eat_ex_kind(end, TokenKind::ID("else".into()))?;
                if self.peek(0, end)?.token == TokenKind::ID("if".into()) {
                    self.eat_ex_kind(end, TokenKind::ID("if".into()))?;
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftParen))?;
                    let _end = self.isolate_block(end)?;
                    let stmt = self.parse_statement(_end)?;
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightParen))?;
                    let block = self.parse_block(end)?;
                    elseifs.push((stmt, block));
                }else{
                    _else = Some(self.parse_block(end)?);
                    break;
                }
            }else{
                break;
            }
        }
        Ok(Statement::If(Box::new(stmt), block, elseifs, _else))
    }
    fn parse_assignment(&mut self, end: usize) -> Result<Statement> {
        let _end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        let __end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Equals))?;
        let subject = self.parse_statement(__end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Equals))?;
        let value = self.parse_statement(_end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        Ok(Statement::Assignment(Box::new(subject), Box::new(value)))
    }
    fn parse_return(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("return".into()))?;
        let __end =
            self.to_first_minding_blocks(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        let mut elements = vec![];
        loop {
            let _end = self.to_first(__end, TokenKind::Punctuation(Punctuation::Comma))?;
            if _end - self.pos <= 0 {
                break;
            }
            elements.push(self.parse_statement(_end)?);
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        Ok(Statement::Return(elements))
    }
    fn parse_for(&mut self, end: usize) -> Result<Statement> {
        self.eat_ex_kind(end, TokenKind::ID("for".into()))?;
        let _end = self.to_first(end, TokenKind::Punctuation(Punctuation::LeftBracket))?;
        let mut indexors = vec![];
        loop {
            indexors.push(match self.eat_ex(end, TokenKindDesc::ID)?.token {
                TokenKind::ID(id) => id,
                _ => unreachable!(),
            });
            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        if indexors.len() < 1 {
            self.peek(0, 0)?;
        }
        let tk = self.peek(0, end)?.token;
        let iterator = if tk == TokenKind::ID("in".into()) {
            self.eat_ex_kind(end, TokenKind::ID("in".into()))?;
            let mut __end = self.pos;
            let mut o = 0;
            let mut i = 0;
            loop {
                if __end >= end {
                    break;
                }
                let tok = self.peek(o, end)?;
                if TokenKindDesc::Punctuation == tok.token {
                    match tok.token {
                        TokenKind::Punctuation(punct) => match punct {
                            Punctuation::LeftBracket => i += 1,
                            Punctuation::RightBracket => i -= 1,
                            Punctuation::LeftSBracket => i += 1,
                            Punctuation::RightSBracket => i -= 1,
                            Punctuation::LeftParen => i += 1,
                            Punctuation::RightParen => i -= 1,
                            _ => {}
                        },
                        _ => {}
                    }
                }
                if tok.token == TokenKind::Punctuation(Punctuation::Dot) && let Ok(tok2) = self.peek(o+1, end) && tok2.token == TokenKind::Punctuation(Punctuation::Dot) && i == 0 {
                    break;
                }
                o += 1;
                __end += 1;
            }
            __end = __end.min(_end);
            let start = self.parse_statement(__end)?;
            if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Dot) {
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot))?;
                self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Dot))?;
                let _end = self.to_first(end, TokenKind::Punctuation(Punctuation::LeftBracket))?;
                let __end =
                    self.to_first_minding_blocks(_end, TokenKind::Punctuation(Punctuation::Comma))?;
                let i_end = self.parse_statement(__end)?;

                let step = if let Ok(t) = self.peek(0, end) && t.token == TokenKind::Punctuation(Punctuation::Comma) {
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Comma))?;
                    Some(Box::new(self.parse_statement(_end)?))
                }else{
                    None
                };
                BluIterator::Numerical(indexors[0].clone(), Box::new(start), Box::new(i_end), step)
            } else {
                BluIterator::Each(indexors, Box::new(start))
            }
        } else if tk == TokenKind::ID("iter".into()) {
            self.eat_ex_kind(end, TokenKind::ID("iter".into()))?;
            let iter = self.parse_statement(_end)?;
            BluIterator::Iterator(indexors, Box::new(iter))
        } else {
            return Err(ParserError::UnexpectedTokenEx(
                self.eat(end)?,
                TokenKindDesc::ID,
            ));
        };
        let block = self.parse_block(end)?;
        Ok(Statement::For(iterator, block))
    }
    fn parse_match(&mut self, end: usize, is_standalone: bool) -> Result<Statement> {
        let match_tok = self.eat_ex_kind(end, TokenKind::ID("match".into()))?;
        let input = self.parse_statement(end)?;
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftBracket))?;
        let _end = self.isolate_block(end)?;

        let mut cases = vec![];
        let mut default_case = None;

        while _end - self.pos > 0 {
            let __end =
                self.to_first_minding_blocks(_end, TokenKind::Punctuation(Punctuation::Comma))?;
            if self.peek(0, __end)?.token == TokenKind::ID("_".into()) {
                self.eat_ex_kind(__end, TokenKind::ID("_".into()))?;
                self.eat_ex_kind(__end, TokenKind::Punctuation(Punctuation::Equals))?;
                self.eat_ex_kind(__end, TokenKind::Punctuation(Punctuation::RightArrow))?;
                let output = self.parse_match_output(__end)?;
                if default_case.is_some() {
                    return Err(ParserError::DoubleDefaultCase(
                        match_tok.line,
                        match_tok.col,
                    ));
                }
                default_case = Some(output);
            } else {
                let mut inputs = vec![];
                loop {
                    let lit = match self.parse_literal(__end)? {
                        Statement::Literal(lit) => lit,
                        _ => unreachable!(),
                    };
                    inputs.push(lit);
                    if self.peek(0, __end)?.token != TokenKind::Punctuation(Punctuation::BitwiseOr)
                    {
                        break;
                    }
                    self.eat_ex_kind(_end, TokenKind::Punctuation(Punctuation::BitwiseOr))?;
                }
                self.eat_ex_kind(__end, TokenKind::Punctuation(Punctuation::Equals))?;
                self.eat_ex_kind(__end, TokenKind::Punctuation(Punctuation::RightArrow))?;
                let output = self.parse_match_output(__end)?;
                cases.push((inputs, output));
            }
            if self.peek(0, _end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            }
            self.eat_ex_kind(_end, TokenKind::Punctuation(Punctuation::Comma))?;
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightBracket))?;
        Ok(Statement::Match(
            Box::new(input),
            cases,
            default_case,
            is_standalone,
        ))
    }
    fn parse_match_output(&mut self, end: usize) -> Result<MatchOutput> {
        if let Ok(block) = self.parse_block(end) {
            Ok(MatchOutput::Block(block))
        } else {
            Ok(MatchOutput::Statement(Box::new(self.parse_statement(end)?)))
        }
    }

    fn parse_block(&mut self, end: usize) -> Result<Block> {
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::LeftBracket))?;
        let _end = self.isolate_block(end)?;
        let mut statements = vec![];
        while _end - self.pos > 0 {
            statements.push(self.parse_standalone_statement(_end)?);
        }
        self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::RightBracket))?;
        Ok(Block(statements))
    }

    fn to_first(&mut self, _end: usize, _tok: TokenKind) -> Result<usize> {
        let mut end = self.pos;
        let mut o = 0;
        loop {
            if end >= _end {
                break;
            }
            let tok = self.peek(o, _end)?;
            if tok.token == _tok {
                break;
            }
            o += 1;
            end += 1;
        }
        Ok(end)
    }
    fn to_first_minding_blocks(&mut self, _end: usize, _tok: TokenKind) -> Result<usize> {
        let mut end = self.pos;
        let mut o = 0;
        let mut i = 0;
        loop {
            if end >= _end {
                break;
            }
            let tok = self.peek(o, _end)?;
            if TokenKindDesc::Punctuation == tok.token {
                match tok.token {
                    TokenKind::Punctuation(punct) => match punct {
                        Punctuation::LeftBracket => i += 1,
                        Punctuation::RightBracket => i -= 1,
                        Punctuation::LeftSBracket => i += 1,
                        Punctuation::RightSBracket => i -= 1,
                        Punctuation::LeftParen => i += 1,
                        Punctuation::RightParen => i -= 1,
                        _ => {}
                    },
                    _ => {}
                }
            }
            if tok.token == _tok && i == 0 {
                break;
            }
            o += 1;
            end += 1;
        }
        Ok(end)
    }
    fn to_last(&mut self, _end: usize, _tok: TokenKind) -> Result<usize> {
        let mut end = self.pos;
        let mut o = 0;
        loop {
            if end >= _end || self.pos + o >= _end {
                break;
            }
            let tok = self.peek(o as isize, _end)?;
            if tok.token == _tok {
                end = self.pos + o;
            }
            o += 1;
        }
        Ok(end)
    }
    fn to_last_minding_blocks(&mut self, _end: usize, _tok: TokenKind) -> Result<usize> {
        let mut end = self.pos;
        let mut o = 0;
        let mut i = 0;
        loop {
            if end >= _end || self.pos + o >= _end {
                break;
            }
            let tok = self.peek(o as isize, _end)?;
            if tok.token == _tok && i == 0 {
                end = self.pos + o;
            }
            if TokenKindDesc::Punctuation == tok.token {
                match tok.token {
                    TokenKind::Punctuation(punct) => match punct {
                        Punctuation::LeftBracket => i += 1,
                        Punctuation::RightBracket => i -= 1,
                        Punctuation::LeftSBracket => i += 1,
                        Punctuation::RightSBracket => i -= 1,
                        Punctuation::LeftParen => i += 1,
                        Punctuation::RightParen => i -= 1,
                        _ => {}
                    },
                    _ => {}
                }
            }

            o += 1;
        }
        Ok(end)
    }
    fn isolate_block(&mut self, _end: usize) -> Result<usize> {
        let mut i = 1;
        let mut end = self.pos - 1;
        let mut o = 0;
        loop {
            if i == 0 {
                break;
            }
            let tok = self.peek(o, _end)?;
            if TokenKindDesc::Punctuation == tok.token {
                match tok.token {
                    TokenKind::Punctuation(punct) => match punct {
                        Punctuation::LeftBracket => i += 1,
                        Punctuation::RightBracket => i -= 1,
                        Punctuation::LeftSBracket => i += 1,
                        Punctuation::RightSBracket => i -= 1,
                        Punctuation::LeftParen => i += 1,
                        Punctuation::RightParen => i -= 1,
                        _ => {}
                    },
                    _ => {}
                }
            }
            end += 1;
            o += 1;
        }
        Ok(end)
    }

    fn eat(&mut self, end: usize) -> Result<Token> {
        if end < self.pos || end - self.pos <= 0 {
            self.peek(0, self.token_stream.len())?;
        }
        self.pos += 1;
        Ok(self.token_stream[self.pos - 1].clone())
    }
    fn eat_ex(&mut self, end: usize, ex: TokenKindDesc) -> Result<Token> {
        let token = self.eat(end)?;
        if ex != token.token {
            self.pos -= 1;
            return Err(ParserError::UnexpectedTokenEx(token, ex));
        }
        Ok(token)
    }
    fn eat_ex_kind(&mut self, end: usize, ex_kind: TokenKind) -> Result<Token> {
        let token = self.eat(end)?;
        if ex_kind != token.token {
            self.pos -= 1;
            return Err(ParserError::UnexpectedTokenExKind(token, ex_kind));
        }
        Ok(token)
    }

    fn peek(&mut self, offset: isize, end: usize) -> Result<Token> {
        if end as isize - (self.pos as isize + offset) <= 0 {
            let tk = self.peek(offset - 1, self.token_stream.len())?;
            return Err(ParserError::UnexpectedEos(tk.line, tk.col));
        }
        Ok(self.token_stream[(self.pos as isize + offset) as usize].clone())
    }
}
#[macro_export]
macro_rules! parser {
    ($($arg:tt)*) => {
        {
            let token_stream = $crate::lexer::Lexer::new(format!($($arg)*).chars()).tokenize().expect("Should be able to tokenize");
            let end = token_stream.len();
            ($crate::parser::Parser::new(token_stream), end)
        }
    };
}
#[macro_export]
macro_rules! parse_standalone {
    ($($arg:tt)*) => {
        {
            let (mut parser, end) = $crate::parser!($($arg)*);
            parser.parse_standalone_statement(end).expect("Should parse")
        }
    };
}
#[macro_export]
macro_rules! parse {
    ($($arg:tt)*) => {
        {
            let (mut parser, end) = $crate::parser!($($arg)*);
            parser.parse_statement(end).expect("Should parse")
        }
    };
}
