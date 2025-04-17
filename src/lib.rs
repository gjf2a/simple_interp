#![cfg_attr(not(test), no_std)]

// With weird git errors that mess with rust-analyzer, try this:
//
// export CARGO_NET_GIT_FETCH_WITH_CLI=true
// cargo clean
// cargo update
// cargo build

use core::cmp::min;
use core::default::Default;
use core::ops::{Add, Div, Mul, Sub};
use core::option::Option;
use core::option::Option::{None, Some};
use core::result::Result;
use core::fmt::{self, Write};
use core::str::Utf8Error;

use bare_metal_map::BareMetalMap;
use bare_metal_queue::BareMetalStack;
use gc_headers::{GarbageCollectingHeap, HeapError, Pointer, Tracer};

use thiserror_no_std::Error;

pub trait InterpreterOutput {
    fn print(&mut self, chars: &[u8]);
}

#[derive(Copy, Clone, Debug)]
pub struct Interpreter<
    const MAX_TOKENS: usize,
    const MAX_LITERAL_CHARS: usize,
    const STACK_DEPTH: usize,
    const MAX_LOCAL_VARS: usize,
    const OUTPUT_WIDTH: usize,
    G: GarbageCollectingHeap + Copy,
> {
    tokens: Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS>,
    token: usize,
    heap: G,
    variables: VarTracer<MAX_LOCAL_VARS, MAX_LITERAL_CHARS, STACK_DEPTH>,
    pending_assignment: Option<Variable<MAX_LITERAL_CHARS>>,
    brace_stacker: BraceStacker<STACK_DEPTH>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TickStatus {
    Continuing,
    Finished,
    AwaitInput,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Error)]
pub enum TickError {
    #[error("Heap error: {0}")]
    HeapIssue(HeapError),
    #[error("Awaiting input, but advanced by one tick anyway")]
    TickWhileAwaitingInput,
    #[error("Cannot negate")]
    NotNegateable,
    #[error("Unmatched parentheses")]
    UnmatchedParen,
    #[error("Syntax error")]
    SyntaxError,
    #[error("Unprocessable token: ")]
    UnprocessableToken,
    #[error("Unassigned variable")]
    UnassignedVariable,
    #[error("Nested input() statement")]
    NestedInput,
    #[error("Expected binary operator")]
    MissingBinaryOperator,
    #[error("Illegal binary operator")]
    IllegalBinaryOperator,
    #[error("Boolean value needed")]
    NeedsBoolean,
    #[error("Missing opening brace")]
    MissingOpeningBrace,
    #[error("Missing opening parentheses")]
    MissingOpeningParen,
    #[error("Unprocessable symbol")]
    UnprocessableSymbol,
    #[error("Unimplemented operation")]
    UnimplementedOpeartion,
}

impl<
        const MAX_TOKENS: usize,
        const MAX_LITERAL_CHARS: usize,
        const STACK_DEPTH: usize,
        const MAX_LOCAL_VARS: usize,
        const OUTPUT_WIDTH: usize,
        G: GarbageCollectingHeap + Copy,
    > Interpreter<MAX_TOKENS, MAX_LITERAL_CHARS, STACK_DEPTH, MAX_LOCAL_VARS, OUTPUT_WIDTH, G>
{
    pub fn new(program: &str) -> Self {
        let tokens = Tokenized::tokenize(program).unwrap();
        Self {
            tokens,
            token: 0,
            heap: G::new(),
            variables: VarTracer::new(),
            pending_assignment: None,
            brace_stacker: BraceStacker::new(),
        }
    }

    fn advance_token(&mut self) {
        self.token += 1;
    }

    pub fn current_token(&self) -> Token<MAX_LITERAL_CHARS> {
        self.tokens.tokens[self.token]
    }

    fn token_panic(&self, label: &str) {
        panic!(
            "{label} {}/{} {:?}",
            self.token,
            self.tokens.num_tokens,
            &self.tokens.tokens[0..self.tokens.num_tokens]
        );
    }

    pub fn completed(&self) -> bool {
        self.token >= self.tokens.num_tokens
    }

    pub fn blocked_on_input(&self) -> bool {
        self.pending_assignment.is_some()
    }

    pub fn provide_input(&mut self, input: &[char]) {
        if !self.blocked_on_input() {
            panic!("Called provide_input() when no input was expected.");
        }
        let var = self.pending_assignment.unwrap();
        if is_number(input) {
            self.malloc_number(input)
        } else {
            self.malloc_string(input)
        }
        .unwrap();
        let value = self.variables.pop_value();
        self.variables.assign(var, value);
        self.pending_assignment = None;
    }

    pub fn tick<I: InterpreterOutput>(&mut self, io: &mut I) -> TickStatus {
        if self.blocked_on_input() {
            TickStatus::AwaitInput
        } else if self.token == self.tokens.num_tokens {
            TickStatus::Finished
        } else {
            match self.parse_next_cmd(io) {
                Ok(status) => status,
                Err(e) => {
                    let mut error_buffer = ArrayString::<100>::default();
                    write!(&mut error_buffer, "{e:?}").unwrap();
                    io.print(error_buffer.buffer_slice());
                    TickStatus::Finished
                }
            }
        }
        
    }

    fn parse_next_cmd<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.current_token() {
            Token::Print => {
                self.advance_token();
                self.parse_print(io)
            }
            Token::Symbol(s) => {
                self.advance_token();
                self.parse_symbol(io, s)
            }
            Token::While => {
                let while_start = self.token;
                self.advance_token();
                self.parse_while(io, while_start)
            }
            Token::If => {
                self.advance_token();
                self.parse_if(io)
            }
            Token::Else => {
                self.advance_token();
                self.skip_block()
            }
            Token::CloseCurly => self.parse_block_end(),
            _ => {
                Err(TickError::UnprocessableToken)
            }
        }
    }

    fn parse_print<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.current_token() {
            Token::OpenParen => {
                self.advance_token();
                match self.parse_expr(io)? {
                    TickStatus::Continuing => {
                        let value = self.variables.pop_value();
                        self.print_value(&value, io)?;
                    }
                    TickStatus::AwaitInput => {
                        panic!("Input to print not implemented");
                    }
                    TickStatus::Finished => panic!("Program ended too soon."),
                }
                match self.current_token() {
                    Token::CloseParen => {
                        self.advance_token();
                        Ok(TickStatus::Continuing)
                    }
                    _ => return Err(TickError::UnmatchedParen),
                }
            }
            _ => return Err(TickError::MissingOpeningParen),
        }
    }

    fn parse_symbol<I: InterpreterOutput>(
        &mut self,
        io: &mut I,
        s: [char; MAX_LITERAL_CHARS],
    ) -> Result<TickStatus, TickError> {
        match self.current_token() {
            Token::Assign => {
                self.advance_token();
                match self.parse_expr(io)? {
                    TickStatus::Continuing => {
                        let value = self.variables.pop_value();
                        self.variables.assign(Variable(s), value);
                        Ok(TickStatus::Continuing)
                    }
                    TickStatus::AwaitInput => {
                        self.pending_assignment = Some(Variable(s));
                        return Ok(TickStatus::AwaitInput);
                    }
                    TickStatus::Finished => panic!("Program ended too soon."),
                }
            }
            Token::OpenParen => {
                self.token_panic("Func call");
                todo!("Function call");
            }
            _ => return Err(TickError::UnprocessableSymbol),
        }
    }

    fn parse_while<I: InterpreterOutput>(
        &mut self,
        io: &mut I,
        while_start: usize,
    ) -> Result<TickStatus, TickError> {
        match self.parse_expr(io)? {
            TickStatus::Finished => panic!("Program ended too soon."),
            TickStatus::AwaitInput => return Err(TickError::NestedInput),
            TickStatus::Continuing => {
                let value = self.variables.pop_value();
                match value.t {
                    ValueType::Boolean => {
                        if self.load_boolean(value.location) {
                            self.enter_while(while_start)
                        } else {
                            self.skip_block()
                        }
                    }
                    _ => Err(TickError::NeedsBoolean),
                }
            }
        }
    }

    fn enter_while(&mut self, while_start: usize) -> Result<TickStatus, TickError> {
        match self.current_token() {
            Token::OpenCurly => {
                self.advance_token();
                self.brace_stacker.while_loop(while_start);
                Ok(TickStatus::Continuing)
            }
            _ => Err(TickError::MissingOpeningBrace),
        }
    }

    fn parse_if<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.parse_expr(io)? {
            TickStatus::Finished => panic!("Program ended too soon."),
            TickStatus::AwaitInput => return Err(TickError::NestedInput),
            TickStatus::Continuing => {
                let value = self.variables.pop_value();
                match value.t {
                    ValueType::Boolean => {
                        if self.load_boolean(value.location) {
                            self.parse_block_start()
                        } else {
                            self.skip_block()?;
                            if let Token::Else = self.current_token() {
                                self.advance_token();
                                self.parse_block_start()
                            } else {
                                Ok(TickStatus::Continuing)
                            }
                        }
                    }
                    _ => Err(TickError::NeedsBoolean),
                }
            }
        }
    }

    fn skip_block(&mut self) -> Result<TickStatus, TickError> {
        let goal_depth = self.brace_stacker.depth();
        match self.current_token() {
            Token::OpenCurly => {
                self.advance_token();
                self.brace_stacker.opening_brace();
                while self.brace_stacker.depth() > goal_depth {
                    match self.current_token() {
                        Token::OpenCurly => {
                            self.brace_stacker.opening_brace();
                        }
                        Token::CloseCurly => {
                            self.brace_stacker.closing_brace();
                        }
                        _ => {}
                    }
                    self.advance_token();
                }
                Ok(TickStatus::Continuing)
            }
            _ => Err(TickError::MissingOpeningBrace),
        }
    }

    fn parse_block_start(&mut self) -> Result<TickStatus, TickError> {
        if let Token::OpenCurly = self.current_token() {
            self.advance_token();
            self.brace_stacker.opening_brace();
            Ok(TickStatus::Continuing)
        } else {
            Err(TickError::MissingOpeningBrace)
        }
    }

    fn parse_block_end(&mut self) -> Result<TickStatus, TickError> {
        match self.brace_stacker.closing_brace() {
            Some(while_token) => {
                self.token = while_token;
            }
            None => {
                self.advance_token();
            }
        }
        Ok(TickStatus::Continuing)
    }

    fn parse_expr<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.current_token() {
            Token::True => {
                self.advance_token();
                self.malloc_boolean(true).map(|_| TickStatus::Continuing)
            }
            Token::False => {
                self.advance_token();
                self.malloc_boolean(false).map(|_| TickStatus::Continuing)
            }
            Token::Number(n) => {
                self.advance_token();
                self.malloc_number(&n).map(|_| TickStatus::Continuing)
            }
            Token::String(s) => {
                self.advance_token();
                self.malloc_string(&s).map(|_| TickStatus::Continuing)
            }
            Token::Symbol(s) => {
                self.advance_token();
                match self.variables.look_up(Variable(s)) {
                    Some(value) => {
                        self.variables.push_value(value);
                        Ok(TickStatus::Continuing)
                    }
                    None => Err(TickError::UnassignedVariable),
                }
            }
            Token::Input => {
                self.advance_token();
                self.parse_input(io)
            }
            Token::OpenParen => {
                self.advance_token();
                self.parse_operation(io)
            }
            Token::Not => {
                self.advance_token();
                self.parse_not(io)
            }
            Token::Minus => {
                self.advance_token();
                self.parse_negate(io)
            }
            _ => Err(TickError::UnprocessableToken),
        }
    }

    fn parse_operation<I: InterpreterOutput>(
        &mut self,
        io: &mut I,
    ) -> Result<TickStatus, TickError> {
        self.parse_expr(io)?;
        let op = self.current_token();
        match op {
            Token::And
            | Token::Or
            | Token::Plus
            | Token::Minus
            | Token::Times
            | Token::Divide
            | Token::Equal
            | Token::LessThan
            | Token::GreaterThan => {
                self.advance_token();
                self.parse_expr(io)?;
                let value2 = self.variables.pop_value();
                let value1 = self.variables.pop_value();
                match self.current_token() {
                    Token::CloseParen => {
                        self.advance_token();
                        self.do_arithmetic(value1, value2, op)
                            .map(|_| TickStatus::Continuing)
                    }
                    _ => Err(TickError::UnmatchedParen),
                }
            }
            _ => Err(TickError::MissingBinaryOperator),
        }
    }

    fn do_arithmetic(
        &mut self,
        value1: Value,
        value2: Value,
        op: Token<MAX_LITERAL_CHARS>,
    ) -> Result<(), TickError> {
        match value1.t {
            ValueType::Integer => {
                let v1 = self.load_int(value1.location);
                match value2.t {
                    ValueType::Integer => {
                        let v2 = self.load_int(value2.location);
                        self.perform_binary_op(v1, v2, op, ValueType::Integer, make_unsigned_from)
                    }
                    ValueType::Float => {
                        let v1 = v1 as f64;
                        let v2 = self.load_float(value2.location);
                        self.perform_binary_op(v1, v2, op, ValueType::Float, f64::to_bits)
                    }
                    ValueType::String => self.string_not_string(op),
                    ValueType::Boolean => todo!(),
                }
            }
            ValueType::Float => {
                let v1 = self.load_float(value1.location);
                match value2.t {
                    ValueType::Float => {
                        let v2 = self.load_float(value2.location);
                        self.perform_binary_op(v1, v2, op, ValueType::Float, f64::to_bits)
                    }
                    ValueType::Integer => {
                        let v2 = self.load_int(value2.location) as f64;
                        self.perform_binary_op(v1, v2, op, ValueType::Float, f64::to_bits)
                    }
                    ValueType::String => self.string_not_string(op),
                    ValueType::Boolean => todo!(),
                }
            }
            ValueType::String => match value2.t {
                ValueType::String => {
                    self.malloc_boolean(if value1.location.len() == value2.location.len() {
                        let mut p1 = Some(value1.location.clone());
                        let mut p2 = Some(value2.location.clone());
                        let mut strings_match = true;
                        while strings_match && p1.is_some() {
                            let v1 = self.heap.load(p1.unwrap()).unwrap();
                            let v2 = self.heap.load(p2.unwrap()).unwrap();
                            if v1 == v2 {
                                p1 = p1.unwrap().next();
                                p2 = p2.unwrap().next();
                            } else {
                                strings_match = false;
                            }
                        }
                        strings_match
                    } else {
                        false
                    })
                }
                _ => self.malloc_boolean(false),
            },
            ValueType::Boolean => todo!(),
        }
    }

    fn string_not_string(&mut self, op: Token<MAX_LITERAL_CHARS>) -> Result<(), TickError> {
        if let Token::Equal = op {
            self.malloc_boolean(false)
        } else {
            Err(TickError::IllegalBinaryOperator)
        }
    }

    fn perform_binary_op<
        N: Copy
            + Add<Output = N>
            + Sub<Output = N>
            + Mul<Output = N>
            + Div<Output = N>
            + PartialOrd
            + PartialEq,
        F: Fn(N) -> u64,
    >(
        &mut self,
        v1: N,
        v2: N,
        op: Token<MAX_LITERAL_CHARS>,
        vt: ValueType,
        encoder_u64: F,
    ) -> Result<(), TickError> {
        match op {
            Token::Plus => self.malloc_numeric_value(encoder_u64(v1 + v2), vt),
            Token::Minus => self.malloc_numeric_value(encoder_u64(v1 - v2), vt),
            Token::Times => self.malloc_numeric_value(encoder_u64(v1 * v2), vt),
            Token::Divide => self.malloc_numeric_value(encoder_u64(v1 / v2), vt),
            Token::LessThan => self.malloc_boolean(v1 < v2),
            Token::GreaterThan => self.malloc_boolean(v1 > v2),
            Token::Equal => self.malloc_boolean(v1 == v2),
            _ => Err(TickError::IllegalBinaryOperator),
        }
    }

    fn parse_not<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.parse_expr(io)? {
            TickStatus::Continuing => {
                let arg = self.variables.pop_value();
                match arg.t {
                    ValueType::Boolean => self
                        .malloc_boolean(!self.load_boolean(arg.location))
                        .map(|_| TickStatus::Continuing),
                    _ => Err(TickError::NeedsBoolean),
                }
            }
            TickStatus::Finished => panic!("Program ended too soon."),
            TickStatus::AwaitInput => Err(TickError::NestedInput),
        }
    }

    fn parse_negate<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.parse_expr(io)? {
            TickStatus::Continuing => {
                let arg = self.variables.pop_value();
                match arg.t {
                    ValueType::Integer => self
                        .malloc_numeric_value(
                            make_unsigned_from(-self.load_int(arg.location)),
                            ValueType::Integer,
                        )
                        .map(|_| TickStatus::Continuing),
                    ValueType::Float => self
                        .malloc_numeric_value(
                            (-self.load_float(arg.location)).to_bits(),
                            ValueType::Float,
                        )
                        .map(|_| TickStatus::Continuing),
                    _ => Err(TickError::NotNegateable),
                }
            }
            TickStatus::Finished => panic!("Program ended too soon."),
            TickStatus::AwaitInput => Err(TickError::NestedInput),
        }
    }

    fn load_int(&self, p: Pointer) -> i64 {
        make_signed_from(self.heap.load(p).unwrap())
    }

    fn load_boolean(&self, p: Pointer) -> bool {
        self.heap.load(p).unwrap() != 0
    }

    fn load_float(&self, p: Pointer) -> f64 {
        f64::from_bits(self.heap.load(p).unwrap())
    }

    fn parse_input<I: InterpreterOutput>(&mut self, io: &mut I) -> Result<TickStatus, TickError> {
        match self.current_token() {
            Token::OpenParen => {
                self.advance_token();
                self.parse_expr(io)?;
                match self.parse_expr(io)? {
                    TickStatus::Continuing => {
                        let value = self.variables.pop_value();
                        self.print_value(&value, io)?;
                    }
                    TickStatus::Finished => panic!("Program ended too soon."),
                    TickStatus::AwaitInput => {
                        return Err(TickError::NestedInput);
                    }
                }
                match self.current_token() {
                    Token::CloseParen => {
                        self.advance_token();
                        return Ok(TickStatus::AwaitInput);
                    }
                    _ => return Err(TickError::UnmatchedParen),
                }
            }
            _ => return Err(TickError::MissingOpeningParen),
        }
    }

    fn malloc_boolean(&mut self, value: bool) -> Result<(), TickError> {
        let value = if value { u64::MAX } else { 0 };
        self.malloc_numeric_value(value, ValueType::Boolean)
    }

    fn malloc_number(&mut self, n: &[char]) -> Result<(), TickError> {
        let (t, value) = if n.contains(&'.') {
            let float_val = parse_float_from(n);
            (ValueType::Float, float_val.to_bits())
        } else {
            let int_val = parse_int_from(n);
            (ValueType::Integer, int_val)
        };
        self.malloc_numeric_value(value, t)
    }

    fn malloc_numeric_value(&mut self, value: u64, t: ValueType) -> Result<(), TickError> {
        match self.heap.malloc(1, &self.variables) {
            Ok(p) => match self.heap.store(p, value) {
                Ok(_) => {
                    self.variables.push_value(Value { location: p, t });
                    Ok(())
                }
                Err(e) => Err(TickError::HeapIssue(e)),
            },
            Err(e) => Err(TickError::HeapIssue(e)),
        }
    }

    fn malloc_string(&mut self, s: &[char]) -> Result<(), TickError> {
        let num_chars = s.iter().take_while(|c| **c != '\0').count();
        match self.heap.malloc(num_chars, &self.variables) {
            Ok(location) => {
                let mut p = Some(location);
                for i in 0..num_chars {
                    let pt = p.unwrap();
                    match self.heap.store(pt, s[i] as u64) {
                        Ok(_) => {
                            p = pt.next();
                        }
                        Err(e) => return Err(TickError::HeapIssue(e)),
                    }
                }

                self.variables.push_value(Value {
                    location,
                    t: ValueType::String,
                });
                Ok(())
            }
            Err(e) => Err(TickError::HeapIssue(e)),
        }
    }

    fn print_value<I: InterpreterOutput>(&self, v: &Value, io: &mut I) -> Result<(), TickError> {
        assert!(OUTPUT_WIDTH > 3);
        let mut output_buffer = [0; OUTPUT_WIDTH];
        let num_words = v.output::<G>(&self.heap, &mut output_buffer)?;
        io.print(&output_buffer[0..num_words]);
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
struct BraceStacker<const MAX_DEPTH: usize> {
    brace_depth: usize,
    loop_back_tokens: [Option<usize>; MAX_DEPTH],
}

impl<const MAX_DEPTH: usize> BraceStacker<MAX_DEPTH> {
    fn new() -> Self {
        Self {
            brace_depth: 0,
            loop_back_tokens: [None; MAX_DEPTH],
        }
    }

    fn while_loop(&mut self, condition_token: usize) {
        self.loop_back_tokens[self.brace_depth] = Some(condition_token);
        self.brace_depth += 1;
    }

    fn opening_brace(&mut self) {
        self.brace_depth += 1;
    }

    fn closing_brace(&mut self) -> Option<usize> {
        self.brace_depth -= 1;
        self.loop_back_tokens[self.brace_depth]
    }

    fn depth(&self) -> usize {
        self.brace_depth
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Value {
    location: Pointer,
    t: ValueType,
}

impl Default for Value {
    fn default() -> Self {
        Self {
            location: Default::default(),
            t: ValueType::Integer,
        }
    }
}

impl Value {
    pub fn block_num(&self) -> usize {
        self.location.block_num()
    }

    fn output<G: GarbageCollectingHeap + Copy>(
        &self,
        heap: &G,
        buffer: &mut [u8],
    ) -> Result<usize, TickError> {
        match self.t {
            ValueType::Integer => match heap.load(self.location) {
                Ok(w) => i64_into_buffer(make_signed_from(w), buffer),
                Err(e) => Err(TickError::HeapIssue(e)),
            },
            ValueType::String => {
                let mut p = Some(self.location);
                let end = min(self.location.len(), buffer.len() - 1);
                for i in 0..end {
                    let pt = p.unwrap();
                    match heap.load(pt) {
                        Ok(value) => {
                            buffer[i] = value as u8;
                            p = pt.next();
                        }
                        Err(e) => return Err(TickError::HeapIssue(e)),
                    }
                }
                buffer[end] = '\n' as u8;
                Ok(end + 1)
            }
            ValueType::Float => match heap.load(self.location) {
                Ok(w) => f64_into_buffer(f64::from_bits(w), buffer),
                Err(e) => Err(TickError::HeapIssue(e)),
            },
            ValueType::Boolean => match heap.load(self.location) {
                Err(e) => Err(TickError::HeapIssue(e)),
                Ok(b) => {
                    let bytes = if b == 0 {
                        "false\n".as_bytes()
                    } else {
                        "true\n".as_bytes()
                    };
                    for (i, c) in bytes.iter().enumerate() {
                        buffer[i] = *c;
                    }
                    Ok(buffer.len() + 1)
                }
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ValueType {
    Integer,
    Float,
    String,
    Boolean,
}

fn parse_int_from(chars: &[char]) -> u64 {
    let mut value = 0;
    for c in chars.iter().take_while(|c| **c != '\0') {
        value *= 10;
        value += *c as u64 - '0' as u64;
    }
    value
}

fn make_signed_from(value: u64) -> i64 {
    if value >> 63 == 1 {
        -((!value + 1) as i64)
    } else {
        value as i64
    }
}

fn make_unsigned_from(value: i64) -> u64 {
    if value < 0 {
        ((value & i64::MAX) as u64) | (1 << 63)
    } else {
        value as u64
    }
}

fn parse_float_from(chars: &[char]) -> f64 {
    let mut value = 0.0;
    let mut shifter = None;
    for c in chars.iter().take_while(|c| **c != '\0') {
        if *c == '.' {
            shifter = Some(0.1);
        } else {
            match shifter.as_mut() {
                Some(shift) => {
                    value += ((*c as u64 - '0' as u64) as f64) * *shift;
                    *shift /= 10.0;
                }
                None => {
                    value *= 10.0;
                    value += (*c as u64 - '0' as u64) as f64;
                }
            }
        }
    }
    value
}

pub fn i64_into_buffer(mut value: i64, buffer: &mut [u8]) -> Result<usize, TickError> {
    if value == 0 {
        buffer[0] = '0' as u8;
        buffer[1] = '\n' as u8;
        return Ok(2);
    }
    let mut start = if value < 0 {
        buffer[0] = '-' as u8;
        value = -value;
        1
    } else {
        0
    };
    let mut i = start;
    while value > 0 && i < buffer.len() - 1 {
        buffer[i] = (value % 10) as u8 + '0' as u8;
        value /= 10;
        i += 1;
    }
    while start < i / 2 {
        let temp = buffer[start];
        buffer[start] = buffer[i - start - 1];
        buffer[i - start - 1] = temp;
        start += 1;
    }
    buffer[i] = '\n' as u8;

    Ok(i + 1)
}

pub fn f64_into_buffer(mut value: f64, buffer: &mut [u8]) -> Result<usize, TickError> {
    let start = if value < 0.0 {
        buffer[0] = '-' as u8;
        value = -value;
        1
    } else {
        0
    };
    let mut bytes = start + i64_into_buffer(value as i64, &mut buffer[start..]).unwrap();
    buffer[bytes - 1] = '.' as u8;
    value -= (value as i64) as f64;
    let mut shifts = 0;
    let mut found_non_zero = false;
    let mut num_leading_zeros = 0;
    while (value as i64) as f64 != value && shifts + bytes + 1 < buffer.len() {
        value *= 10.0;
        if !found_non_zero {
            if value as i64 == 0 {
                num_leading_zeros += 1;
            } else {
                found_non_zero = true;
            }
        }
        shifts += 1;
    }
    for i in 0..num_leading_zeros {
        buffer[bytes + i] = '0' as u8;
    }
    bytes += num_leading_zeros;
    if bytes + 2 < buffer.len() {
        bytes += i64_into_buffer(value as i64, &mut buffer[bytes..]).unwrap();
    } else {
        buffer[buffer.len() - 1] = '\n' as u8;
        bytes = buffer.len();
    }
    Ok(bytes)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Variable<const MAX_LITERAL_CHARS: usize>([char; MAX_LITERAL_CHARS]);

impl<const MAX_LITERAL_CHARS: usize> Default for Variable<MAX_LITERAL_CHARS> {
    fn default() -> Self {
        let chars = ['\0'; MAX_LITERAL_CHARS];
        Self(chars)
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub struct VarTracer<
    const MAX_LOCAL_VARS: usize,
    const MAX_LITERAL_CHARS: usize,
    const STACK_DEPTH: usize,
> {
    vars: BareMetalMap<Variable<MAX_LITERAL_CHARS>, Value, MAX_LOCAL_VARS>,
    expr_stack: BareMetalStack<Value, STACK_DEPTH>,
}

impl<const MAX_LOCAL_VARS: usize, const MAX_LITERAL_CHARS: usize, const STACK_DEPTH: usize> Tracer
    for VarTracer<MAX_LOCAL_VARS, MAX_LITERAL_CHARS, STACK_DEPTH>
{
    // If I ever add structs or arrays, this will need to be modified
    // to trace out those internal pointers as well.
    fn trace(&self, blocks_used: &mut [bool]) {
        for i in 0..self.vars.len() {
            blocks_used[self.vars.get_at(i).unwrap().block_num()] = true;
        }

        for i in 0..self.expr_stack.len() {
            blocks_used[self.expr_stack[i].block_num()] = true;
        }
    }
}

impl<const MAX_LOCAL_VARS: usize, const MAX_LITERAL_CHARS: usize, const STACK_DEPTH: usize>
    VarTracer<MAX_LOCAL_VARS, MAX_LITERAL_CHARS, STACK_DEPTH>
{
    pub fn new() -> Self {
        Self {
            vars: BareMetalMap::new(),
            expr_stack: BareMetalStack::new(),
        }
    }

    fn assign(&mut self, variable: Variable<MAX_LITERAL_CHARS>, value: Value) {
        self.vars.put(variable, value);
    }

    fn look_up(&self, variable: Variable<MAX_LITERAL_CHARS>) -> Option<Value> {
        self.vars.get(variable)
    }

    fn push_value(&mut self, v: Value) {
        self.expr_stack.push(v);
    }

    fn pop_value(&mut self) -> Value {
        self.expr_stack.pop()
    }
}

#[derive(Debug)]
pub enum TokenResult<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> {
    Ok(Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS>),
    Err(TokenError),
}

impl<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize>
    TokenResult<MAX_TOKENS, MAX_LITERAL_CHARS>
{
    pub fn unwrap(self) -> Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS> {
        match self {
            TokenResult::Ok(t) => t,
            TokenResult::Err(e) => panic!("Token error: {e:?}"),
        }
    }
}

#[derive(Debug)]
pub enum TokenError {
    LiteralTooLong,
    TooManyTokens,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Token<const MAX_LITERAL_CHARS: usize> {
    Symbol([char; MAX_LITERAL_CHARS]),
    Number([char; MAX_LITERAL_CHARS]),
    String([char; MAX_LITERAL_CHARS]),
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    Plus,
    Minus,
    Times,
    Divide,
    Comma,
    Assign,
    Equal,
    True,
    False,
    While,
    If,
    Else,
    And,
    Or,
    Not,
    LessThan,
    GreaterThan,
    Print,
    Input,
}

impl<const MAX_LITERAL_CHARS: usize> Default for Token<MAX_LITERAL_CHARS> {
    fn default() -> Self {
        Token::False
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Tokenized<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> {
    tokens: [Token<MAX_LITERAL_CHARS>; MAX_TOKENS],
    num_tokens: usize,
}

macro_rules! try_token {
    ($result:ident, $t:expr, $nc:ident, $sb:ident) => {
        match $result.push_token($t) {
            None => {
                $nc = 0;
                Self::clear(&mut $sb);
            }
            Some(e) => return TokenResult::Err(e),
        }
    };
}

macro_rules! try_add_char {
    ($c:expr, $nc:ident, $sb:ident) => {
        if $nc < $sb.len() {
            $sb[$nc] = $c;
            $nc += 1;
        } else {
            return TokenResult::Err(TokenError::LiteralTooLong);
        }
    };
}

macro_rules! try_terminate {
    ($result:ident, $nc:ident, $sb:ident) => {
        if $nc > 0 {
            let slice = &$sb[0..$nc];
            match Self::match_multiple(slice) {
                Some(t) => {
                    try_token!($result, t, $nc, $sb);
                }
                None => {
                    if is_number(slice) {
                        try_token!($result, Token::Number($sb), $nc, $sb);
                    } else {
                        try_token!($result, Token::Symbol($sb), $nc, $sb);
                    }
                }
            }
        }
    };
}

impl<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize>
    Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS>
{
    pub fn tokenize(text: &str) -> TokenResult<MAX_TOKENS, MAX_LITERAL_CHARS> {
        let mut result = Self {
            num_tokens: 0,
            tokens: [Token::False; MAX_TOKENS],
        };
        let mut str_buffer = ['\0'; MAX_LITERAL_CHARS];
        let mut num_chars = 0;
        let mut inside_string = false;
        for c in text.chars() {
            if inside_string && c == '"' {
                inside_string = false;
                try_token!(result, Token::String(str_buffer), num_chars, str_buffer);
            } else if c == '"' {
                inside_string = true;
            } else if inside_string {
                try_add_char!(c, num_chars, str_buffer);
            } else if let Some(token) = Self::match_single(c) {
                try_terminate!(result, num_chars, str_buffer);
                result.push_token(token);
            } else if c.is_whitespace() {
                try_terminate!(result, num_chars, str_buffer);
            } else {
                try_add_char!(c, num_chars, str_buffer);
            }
        }
        TokenResult::Ok(result)
    }

    fn push_token(&mut self, t: Token<MAX_LITERAL_CHARS>) -> Option<TokenError> {
        if self.num_tokens < self.tokens.len() {
            self.tokens[self.num_tokens] = t;
            self.num_tokens += 1;
            None
        } else {
            Some(TokenError::TooManyTokens)
        }
    }

    fn clear(str_buffer: &mut [char]) {
        for i in 0..str_buffer.len() {
            str_buffer[i] = '\0';
        }
    }

    fn match_single(c: char) -> Option<Token<MAX_LITERAL_CHARS>> {
        match c {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Times),
            '/' => Some(Token::Divide),
            ',' => Some(Token::Comma),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenCurly),
            '}' => Some(Token::CloseCurly),
            '[' => Some(Token::OpenSquare),
            ']' => Some(Token::CloseSquare),
            '<' => Some(Token::LessThan),
            '>' => Some(Token::GreaterThan),
            _ => None,
        }
    }

    fn match_multiple(chars: &[char]) -> Option<Token<MAX_LITERAL_CHARS>> {
        match chars {
            [':', '='] => Some(Token::Assign),
            ['=', '='] => Some(Token::Equal),
            ['t', 'r', 'u', 'e'] => Some(Token::True),
            ['f', 'a', 'l', 's', 'e'] => Some(Token::False),
            ['w', 'h', 'i', 'l', 'e'] => Some(Token::While),
            ['i', 'f'] => Some(Token::If),
            ['e', 'l', 's', 'e'] => Some(Token::Else),
            ['a', 'n', 'd'] => Some(Token::And),
            ['o', 'r'] => Some(Token::Or),
            ['n', 'o', 't'] => Some(Token::Not),
            ['p', 'r', 'i', 'n', 't'] => Some(Token::Print),
            ['i', 'n', 'p', 'u', 't'] => Some(Token::Input),
            _ => None,
        }
    }
}

fn is_number(chars: &[char]) -> bool {
    chars.len() > 0
        && chars[0].is_numeric()
        && chars.iter().filter(|c| **c == '.').count() <= 1
        && chars.iter().all(|c| c.is_numeric() || *c == '.')
}

// From https://www.perplexity.ai/search/in-no-std-rust-how-can-you-con-BXi9dcqaT16t_uq.nPJAZA
#[derive(Copy, Clone)]
pub struct ArrayString<const BUFFER_SIZE: usize> {
    buf: [u8; BUFFER_SIZE],
    len: usize,
}

impl<const BUFFER_SIZE: usize> ArrayString<BUFFER_SIZE> {
    pub fn buffer_slice(&self) -> &[u8] {
        &self.buf[..self.len]
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_str(&self) -> Result::<&str, Utf8Error> {
        core::str::from_utf8(self.buffer_slice())
    }

    pub fn push_char(&mut self, c: char) {
        if c == '\u{8}' && self.len > 0 {
            self.len -= 1;
        } else if self.len < self.buf.len() {
            self.buf[self.len] = c as u8;
            self.len += 1;
        }
    }
}

impl<const BUFFER_SIZE: usize> Default for ArrayString<BUFFER_SIZE> {
    fn default() -> Self {
        Self { buf: [0; BUFFER_SIZE], len: 0 }
    }
}

impl<const BUFFER_SIZE: usize> Write for ArrayString<BUFFER_SIZE> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let bytes = s.as_bytes();
        let available = self.buf.len() - self.len;
        let to_copy = bytes.len().min(available);
        self.buf[self.len..self.len + to_copy].copy_from_slice(&bytes[..to_copy]);
        self.len += to_copy;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Display;
    use std::fs::read_to_string;

    use super::*;

    impl<const MAX_LITERAL_CHARS: usize> Token<MAX_LITERAL_CHARS> {
        fn var(&self) -> Option<String> {
            match self {
                Token::Symbol(chars) | Token::Number(chars) | Token::String(chars) => {
                    Some(chars.iter().take_while(|c| **c != '\0').collect())
                }
                _ => None,
            }
        }
    }

    impl<const MAX_LITERAL_CHARS: usize> Display for Token<MAX_LITERAL_CHARS> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                Token::Symbol(_) => {
                    write!(f, "Symbol ({})", self.var().unwrap())
                }
                Token::Number(_) => {
                    write!(f, "Number ({})", self.var().unwrap())
                }
                Token::String(_) => {
                    write!(f, "String \"{}\"", self.var().unwrap())
                }
                token => write!(f, "{token:?}"),
            }
        }
    }

    impl<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> Display
        for Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS>
    {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            for i in 0..self.num_tokens {
                writeln!(f, "{}", self.tokens[i])?;
            }
            Ok(())
        }
    }

    #[test]
    fn test_token_1() {
        let example = read_to_string("programs/average.prog").unwrap();
        let tokens = Tokenized::<500, 100>::tokenize(example.as_str()).unwrap();
        println!("{tokens}");
    }

    #[test]
    fn test_token_2() {
        let example = read_to_string("programs/hello.prog").unwrap();
        let tokens = Tokenized::<500, 100>::tokenize(example.as_str()).unwrap();
        println!("{tokens}");
    }

    #[test]
    fn test_parse_int() {
        for s in [['1', '0'], ['1', '2'], ['2', '1'], ['4', '3']] {
            let expected = s.iter().collect::<String>().parse::<u64>().unwrap();
            assert_eq!(expected, parse_int_from(&s));
        }
    }

    #[test]
    fn test_parse_float() {
        for s in [
            ['1', '0', '.', '0', '0'],
            ['0', '1', '.', '0', '1'],
            ['2', '5', '.', '1', '9'],
            ['2', '.', '4', '4', '8'],
        ] {
            let expected = s.iter().collect::<String>().parse::<f64>().unwrap();
            assert_eq!(expected, parse_float_from(&s));
        }
    }

    #[test]
    fn test_signed() {
        for (bits, value) in [
            (u64::MAX, -1),
            (1, 1),
            (0, 0),
            (u64::MAX - 1, -2),
            (u64::MAX - 100, -101),
        ] {
            println!("{value}");
            let signed = make_signed_from(bits);
            assert_eq!(value, signed);
            assert_eq!(bits, make_unsigned_from(signed));
        }
    }

    #[test]
    fn test_output_f64() {
        let f = 1234.56789;
        let mut buffer = [0; 12];
        let bytes = f64_into_buffer(f, &mut buffer).unwrap();
        assert_eq!(bytes, buffer.len());
        assert_eq!(
            format!("{buffer:?}"),
            "[49, 50, 51, 52, 46, 53, 54, 55, 56, 57, 48, 10]"
        );

        let f = -43.21;
        let mut buffer = [0; 7];
        let bytes = f64_into_buffer(f, &mut buffer).unwrap();
        assert_eq!(bytes, buffer.len());
        assert_eq!(format!("{buffer:?}"), "[45, 52, 51, 46, 50, 49, 10]");
    }

    #[test]
    fn test_simple_pi() {
        let f = 3.0418396189294032;
        let mut buffer = [0; 30];
        let bytes = f64_into_buffer(f, &mut buffer).unwrap();
        assert_eq!(bytes, 20);
        assert_eq!(format!("{buffer:?}"), "[51, 46, 48, 52, 49, 56, 51, 57, 54, 49, 56, 57, 50, 57, 52, 48, 51, 50, 52, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]");
    }

    #[test]
    fn test_zero_float_out() {
        let f = 0.0000001;
        let mut buffer = [0; 7];
        let bytes = f64_into_buffer(f, &mut buffer).unwrap();
        assert_eq!(bytes, buffer.len());
        assert_eq!(format!("{buffer:?}"), "[48, 46, 48, 48, 48, 48, 10]");
    }

    #[test]
    fn test_is_number() {
        assert!(!is_number(&[]));
        assert!(!is_number(&['1', '.', '0', '.', '1']));

        assert!(is_number(&['1', '0']));
        assert!(is_number(&['1', '.', '0', '1']));
    }
}
