#![cfg_attr(not(test), no_std)]

// With weird git errors that mess with rust-analyzer, try this:
//
// export CARGO_NET_GIT_FETCH_WITH_CLI=true
// cargo clean
// cargo update
// cargo build

use core::ops::{FnOnce, Add, Sub, Mul, Div};
use core::default::Default;
use core::option::Option;
use core::option::Option::{None, Some};
use core::cmp::min;

use bare_metal_map::BareMetalMap;
use gc_headers::{GarbageCollectingHeap, HeapResult, HeapError, Pointer, Tracer};

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
    stack: StackFrame<MAX_LOCAL_VARS, MAX_LITERAL_CHARS>,
    pending_assignment: Option<Variable<MAX_LITERAL_CHARS>>,
    brace_stacker: BraceStacker<STACK_DEPTH>,
}

pub enum TickResult<T> {
    Ok(T),
    Finished,
    AwaitInput,
    Err(TickError),
}

impl<T> TickResult<T> {
    pub fn unwrap(self) -> T {
        match self {
            TickResult::Ok(v) => v,
            TickResult::Finished => panic!("No result - program already over"),
            TickResult::AwaitInput => panic!("No result - awaiting input"),
            TickResult::Err(e) => panic!("Interpreter Error: {e:?}"),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> TickResult<U> {
        match self {
            TickResult::Ok(value) => TickResult::Ok(op(value)),
            TickResult::Err(e) => TickResult::Err(e),
            TickResult::AwaitInput => TickResult::AwaitInput,
            TickResult::Finished => TickResult::Finished,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TickError {
    HeapIssue(HeapError),
    NotNegateable,
    UnmatchedParen,
    SyntaxError,
    UnprocessableToken,
    UnassignedVariable,
    NestedInput,
    MissingBinaryOperator,
    IllegalBinaryOperator,
    NeedsBoolean,
    MissingOpeningBrace,
    MissingOpeningParen,
    UnprocessableSymbol,
    UnimplementedOpeartion,
}

impl<
        const MAX_TOKENS: usize,
        const MAX_LITERAL_CHARS: usize,
        const STACK_DEPTH: usize,
        const MAX_LOCAL_VARS: usize,
        const OUTPUT_WIDTH: usize,
        G: GarbageCollectingHeap + Copy,
    >
    Interpreter<
        MAX_TOKENS,
        MAX_LITERAL_CHARS,
        STACK_DEPTH,
        MAX_LOCAL_VARS,
        OUTPUT_WIDTH,
        G,
    >
{
    pub fn new(program: &str) -> Self {
        let tokens = Tokenized::tokenize(program).unwrap();
        Self {
            tokens,
            token: 0,
            heap: G::new(),
            stack: StackFrame::new(),
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
        panic!("{label} {}/{} {:?}", self.token, self.tokens.num_tokens, &self.tokens.tokens[0..self.tokens.num_tokens]);
    }

    pub fn completed(&self) -> bool {
        self.token >= self.tokens.num_tokens
    }

    pub fn blocked_on_input(&self) -> bool {
        self.pending_assignment.is_some()
    }

    pub fn provide_input(&mut self, input: &[char]) {
        let var = self.pending_assignment.unwrap();
        let value = if is_number(input) {
            self.malloc_number(input)
        } else {
            self.malloc_string(input)
        }.unwrap();
        self.stack.assign(var, value);
    }

    pub fn tick<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<()> {
        if self.token == self.tokens.num_tokens {
            return TickResult::Finished;
        }
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
                self.token_panic("unimplemented");
                todo!()
            }
        }
    }

    fn parse_print<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<()> {
        match self.current_token() {
            Token::OpenParen => {
                self.advance_token();
                match self.parse_expr(io) {
                    TickResult::Ok(v) => match self.print_value(&v, io) {
                        TickResult::Ok(_) => {}
                        TickResult::Err(e) => return TickResult::Err(e),
                        TickResult::AwaitInput => panic!("Input to print not implemented"),
                        TickResult::Finished => panic!("Program ended too soon."),
                    },
                    TickResult::AwaitInput => {
                        panic!("Input to print not implemented");
                    }
                    TickResult::Err(e) => return TickResult::Err(e),
                    TickResult::Finished => panic!("Program ended too soon."),
                }
                match self.current_token() {
                    Token::CloseParen => {
                        self.advance_token();
                        TickResult::Ok(())
                    }
                    _ => return TickResult::Err(TickError::UnmatchedParen)
                }
            }
            _ => return TickResult::Err(TickError::MissingOpeningParen),
        }
    }

    fn parse_symbol<I: InterpreterOutput>(&mut self, io: &mut I, s: [char; MAX_LITERAL_CHARS]) -> TickResult<()> {
        match self.current_token() {
            Token::Assign => {
                self.advance_token();
                match self.parse_expr(io) {
                    TickResult::Ok(value) => {
                        self.stack.assign(Variable(s), value);
                        TickResult::Ok(())
                    }
                    TickResult::Err(e) => return TickResult::Err(e),
                    TickResult::AwaitInput => {
                        self.pending_assignment = Some(Variable(s));
                        return TickResult::AwaitInput;
                    }
                    TickResult::Finished => panic!("Program ended too soon."),
                }
            }
            Token::OpenParen => {
                self.token_panic("Func call");
                todo!("Function call");
            }
            _ => return TickResult::Err(TickError::UnprocessableSymbol),
        }
    }

    fn parse_while<I: InterpreterOutput>(&mut self, io: &mut I, while_start: usize) -> TickResult<()> {
        match self.parse_expr(io) {
            TickResult::Finished => panic!("Program ended too soon."),
            TickResult::AwaitInput => return TickResult::Err(TickError::NestedInput),
            TickResult::Err(e) => return TickResult::Err(e),
            TickResult::Ok(value) => {
                match value.t {
                    ValueType::Boolean => {
                        if self.load_boolean(value.location) {
                            self.enter_while(while_start)
                        } else {
                            self.skip_block()
                        }
                    }
                    _ => TickResult::Err(TickError::NeedsBoolean)
                }
            }
        }
    }

    fn enter_while(&mut self, while_start: usize) -> TickResult<()> {
        match self.current_token() {
            Token::OpenCurly => {
                self.advance_token();
                self.brace_stacker.while_loop(while_start);
                TickResult::Ok(())
            }
            _ => TickResult::Err(TickError::MissingOpeningBrace)
        }
    }

    fn parse_if<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<()> {
        match self.parse_expr(io) {
            TickResult::Finished => panic!("Program ended too soon."),
            TickResult::AwaitInput => return TickResult::Err(TickError::NestedInput),
            TickResult::Err(e) => return TickResult::Err(e),
            TickResult::Ok(value) => {
                match value.t {
                    ValueType::Boolean => {
                        if self.load_boolean(value.location) {
                            self.parse_block_start()
                        } else {
                            self.skip_block();
                            if let Token::Else = self.current_token() {
                                self.advance_token();
                                self.parse_block_start()
                            } else {
                                TickResult::Ok(())
                            }
                        }
                    }
                    _ => {
                        TickResult::Err(TickError::NeedsBoolean)
                    }
                }
            }
        }
    }

    fn skip_block(&mut self) -> TickResult<()> {
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
                TickResult::Ok(())
            }
            _ => TickResult::Err(TickError::MissingOpeningBrace)
        }
    }

    fn parse_block_start(&mut self) -> TickResult<()> {
        if let Token::OpenCurly = self.current_token() {
            self.advance_token();
            self.brace_stacker.opening_brace();
            TickResult::Ok(())
        } else {
            TickResult::Err(TickError::MissingOpeningBrace)
        }
    }

    fn parse_block_end(&mut self) -> TickResult<()> {
        match self.brace_stacker.closing_brace() {
            Some(while_token) => {
                self.token = while_token;
            }
            None => {
                self.advance_token();
            }
        }
        TickResult::Ok(())
    }

    fn parse_expr<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<Value> {
        match self.current_token() {
            Token::True => {
                self.advance_token();
                self.malloc_boolean(true)
            }
            Token::False => {
                self.advance_token();
                self.malloc_boolean(false)
            }
            Token::Number(n) => {
                self.advance_token();
                self.malloc_number(&n)
            }
            Token::String(s) => {
                self.advance_token();
                self.malloc_string(&s)
            }
            Token::Symbol(s) => {
                self.advance_token();
                match self.stack.look_up(Variable(s)) {
                    Some(value) => {
                        let mut buffer = [0; 10];
                        value.output(&self.heap, &mut buffer).unwrap();
                        TickResult::Ok(value)
                    }
                    None => TickResult::Err(TickError::UnassignedVariable)
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
            _ => {
                TickResult::Err(TickError::UnprocessableToken)
            }
        }
    }

    fn parse_operation<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<Value> {
        match self.parse_expr(io) {
            TickResult::Finished => panic!("Program ended too soon."),
            TickResult::Ok(value1) => {
                let op = self.current_token();
                match op {
                    Token::And | Token::Or | Token::Plus | Token::Minus | Token::Times | Token::Divide | Token::Equal | Token::LessThan | Token::GreaterThan => {
                        self.advance_token();
                        match self.parse_expr(io) {
                            TickResult::Ok(value2) => {
                                match self.current_token() {
                                    Token::CloseParen => {
                                        self.advance_token();
                                        self.do_arithmetic(value1, value2, op)
                                    }
                                    _ => TickResult::Err(TickError::UnmatchedParen)
                                }
                            }
                            TickResult::AwaitInput => TickResult::Err(TickError::NestedInput),
                            TickResult::Err(e) => TickResult::Err(e),
                            TickResult::Finished => panic!("Program ended too soon."),
                        }
                    }
                    _ => panic!("Missing operator: {:?}", op)//TickResult::Err(TickError::MissingBinaryOperator)
                }
            }
            TickResult::AwaitInput => TickResult::Err(TickError::NestedInput),
            TickResult::Err(e) => TickResult::Err(e)
        }
    }

    fn do_arithmetic(&mut self, value1: Value, value2: Value, op: Token<MAX_LITERAL_CHARS>) -> TickResult<Value> {
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
            ValueType::String => {
                match value2.t {
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
                }
            }
            ValueType::Boolean => todo!(),
        }
    }

    fn string_not_string(&mut self, op: Token<MAX_LITERAL_CHARS>) -> TickResult<Value> {
        if let Token::Equal = op {
            self.malloc_boolean(false)
        } else {
            TickResult::Err(TickError::IllegalBinaryOperator)
        }
    }

    fn perform_binary_op<N:Add<Output=N> + Sub<Output=N> + Mul<Output=N> + Div<Output=N> + PartialOrd + PartialEq, F:Fn(N) -> u64>(&mut self, v1: N, v2: N, op: Token<MAX_LITERAL_CHARS>, vt: ValueType, encoder_u64: F) -> TickResult<Value> {
        match op {
            Token::Plus => {
                self.malloc_numeric_value(encoder_u64(v1 + v2), vt)
            }
            Token::Minus => {
                self.malloc_numeric_value(encoder_u64(v1 - v2), vt)
            }
            Token::Times => {
                self.malloc_numeric_value(encoder_u64(v1 * v2), vt)
            }
            Token::Divide => {
                self.malloc_numeric_value(encoder_u64(v1 / v2), vt)
            }
            Token::LessThan => {
                self.malloc_boolean(v1 < v2)
            }
            Token::GreaterThan => {
                self.malloc_boolean(v1 > v2)
            }
            Token::Equal => {
                self.malloc_boolean(v1 == v2)
            }
            _ => TickResult::Err(TickError::IllegalBinaryOperator)
        }
    }

    fn parse_not<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<Value> {
        match self.parse_expr(io) {
            TickResult::Ok(arg) => {
                match arg.t {
                    ValueType::Boolean => self.malloc_boolean(!self.load_boolean(arg.location)),
                    _ => TickResult::Err(TickError::NeedsBoolean)
                }
            }
            TickResult::Finished => panic!("Program ended too soon."),
            TickResult::AwaitInput => TickResult::Err(TickError::NestedInput),
            TickResult::Err(e) => TickResult::Err(e),
        }
    }

    fn parse_negate<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<Value> {
        match self.parse_expr(io) {
            TickResult::Ok(arg) => {
                match arg.t {
                    ValueType::Integer => self.malloc_numeric_value(make_unsigned_from(-self.load_int(arg.location)), ValueType::Integer),
                    ValueType::Float => todo!(),
                    _ => TickResult::Err(TickError::NotNegateable)
                }
            }
            TickResult::Finished => panic!("Program ended too soon."),
            TickResult::AwaitInput => TickResult::Err(TickError::NestedInput),
            TickResult::Err(e) => TickResult::Err(e),
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

    fn parse_input<I: InterpreterOutput>(&mut self, io: &mut I) -> TickResult<Value> {
        match self.current_token() {
            Token::OpenParen => {
                self.advance_token();
                match self.parse_expr(io) {
                    TickResult::Ok(v) => match self.print_value(&v, io) {
                        TickResult::Ok(_) => {}
                        TickResult::Err(e) => return TickResult::Err(e),
                        TickResult::AwaitInput => panic!("Input within input not implemented"),
                        TickResult::Finished => panic!("Program ended too soon."),
                    },
                    TickResult::AwaitInput => {
                        panic!("Input within input not implemented");
                    }
                    TickResult::Err(e) => return TickResult::Err(e),
                    TickResult::Finished => panic!("Program ended too soon."),
                }
                match self.current_token() {
                    Token::CloseParen => {
                        self.advance_token();
                        return TickResult::AwaitInput;
                    }
                    _ => return TickResult::Err(TickError::UnmatchedParen)
                }
            }
            _ => return TickResult::Err(TickError::MissingOpeningParen),
        }
    }

    fn malloc_boolean(&mut self, value: bool) -> TickResult<Value> {
        let value = if value {u64::MAX} else {0};
        self.malloc_numeric_value(value, ValueType::Boolean)
    }

    fn malloc_number(&mut self, n: &[char]) -> TickResult<Value> {
        let (t, value) = if n.contains(&'.') {
            let float_val = parse_float_from(n);
            (ValueType::Float, float_val.to_bits())
        } else {
            (ValueType::Integer, parse_int_from(n))
        };
        self.malloc_numeric_value(value, t)
    }

    fn malloc_numeric_value(&mut self, value: u64, t: ValueType) -> TickResult<Value> {
        match self.heap.malloc(1, &self.stack) {
            HeapResult::Ok(p) => match self.heap.store(p, value) {
                HeapResult::Ok(_) => TickResult::Ok(Value { location: p, t }),
                HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
            },
            HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
        }
    }

    fn malloc_string(&mut self, s: &[char]) -> TickResult<Value> {
        let num_chars = s.iter().take_while(|c| **c != '\0').count();
        match self.heap.malloc(num_chars, &self.stack) {
            HeapResult::Ok(location) => {
                let mut p = Some(location);
                for i in 0..num_chars {
                    let pt = p.unwrap();
                    match self.heap.store(pt, s[i] as u64) {
                        HeapResult::Ok(_) => {
                            p = pt.next();
                        }
                        HeapResult::Err(e) => {
                            return TickResult::Err(TickError::HeapIssue(e))
                        }
                    }
                }
                TickResult::Ok(Value {
                    location,
                    t: ValueType::String,
                })
            }
            HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
        }
    }

    fn print_value<I: InterpreterOutput>(&self, v: &Value, io: &mut I) -> TickResult<()> {
        let mut output_buffer = [0; OUTPUT_WIDTH];
        match v.output::<G>(&self.heap, &mut output_buffer) {
            TickResult::Ok(num_words) => {
                io.print(&output_buffer[0..num_words]);
                TickResult::Ok(())
            }
            TickResult::Err(e) => TickResult::Err(e),
            TickResult::AwaitInput => panic!("This should never happen."),
            TickResult::Finished => panic!("Program ended too soon."),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct BraceStacker<const MAX_DEPTH: usize> {
    brace_depth: usize,
    loop_back_tokens: [Option<usize>; MAX_DEPTH]
}

impl<const MAX_DEPTH: usize> BraceStacker<MAX_DEPTH> {
    fn new() -> Self {
        Self {brace_depth: 0, loop_back_tokens: [None; MAX_DEPTH]}
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
        Self { location: Default::default(), t: ValueType::Integer }
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
    ) -> TickResult<usize> {
        match self.t {
            ValueType::Integer => match heap.load(self.location) {
                HeapResult::Ok(w) => i64_into_buffer(make_signed_from(w), buffer),
                HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
            },
            ValueType::String => {
                let mut p = Some(self.location);
                let end = min(self.location.len(), buffer.len() - 1);
                for i in 0..end {
                    let pt = p.unwrap();
                    match heap.load(pt) {
                        HeapResult::Ok(value) => {
                            buffer[i] = value as u8;
                            p = pt.next();
                        }
                        HeapResult::Err(e) => return TickResult::Err(TickError::HeapIssue(e)),
                    }
                }
                buffer[end] = '\n' as u8;
                TickResult::Ok(end + 1)
            }
            ValueType::Float => match heap.load(self.location) {
                HeapResult::Ok(w) => f64_into_buffer(f64::from_bits(w), buffer),
                HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
            }
            ValueType::Boolean => {
                match heap.load(self.location) {
                    HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
                    HeapResult::Ok(b) => {
                        let bytes = if b == 0 {"false\n".as_bytes()} else {"true\n".as_bytes()};
                        for (i, c) in bytes.iter().enumerate() {
                            buffer[i] = *c;
                        }
                        TickResult::Ok(buffer.len() + 1)
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ValueType {
    Integer,
    Float,
    String,
    Boolean
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

pub fn i64_into_buffer(mut value: i64, buffer: &mut [u8]) -> TickResult<usize> {
    if value == 0 {
        buffer[0] = '0' as u8;
        buffer[1] = '\n' as u8;
        return TickResult::Ok(2);
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

    TickResult::Ok(i+1)
}

pub fn f64_into_buffer(mut value: f64, buffer: &mut [u8]) -> TickResult<usize> {
    todo!()
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
pub struct StackFrame<const MAX_LOCAL_VARS: usize, const MAX_LITERAL_CHARS: usize> {
    vars: BareMetalMap<Variable<MAX_LITERAL_CHARS>, Value, MAX_LOCAL_VARS>,
}

impl<const MAX_LOCAL_VARS: usize, const MAX_LITERAL_CHARS: usize> Tracer for StackFrame<MAX_LOCAL_VARS, MAX_LITERAL_CHARS> {
    // If I ever add structs or arrays, this will need to be modified
    // to trace out those internal pointers as well.
    fn trace(&self, blocks_used: &mut [bool]) {
        for i in 0..self.vars.len() {
            blocks_used[self.vars.get_at(i).unwrap().block_num()] = true;
        }
    }
}

impl<const MAX_LOCAL_VARS: usize, const MAX_LITERAL_CHARS: usize> StackFrame<MAX_LOCAL_VARS, MAX_LITERAL_CHARS> {
    pub fn new() -> Self {
        Self {vars: BareMetalMap::new()}
    }

    fn assign(&mut self, variable: Variable<MAX_LITERAL_CHARS>, value: Value) {
        self.vars.put(variable, value);
    }

    fn look_up(&self, variable: Variable<MAX_LITERAL_CHARS>) -> Option<Value> {
        self.vars.get(variable)
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
    chars[0].is_numeric()
        && chars.iter().filter(|c| **c == '.').count() <= 1
        && chars.iter().all(|c| c.is_numeric() || *c == '.')
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
            (u64::MAX - 100, -101)
        ] {
            println!("{value}");
            let signed = make_signed_from(bits);
            assert_eq!(value, signed);
            assert_eq!(bits, make_unsigned_from(signed));
        }
    }
}
