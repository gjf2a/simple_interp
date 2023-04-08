//#![cfg_attr(not(test), no_std)]

// With weird git errors that mess with rust-analyzer, try this:
//
// export CARGO_NET_GIT_FETCH_WITH_CLI=true
// cargo clean
// cargo update
// cargo build

/*
const GRAMMAR: &str = r#"
    script     := line script | line ;
    line       := statement '\n' ;
    statement  := assignment | while | for | if | call ;
    assignment := symbol sp ':=' sp expr ;
    expr       := expr sp ('+' | '-') sp mulExpr | mulExpr ;
    mulExpr    := mulExpr sp ('*' | '/') sp factor | factor ;
    factor     := '-' term | term ;
    term       := num | symbol | '(' expr ')' ;
    while      := 'while' sp condition sp block ;
    condition  := condition sp 'or' sp andCond ;
    andCond    := andCond sp 'and' sp notCond ;
    notCond    := 'not' sp comparison | comparison ;
    comparison := expr sp comp sp expr | symbol ;
    comp       := '==' | '<=' | '>=' | '<' | '>' | '!=' ;
    block      := '{' sp script '}' ;
    for        := 'for' sp symbol sp 'in' sp range sp block ;
    range      := expr '..' expr ;
    if         := 'if' sp condition sp block sp 'else' sp block | 'if' sp condition sp block ;
    call       := symbol '(' argList ')' | symbol '()' ;
    argList    := expr ',' sp argList | expr ;
    definition := 'fn' sp symbol '(' paramList ')' sp block | 'fn' sp symbol '()' sp block ;
    paramList  := symbol ',' sp paramList | symbol ;
"#;
*/

use core::cmp::min;

use bare_metal_map::BareMetalMap;
use gc_heap::{CopyingHeap, HeapResult, Pointer, Tracer};

pub trait InterpreterIo {
    fn print(&mut self, chars: &[u8]);
    fn input(&mut self, buffer: &mut [u8]);
}

pub struct Interpreter<
    const MAX_TOKENS: usize,
    const MAX_LITERAL_CHARS: usize,
    const STACK_DEPTH: usize,
    const MAX_LOCAL_VARS: usize,
    const HEAP_SIZE: usize,
    const MAX_BLOCKS: usize,
    const OUTPUT_WIDTH: usize,
> {
    tokens: Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS>,
    token: usize,
    heap: CopyingHeap<u64, HEAP_SIZE, MAX_BLOCKS>,
    stack: ProgramStack<MAX_LITERAL_CHARS, STACK_DEPTH, MAX_LOCAL_VARS>,
}

pub enum TickResult<T> {
    Ok(T),
    Err(TickError),
}

impl<T> TickResult<T> {
    pub fn unwrap(self) -> T {
        match self {
            TickResult::Ok(v) => v,
            TickResult::Err(e) => panic!("Interpreter Error: {e:?}"),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> TickResult<U> {
        match self {
            TickResult::Ok(value) => TickResult::Ok(op(value)),
            TickResult::Err(e) => TickResult::Err(e),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TickError {
    HeapIssue(gc_heap::HeapError),
    ParseIssue(ParseError),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParseError {
    UnmatchedParen,
    SyntaxError,
    TokensExhausted,
}

impl<
        const MAX_TOKENS: usize,
        const MAX_LITERAL_CHARS: usize,
        const STACK_DEPTH: usize,
        const MAX_LOCAL_VARS: usize,
        const HEAP_SIZE: usize,
        const MAX_BLOCKS: usize,
        const OUTPUT_WIDTH: usize,
    >
    Interpreter<
        MAX_TOKENS,
        MAX_LITERAL_CHARS,
        STACK_DEPTH,
        MAX_LOCAL_VARS,
        HEAP_SIZE,
        MAX_BLOCKS,
        OUTPUT_WIDTH,
    >
{
    pub fn new(program: &str) -> Self {
        let tokens = Tokenized::tokenize(program).unwrap();
        println!("Read in tokens: {tokens:?}");
        Self {
            tokens,
            token: 0,
            heap: CopyingHeap::new(),
            stack: ProgramStack::default(),
        }
    }

    pub fn tick<I: InterpreterIo>(&mut self, io: &mut I) -> TickResult<()> {
        println!("*** tick");
        match self.tokens.tokens[self.token] {
            Token::Print => {
                println!("*** Print token");
                self.token += 1;
                match self.tokens.tokens[self.token] {
                    Token::OpenParen => {
                        self.token += 1;
                        match self.parse_expr() {
                            TickResult::Ok(v) => match self.print_value(&v, io) {
                                TickResult::Ok(_) => {}
                                TickResult::Err(e) => return TickResult::Err(e),
                            },
                            TickResult::Err(e) => return TickResult::Err(e),
                        }
                        match self.tokens.tokens[self.token] {
                            Token::CloseParen => {
                                self.token += 1;
                            }
                            _ => {
                                return TickResult::Err(TickError::ParseIssue(
                                    ParseError::UnmatchedParen,
                                ))
                            }
                        }
                    }
                    _ => return TickResult::Err(TickError::ParseIssue(ParseError::SyntaxError)),
                }
            }
            Token::Symbol(s) => {
                self.token += 1;
                match self.tokens.tokens[self.token] {
                    Token::Assign => {}
                    Token::OpenParen => {
                        todo!("Function call");
                    }
                    _ => return TickResult::Err(TickError::ParseIssue(ParseError::SyntaxError)),
                }
            }
            _ => return TickResult::Err(TickError::ParseIssue(ParseError::SyntaxError)),
        }
        TickResult::Ok(())
    }

    fn parse_expr(&mut self) -> TickResult<Value> {
        match self.tokens.tokens[self.token] {
            Token::Number(n) => {
                self.token += 1;
                let (t, v) = if n.contains(&'.') {
                    let float_val = make_float_from(&n);
                    (ValueType::Integer, float_val.to_bits())
                } else {
                    (ValueType::Integer, make_int_from(&n))
                };
                match self.heap.malloc(1, &self.stack) {
                    HeapResult::Ok(p) => match self.heap.store(p, v) {
                        HeapResult::Ok(_) => TickResult::Ok(Value { location: p, t }),
                        HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
                    },
                    HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
                }
            }
            Token::String(s) => {
                println!("**string token {:?}", s);
                self.token += 1;
                let num_chars = s.iter().take_while(|c| **c != '\0').count();
                match self.heap.malloc(num_chars, &self.stack) {
                    HeapResult::Ok(location) => {
                        let mut p = Some(location);
                        for i in 0..num_chars {
                            println!("{i} {}", s[i]);
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
            _ => TickResult::Err(TickError::ParseIssue(ParseError::TokensExhausted)),
        }
    }

    fn print_value<I: InterpreterIo>(&self, v: &Value, io: &mut I) -> TickResult<()> {
        let mut output_buffer = [0; OUTPUT_WIDTH];
        match v.output(&self.heap, &mut output_buffer) {
            TickResult::Ok(num_words) => {
                io.print(&output_buffer[0..num_words]);
                TickResult::Ok(())
            }
            TickResult::Err(e) => TickResult::Err(e),
        }
    }
}

struct Value {
    location: Pointer,
    t: ValueType,
}

impl Value {
    fn output<const HEAP_SIZE: usize, const MAX_BLOCKS: usize>(
        &self,
        heap: &CopyingHeap<u64, HEAP_SIZE, MAX_BLOCKS>,
        buffer: &mut [u8],
    ) -> TickResult<usize> {
        match self.t {
            ValueType::Integer => match heap.load(self.location) {
                HeapResult::Ok(mut w) => {
                    if w == 0 {
                        buffer[0] = '0' as u8;
                        return TickResult::Ok(1);
                    }
                    let mut start = if w >> 63 == 1 {
                        buffer[0] = '-' as u8;
                        w &= u64::MAX >> 1;
                        1
                    } else {
                        0
                    };
                    let mut i = start;
                    while w > 0 && i < buffer.len() {
                        buffer[i] = (w % 10) as u8 + '0' as u8;
                        w /= 10;
                        i += 1;
                    }
                    while start < i {
                        let temp = buffer[start];
                        buffer[start] = buffer[i - start - 1];
                        buffer[i - start - 1] = temp;
                        start += 1;
                    }

                    TickResult::Ok(i)
                }
                HeapResult::Err(e) => TickResult::Err(TickError::HeapIssue(e)),
            },
            ValueType::Float => todo!(),
            ValueType::String => {
                let mut p = Some(self.location);
                for i in 0..min(self.location.len(), buffer.len()) {
                    let pt = p.unwrap();
                    match heap.load(pt) {
                        HeapResult::Ok(value) => {
                            buffer[i] = value as u8;
                            p = pt.next();
                        }
                        HeapResult::Err(e) => return TickResult::Err(TickError::HeapIssue(e)),
                    }
                }
                TickResult::Ok(self.location.len())
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ValueType {
    Integer,
    Float,
    String,
}

fn make_int_from(chars: &[char]) -> u64 {
    let mut value = 0;
    for c in chars.iter().take_while(|c| **c != '\0') {
        value *= 10;
        value += *c as u64 - '0' as u64;
    }
    value
}

fn make_float_from(chars: &[char]) -> f64 {
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

#[derive(Copy, Clone)]
struct ProgramStack<
    const MAX_LITERAL_CHARS: usize,
    const STACK_DEPTH: usize,
    const MAX_LOCAL_VARS: usize,
> {
    stack: [StackFrame<MAX_LOCAL_VARS, MAX_LITERAL_CHARS>; STACK_DEPTH],
    stack_level: usize,
}

impl<const MAX_LITERAL_CHARS: usize, const STACK_DEPTH: usize, const MAX_LOCAL_VARS: usize>
    ProgramStack<MAX_LOCAL_VARS, STACK_DEPTH, MAX_LITERAL_CHARS>
{
}

impl<const MAX_LITERAL_CHARS: usize, const STACK_DEPTH: usize, const MAX_LOCAL_VARS: usize> Default
    for ProgramStack<MAX_LOCAL_VARS, STACK_DEPTH, MAX_LITERAL_CHARS>
{
    fn default() -> Self {
        Self {
            stack: [StackFrame::default(); STACK_DEPTH],
            stack_level: Default::default(),
        }
    }
}

impl<const MAX_LITERAL_CHARS: usize, const STACK_DEPTH: usize, const MAX_LOCAL_VARS: usize> Tracer
    for ProgramStack<MAX_LOCAL_VARS, STACK_DEPTH, MAX_LITERAL_CHARS>
{
    fn trace(&self, blocks_used: &mut [bool]) {
        todo!()
    }
}

#[derive(Copy, Clone, Default)]
pub struct StackFrame<const MAX_LOCAL_VARS: usize, const MAX_LITERAL_CHARS: usize> {
    start_token: usize,
    vars: BareMetalMap<Token<MAX_LITERAL_CHARS>, Pointer, MAX_LOCAL_VARS>,
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
    Print,
    Input,
}

impl<const MAX_LITERAL_CHARS: usize> Default for Token<MAX_LITERAL_CHARS> {
    fn default() -> Self {
        Token::False
    }
}

#[derive(Debug)]
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
                    if Self::is_number(slice) {
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

    fn is_number(chars: &[char]) -> bool {
        chars[0].is_numeric()
            && chars.iter().filter(|c| **c == '.').count() <= 1
            && chars.iter().all(|c| c.is_numeric() || *c == '.')
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Display;
    use std::collections::VecDeque;
    use std::fs::read_to_string;

    use super::*;

    #[derive(Clone, Debug, Default)]
    struct TestIo {
        printed: String,
        inputs: VecDeque<String>,
    }

    impl InterpreterIo for TestIo {
        fn print(&mut self, chars: &[u8]) {
            for c in chars.iter() {
                self.printed.push(*c as char);
            }
        }

        fn input(&mut self, buffer: &mut [u8]) {
            let msg = self.inputs.pop_front().unwrap();
            for (i, c) in msg.char_indices().take(buffer.len()) {
                buffer[i] = c as u8;
            }
            self.inputs.push_back(msg);
        }
    }

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
    fn test_hello_world() {
        println!("Reading to string....");
        let s = std::fs::read_to_string("programs/hello.prog").unwrap();
        println!("Program: {s}");
        // TODO: Stack overflow with heap size 16384. Look at this at some point.
        //let mut interp: Interpreter<1000, 30, 50, 20, 16384, 4096, 80> =
        let mut interp: Interpreter<1000, 30, 50, 20, 4096, 4096, 80> =
            Interpreter::new(s.as_str());
        println!("Built interpreter");
        let mut io = TestIo::default();
        interp.tick(&mut io).unwrap();
        assert_eq!(io.printed, "Hello, world!");
    }

    #[test]
    fn test_parse_int() {
        for s in [['1', '0'], ['1', '2'], ['2', '1'], ['4', '3']] {
            let expected = s.iter().collect::<String>().parse::<u64>().unwrap();
            assert_eq!(expected, make_int_from(&s));
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
            assert_eq!(expected, make_float_from(&s));
        }
    }
}
