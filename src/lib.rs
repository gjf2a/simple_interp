#![cfg_attr(not(test), no_std)]

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

#[derive(Debug)]
pub enum TokenResult<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> {
    Ok(Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS>),
    Err(TokenError),
}

impl<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> TokenResult<MAX_TOKENS, MAX_LITERAL_CHARS> {
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
}

#[derive(Debug)]
pub struct Tokenized<const MAX_TOKENS:usize, const MAX_LITERAL_CHARS: usize> {
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
            Some(e) => return TokenResult::Err(e)
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
    }
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
    }
}

impl<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS> {
    pub fn tokenize(text: &str) -> TokenResult<MAX_TOKENS, MAX_LITERAL_CHARS> {
        let mut result = Self {num_tokens: 0, tokens: [Token::False; MAX_TOKENS]};
        let mut str_buffer = ['\0'; MAX_LITERAL_CHARS];
        let mut num_chars = 0;
        let mut inside_string = false;
        for c in text.chars() {
            if inside_string && c == '"' {
                inside_string = false;
                try_token!(result, Token::String(str_buffer), num_chars, str_buffer);
            } else if c == '"' {
                inside_string = true;
            } else if let Some(token) = Self::match_single(c) {
                try_terminate!(result, num_chars, str_buffer);
                result.push_token(token);
            } else if inside_string || !c.is_whitespace() {
                try_add_char!(c, num_chars, str_buffer);
            } else if c.is_whitespace() {
                try_terminate!(result, num_chars, str_buffer);
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
            _ => None
        }
    }

    fn match_multiple(chars: &[char]) -> Option<Token<MAX_LITERAL_CHARS>> {
        match chars {
            [':', '='] => Some(Token::Assign),
            ['=','='] => Some(Token::Equal),
            ['t', 'r', 'u', 'e'] => Some(Token::True),
            ['f', 'a', 'l', 's', 'e'] => Some(Token::False),
            ['w', 'h', 'i', 'l', 'e'] => Some(Token::While),
            ['i', 'f'] => Some(Token::If),
            ['e', 'l', 's', 'e'] => Some(Token::Else),
            ['a', 'n', 'd'] => Some(Token::And),
            ['o', 'r'] => Some(Token::Or),
            ['n', 'o', 't'] => Some(Token::Not),
            _ => None
        }
    }

    fn is_number(chars: &[char]) -> bool {
        chars[0].is_numeric() && chars.iter().filter(|c| **c == '.').count() <= 1 && chars.iter().all(|c| c.is_numeric() || *c == '.')
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Display;
    use std::fs::read_to_string;
    use super::*;

    impl <const MAX_LITERAL_CHARS: usize> Token<MAX_LITERAL_CHARS> {
        fn var(&self) -> Option<String> {
            match self {
                Token::Symbol(chars) | Token::Number(chars) | Token::String(chars) => {
                    Some(chars.iter().take_while(|c| **c != '\0').collect())
                }
                _ => None
            }
        }
    }

    impl <const MAX_LITERAL_CHARS: usize> Display for Token<MAX_LITERAL_CHARS> {
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
                token => write!(f, "{token:?}")
            }
        }
    }

    impl<const MAX_TOKENS: usize, const MAX_LITERAL_CHARS: usize> Display for Tokenized<MAX_TOKENS, MAX_LITERAL_CHARS> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            for i in 0..self.num_tokens {
                writeln!(f, "{}", self.tokens[i])?;
            }
            Ok(())
        }
    }

    #[test]
    fn it_works() {
        let example = read_to_string("programs/average.prog").unwrap();
        let tokens = Tokenized::<500, 100>::tokenize(example.as_str()).unwrap();
        println!("{tokens}");
    }
}