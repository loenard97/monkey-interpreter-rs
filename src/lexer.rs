#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Keyword {
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Keyword {
    fn from_str(keyword: &str) -> Option<Self> {
        match keyword {
            "fn"        => Some(Self::Function),
            "let"       => Some(Self::Let),
            "true"      => Some(Self::True),
            "false"     => Some(Self::False),
            "if"        => Some(Self::If),
            "else"      => Some(Self::Else),
            "return"    => Some(Self::Return),
            _           => None,
        }
    }

    fn literal(&self) -> &str {
        match self {
            Keyword::Function => "fn",
            Keyword::Let    => "let",
            Keyword::True   => "true",
            Keyword::False  => "false",
            Keyword::If     => "if",
            Keyword::Else   => "else",
            Keyword::Return => "return",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Identifier(String),
    Number(String),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,

    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Eq,
    Neq,

    Keyword(Keyword),

    Eof,
    Illegal,
}

impl Token {
    fn from_identifier(identifier: &str) -> Self {
        match Keyword::from_str(identifier) {
            Some(val) => Token::Keyword(val),
            None => Token::Identifier(identifier.to_string()),
        }
    }

    pub fn literal(&self) -> &str {
        match self {
            Token::Identifier(val) => val,
            Token::Number(val) => val,

            Token::Assign           => "=",
            Token::Plus             => "+",
            Token::Minus            => "-",
            Token::Bang             => "!",
            Token::Asterisk         => "*",
            Token::Slash            => "/",
            Token::Lt               => "<",
            Token::Gt               => ">",

            Token::Comma            => ",",
            Token::Semicolon        => ";",
            Token::Lparen           => "(",
            Token::Rparen           => ")",
            Token::Lbrace           => "{",
            Token::Rbrace           => "}",

            Token::Eq               => "==",
            Token::Neq              => "!=",

            Token::Keyword(val) => val.literal(),

            Token::Eof              => "EOF",
            Token::Illegal          => "Illegal",
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self { input: input.to_string(), position: 0, read_position: 0, ch: '\0' };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_position).unwrap_or('\0');
        self.position = self.read_position;
        self.read_position += 1;
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while self.ch.is_whitespace() {
            self.read_char();
        }

        let tok = match self.ch {
            '=' => {
                if self.input.chars().nth(self.read_position) == Some('=') {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            },
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.input.chars().nth(self.read_position) == Some('=') {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            },
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,

            '\0' => Token::Eof,

            _ => {
                if self.ch.is_alphabetic() {
                    let pos = self.position;
                    while self.ch.is_alphabetic() {
                        self.read_char()
                    }
                    let identifier = &self.input[pos..self.position];
                    return Some(Token::from_identifier(identifier))

                } else if self.ch.is_numeric() {
                    let pos = self.position;
                    while self.ch.is_numeric() {
                        self.read_char();
                    }
                    let number = &self.input[pos..self.position];
                    return Some(Token::Number(number.to_string()))

                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();

        match tok {
            Token::Eof => None,
            _ => Some(tok),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn token() {
        let input = "=+(){},;-!/*<>";
        let tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
            Token::Minus,
            Token::Bang,
            Token::Slash,
            Token::Asterisk,
            Token::Lt,
            Token::Gt,
        ];

        let lexer = Lexer::new(input);

        assert_eq_vec(tokens, lexer.collect());
    }

    #[test]
    fn composed_token() {
        let input = " \
            0 == 0; \
            0 != 1;";
        let tokens = vec![
            Token::Number("0".to_string()),
            Token::Eq,
            Token::Number("0".to_string()),
            Token::Semicolon,

            Token::Number("0".to_string()),
            Token::Neq,
            Token::Number("1".to_string()),
            Token::Semicolon,
        ];

        let lexer = Lexer::new(input);

        assert_eq_vec(tokens, lexer.collect());
    }

    #[test]
    fn statement() {
        let input = " \
            let five = 5; \
            let ten = 10; \
            \
            let add = fn(x, y) { \
                x + y; \
            }; \
            \
            let result = add(five, ten);";
        let tokens = vec![
            Token::Keyword(Keyword::Let),
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Number("5".to_string()),
            Token::Semicolon,

            Token::Keyword(Keyword::Let),
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Number("10".to_string()),
            Token::Semicolon,

            Token::Keyword(Keyword::Let),
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Keyword(Keyword::Function),
            Token::Lparen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,

            Token::Keyword(Keyword::Let),
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::Lparen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
        ];

        let lexer = Lexer::new(input);

        assert_eq_vec(tokens, lexer.collect());

    }

    #[test]
    fn keyword() {
        let input = " \
            if (5 < 10) { \
                return true; \
            } else { \
                return false; \
            }";
        let tokens = vec![
            Token::Keyword(Keyword::If),
            Token::Lparen,
            Token::Number("5".to_string()),
            Token::Lt,
            Token::Number("10".to_string()),
            Token::Rparen,
            Token::Lbrace,

            Token::Keyword(Keyword::Return),
            Token::Keyword(Keyword::True),
            Token::Semicolon,

            Token::Rbrace,
            Token::Keyword(Keyword::Else),
            Token::Lbrace,

            Token::Keyword(Keyword::Return),
            Token::Keyword(Keyword::False),
            Token::Semicolon,

            Token::Rbrace,
        ];

        let lexer = Lexer::new(input);

        assert_eq_vec(tokens, lexer.collect());
    }

    fn assert_eq_vec<T>(a: Vec<T>, b: Vec<T>)
        where T: PartialEq + core::fmt::Debug 
    {
        assert_eq!(a.len(), b.len(), "Length");
        for (i, (ae, be)) in a.iter().zip(b.iter()).enumerate() {
            assert_eq!(ae, be, "Position {}", i);
        }
    }
}
