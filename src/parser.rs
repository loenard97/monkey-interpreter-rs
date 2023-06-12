#![allow(dead_code)]

use std::fmt::Display;

use crate::*;

#[derive(Clone, Debug)]
pub struct ParserError {
    message: String,
}

impl ParserError {
    fn new(message: String) -> Self {
        ParserError { message }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser Error: {}", self.message)
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    Comparison,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn from_token(token: Token) -> Self {
        match token {
            Token::Eq       | Token::Neq        => Self::Equals,
            Token::Lt       | Token::Gt         => Self::Comparison,
            Token::Plus     | Token::Minus      => Self::Sum,
            Token::Slash    | Token::Asterisk   => Self::Product,
            Token::Lparen                       => Self::Call,
            _                                   => Self::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    errors: Vec<ParserError>,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser { 
            lexer, 
            errors: vec![], 
            cur_token: None, 
            peek_token: None, 
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> String {
        self.errors.iter().map(|e| { 
                let mut s = String::new();
                s.push_str(&e.to_string());
                s.push('\n');
                s
            })
            .collect()
    }

    pub fn n_errors(&self) -> usize {
        self.errors.iter().len()
    }

    fn prefix_parse_fn(token: Token) -> Option<fn(&mut Self) -> Expression> {
        match token {
            Token::Identifier(_)                                                => Some(Parser::parse_identifier),
            Token::Number(_)                                                    => Some(Parser::parse_integer_literal),
            Token::Bang                     | Token::Minus                      => Some(Parser::parse_prefix_expression),
            Token::Keyword(Keyword::True)   | Token::Keyword(Keyword::False)    => Some(Parser::parse_boolean),
            Token::Lparen                                                       => Some(Parser::parse_grouped_expression),
            Token::Keyword(Keyword::If)                                         => Some(Parser::parse_if_expression),
            Token::Keyword(Keyword::Function)                                   => Some(Parser::parse_function_literal),
            _                                                                   => None,
        }
    }

    fn infix_parse_fn(token: Token) -> Option<fn(&mut Self, Expression) -> Expression> {
        match token {
            Token::Plus | Token::Minus | Token::Slash | Token::Asterisk | Token::Eq | Token::Neq | Token::Lt | Token::Gt => Some(Parser::parse_infix_expression),
            Token::Lparen   => Some(Parser::parse_call_expression),
            _               => None,
        }
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == Some(token)
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == Some(token)
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        match &self.peek_token {
            Some(_) => {
                self.next_token();
                true
            },
            None => {
                let error = ParserError::new(format!("expected next token to be {:?}, got {:?} instead", &token, self.peek_token));
                self.errors.push(error);
                false
            },
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let value = token.literal().to_string();

        Expression::Identifier(Identifier::new(token, value))
    }

    fn peek_error(&mut self, token: &Token) {
        let error = ParserError::new(format!("expected next token to be {:?}, got {:?} instead", token, self.peek_token));
        self.errors.push(error);
    }

    fn no_prefix_parser_fn_error(&mut self, token: Token) {
        let error = ParserError::new(format!("no prefix parse function for {:?} found", token));
        self.errors.push(error);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.cur_token.is_some() {
            match self.parse_statement() {
                Some(val) => statements.push(val),
                None => {
                    let error = ParserError::new(format!("expected statement, got None instead"));
                    self.errors.push(error);
                },
            };
            self.next_token();
        }

        Program::new(statements)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Some(Token::Keyword(Keyword::Let))      => self.parse_let_statement(),
            Some(Token::Keyword(Keyword::Return))   => self.parse_return_statement(),
            _                                       => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if self.cur_token != Some(Token::Keyword(Keyword::Let)) {
            return None;
        }
        self.next_token();

        let token = self.cur_token.clone().unwrap();
        let value = self.cur_token.clone().unwrap().literal().to_string();
        let name = Identifier::new(token, value);

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(val) => val,
            None => {
                let error = ParserError::new(format!("expected expression, got None instead"));
                self.errors.push(error);
                return None;
            },
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::LetStatement(LetStatement::new(Token::Keyword(Keyword::Let), name, value)))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::ReturnStatement(ReturnStatement::new(token.unwrap(), value.unwrap())))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(val) => val,
            None => {
                let error = ParserError::new(format!("expected expression, got None instead"));
                self.errors.push(error);
                return None;
            }
        };

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::ExpressionStatement(ExpressionStatement::new(token.unwrap(), expression)))
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let value = token.literal().parse().unwrap();

        Expression::IntegerLiteral(IntegerLiteral::new(token, value))
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let operator = token.literal().to_string();

        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Prefix).unwrap());

        Expression::PrefixExpression(PrefixExpression::new(token, operator, right))
    }

    fn parse_boolean(&mut self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let value = token == Token::Keyword(Keyword::True);

        Expression::Boolean(Boolean::new(token, value))
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest).unwrap();

        if self.peek_token != Some(Token::Lparen) {
            let error = ParserError::new(format!("parse_grouped_expression: no lparen"));
            self.errors.push(error);
        }

        expression
    }

    fn parse_if_expression(&mut self) -> Expression {
        let token = self.cur_token.clone().unwrap();

        if !self.expect_peek(Token::Lparen) {
            let error = ParserError::new(format!("parse if expression: no lparen"));
            self.errors.push(error);
        }

        self.next_token();

        let condition = Box::new(self.parse_expression(Precedence::Lowest).unwrap());

        if !self.expect_peek(Token::Rparen) {
            let error = ParserError::new(format!("parse if expression: no rparen"));
            self.errors.push(error);
        }
        
        if !self.expect_peek(Token::Rbrace) {
            let error = ParserError::new(format!("parse if expression: no rbrace"));
            self.errors.push(error);
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(Token::Keyword(Keyword::Else)) {
            self.next_token();

            if !self.expect_peek(Token::Lbrace) {
                let error = ParserError::new(format!("parse if expression: no lbrace"));
                self.errors.push(error);
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Expression::IfExpression(IfExpression::new(token, condition, consequence, alternative))
    }

    fn parse_function_literal(&mut self) -> Expression {
        let token = self.cur_token.clone().unwrap();

        if !self.expect_peek(Token::Lparen) {
            let error = ParserError::new(format!("parse if expression: no lparen"));
            self.errors.push(error);
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::Lbrace) {
            let error = ParserError::new(format!("parse if expression: no lbrace"));
            self.errors.push(error);
        }

        let body = self.parse_block_statement();

        Expression::FunctionLiteral(FunctionLiteral::new(token, parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifier = vec![];

        if self.peek_token_is(Token::Rparen) {
            self.next_token();
            return identifier;
        }

        self.next_token();

        let token = self.cur_token.clone().unwrap();
        let value = token.literal().to_string();
        let ident = Identifier::new(token, value);

        identifier.push(ident);

        if self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();


            let token = self.cur_token.clone().unwrap();
            let value = token.literal().to_string();
            let ident = Identifier::new(token, value);

            identifier.push(ident);
        }

        if self.expect_peek(Token::Rparen) {
            let error = ParserError::new(format!("parse function parameters: no rparen"));
            self.errors.push(error);
        }

        identifier
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let operator = token.literal().to_string();

        let precedence = self.cur_precedence();
        self.next_token();
        let right = Box::new(self.parse_expression(precedence).unwrap());
        let left = Box::new(left);

        Expression::InfixExpression(InfixExpression::new(token, operator, left, right))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let function = Box::new(function);
        let arguments = self.parse_call_arguments();

        Expression::CallExpression(CallExpression::new(token, function, arguments))
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();

        if self.peek_token_is(Token::Rparen) {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            
            args.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_peek(Token::Rparen) {
            let error = ParserError::new(format!("parse call arguments: no rparen"));
            self.errors.push(error);
        }

        args
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.cur_token.clone().unwrap();
        let mut statements = Vec::new();

        self.next_token();

        if !self.cur_token_is(Token::Rbrace) && !self.cur_token_is(Token::Eof) {
            let stmt = self.parse_statement().unwrap();
            statements.push(stmt);
            self.next_token();
        }

        BlockStatement::new(token, statements)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let token = self.cur_token.clone().unwrap();

        let mut left = match Parser::prefix_parse_fn(token) {
            Some(val) => val(self),
            None => return None,
        };

        if self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            let peek_token = self.peek_token.clone().unwrap();
            
            let infix = match Parser::infix_parse_fn(peek_token) {
                Some(val) => val,
                None => return Some(left),
            };
            self.next_token();
            left = infix(self, left);
        }

        Some(left)
    }

    fn cur_precedence(&self) -> Precedence {
        let token = self.cur_token.clone().unwrap();
        Precedence::from_token(token)
    }

    fn peek_precedence(&self) -> Precedence {
        let token = self.peek_token.clone().unwrap();
        Precedence::from_token(token)
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn let_statement() {
        let input = "let x = 5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn error() {
        let input = "let x = ;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let error_msg = "\
            Parser Error: expected expression, got None instead\n\
            Parser Error: expected statement, got None instead\n\
        ";

        println!("{}", parser.errors());
        assert_eq!(2, parser.n_errors());
        assert_eq!(error_msg.to_string(), parser.errors());
        assert_eq!("".to_string(), program.to_string());
    }

    #[test]
    fn bool_statement() {
        let input = "let a = true;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn return_statement() {
        let input = "return 5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn math_expression() {
        let input = "let x = 5 + 10;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn if_expression() {
        let input = "\
            if (true) {\
                return 1;\
            }\
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn if_expression_alternative() {
        let input = "\
            if (true) {\
                return 1;\
            } else {\
                return 0;\
            }\
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn function() {
        let input = "let f = f(3, 4);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }

    #[test]
    fn call() {
        let input = "f(3, 4);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{}", parser.errors());
        assert_eq!(0, parser.n_errors());
        assert_eq!(input.to_string(), program.to_string());
    }
}
