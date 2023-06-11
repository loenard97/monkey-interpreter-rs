#![allow(dead_code)]

use crate::{*, ast::{IntegerLiteral, PrefixExpression, Boolean, IfExpression, FunctionLiteral}};

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

struct Parser {
    lexer: Lexer,
    errors: String,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser { 
            lexer, 
            errors: String::new(), 
            cur_token: None, 
            peek_token: None, 
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn prefix_parse_fn(token: Token) -> fn(&Self) -> Expression {
        match token {
            Token::Identifier(_)                                                => Parser::parse_identifier,
            Token::Number(_)                                                    => Parser::parse_integer_literal,
            Token::Bang                     | Token::Minus                      => Parser::parse_prefix_expression,
            Token::Keyword(Keyword::True)   | Token::Keyword(Keyword::False)    => Parser::parse_boolean,
            Token::Lparen                                                       => Parser::parse_grouped_expression,
            Token::Keyword(Keyword::If)                                         => Parser::parse_if_expression,
            Token::Keyword(Keyword::Function)                                   => Parser::parse_function_literal,
            _                                                                   => panic!(),
        }
    }

    fn infix_parse_fn(token: Token) -> fn(&Self) -> Expression {
        match token {
            Token::Lparen   => Parser::parse_infix_expression,
            _               => Parser::parse_call_expression,
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
                self.errors.push_str(&format!("expected next token to be {:?}, got {:?} instead", &token, self.peek_token));
                false
            },
        }
    }

    fn parse_identifier(&self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let value = self.cur_token.unwrap().literal().to_string();

        Expression::Identifier(Identifier::new(token, value))
    }

    fn errors(&self) -> String {
        self.errors.clone()
    }

    fn peek_error(&mut self, token: &Token) {
        self.errors.push_str(&format!("expected next token to be {:?}, got {:?} instead", token, self.peek_token));
    }

    fn no_prefix_parser_fn_error(&mut self, token: Token) {
        self.errors.push_str(&format!("no prefix parse function for {:?} found", token));
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next()
    }

    fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.cur_token.is_some() {
            let stmt = self.parse_statement();
            statements.push(stmt.unwrap());
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

        let token = self.cur_token.clone().unwrap();
        let value = self.cur_token.clone().unwrap().literal().to_string();
        let name = Identifier::new(token, value);

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest).unwrap();

        if self.peek_token == Some(Token::Semicolon) {
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

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::ExpressionStatement(ExpressionStatement::new(token.unwrap(), expression.unwrap())))
    }

    fn parse_integer_literal(&self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let value = token.literal().parse().unwrap();

        Expression::IntegerLiteral(IntegerLiteral::new(token, value))
    }

    fn parse_prefix_expression(&self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let operator = token.literal().to_string();

        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Prefix).unwrap());

        Expression::PrefixExpression(PrefixExpression::new(token, operator, right))
    }

    fn parse_boolean(&self) -> Expression {
        let token = self.cur_token.clone().unwrap();
        let value = token == Token::Keyword(Keyword::True);

        Expression::Boolean(Boolean::new(token, value))
    }

    fn parse_grouped_expression(&self) -> Expression {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest).unwrap();

        if self.peek_token != Some(Token::Lparen) {
            println!("parse_grouped_expression: no lparen");
        }

        expression
    }

    fn parse_if_expression(&self) -> Expression {
        let token = self.cur_token.clone().unwrap();

        if !self.expect_peek(Token::Lparen) {
            println!("parse if expression: no lparen");
        }

        self.next_token();

        let condition = Box::new(self.parse_expression(Precedence::Lowest).unwrap());

        if !self.expect_peek(Token::Rparen) {
            println!("parse if expression: no rparen");
        }
        
        if !self.expect_peek(Token::Rbrace) {
            println!("parse if expression: no rbrace");
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(Token::Keyword(Keyword::Else)) {
            self.next_token();

            if !self.expect_peek(Token::Lbrace) {
                println!("parse if expression: no lbrace");
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Expression::IfExpression(IfExpression::new(token, condition, consequence, alternative))
    }

    fn parse_function_literal(&self) -> Expression {
        let token = self.cur_token.clone().unwrap();

        if !self.expect_peek(Token::Lparen) {
            println!("parse if expression: no lparen");
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::Lbrace) {
            println!("parse if expression: no lbrace");
        }

        let body = self.parse_block_statement();

        Expression::FunctionLiteral(FunctionLiteral::new(token, parameters, body))
    }

    fn parse_infix_expression(&self) -> Expression {
        
    }

    fn parse_call_expression(&self) -> Expression {
        
    }

    fn parse_expression(&self, precedence: Precedence) -> Option<Expression> {

        let token = self.cur_token.clone().unwrap();

        let prefix = Parser::prefix_parse_fn(token)(self);
        
        None
    }
}
