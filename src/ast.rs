#![allow(dead_code)]

use std::fmt::Display;

use crate::Token;

trait Node {
    fn token_literal(&self) -> &str;
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(val)               => write!(f, "{}", val),
            Statement::ReturnStatement(val)         => write!(f, "{}", val),
            Statement::ExpressionStatement(val) => write!(f, "{}", val),
            Statement::BlockStatement(val)           => write!(f, "{}", val),
        }
    }
}

pub enum Expression {
    Identifier(Identifier),
    Boolean(Boolean),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(val)              => write!(f, "{}", val.value),
            Expression::Boolean(val)                    => write!(f, "{}", val.value),
            Expression::IntegerLiteral(val)      => write!(f, "{}", val.value),
            Expression::PrefixExpression(val)  => write!(f, "prefix"),
            Expression::InfixExpression(val)    => write!(f, "infix"),
            Expression::IfExpression(val)          => {
                match &val.alternative {
                    Some(alternative) => write!(f, "if ({}) {{{}}} else {{{}}}", val.condition, val.consequence, alternative),
                    None => write!(f, "if ({}) {{{}}}", val.condition, val.consequence)
                }
            },
            Expression::FunctionLiteral(val)    => write!(f, "fn"),
            Expression::CallExpression(val)      => write!(f, "call()"),
        }
    }
}

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }

    fn token_literal(&self) -> String {
        match self.statements.get(0) {
            Some(val) => val.to_string(),
            None => String::new(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.statements.iter().map(|s| { s.to_string() }).collect::<String>();
        write!(f, "{}", s)
    }
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Expression) -> Self {
        LetStatement { token, name, value }
    }
}

impl LetStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name.value, self.value)
    }
}

pub struct Identifier {
    token: Token,
    value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Identifier { token, value }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct ReturnStatement {
    token: Token,
    value: Expression,
}

impl ReturnStatement {
    pub fn new(token: Token, value: Expression) -> Self {
        ReturnStatement { token, value }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.value)
    }
}

pub struct ExpressionStatement {
    token: Token,
    expression: Expression,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> Self {
        ExpressionStatement { token, expression }
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

pub struct BlockStatement {
    token: Token,
    statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        BlockStatement { token, statements }
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.statements.iter().map(|s| s.to_string()).collect::<String>())
    }
}

pub struct Boolean {
    token: Token,
    value: bool,
}

impl Boolean {
    pub fn new(token: Token, value: bool) -> Self {
        Boolean { token, value }
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct IntegerLiteral {
    token: Token,
    value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        IntegerLiteral { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct PrefixExpression {
    token: Token,
    operator: String,
    right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<Expression>) -> Self {
        PrefixExpression { token, operator, right }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

pub struct InfixExpression {
    token: Token,
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(token: Token, operator: String, left: Box<Expression>, right: Box<Expression>) -> Self {
        InfixExpression { token, operator, left, right }
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(token: Token, condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Self {
        IfExpression { token, condition, consequence, alternative }
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}{}", self.condition, self.consequence, match &self.alternative {
            Some(val) => " else ".to_string() + &val.to_string(),
            None => "".to_string(),
        })
    }
}

pub struct FunctionLiteral {
    token: Token,
    parameters: Vec<Identifier>,
    body: BlockStatement,
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Identifier>, body: BlockStatement) -> Self {
        FunctionLiteral { token, parameters, body }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn ({}) {}", self.parameters.iter().map(|s| s.to_string()).collect::<String>(), self.body)
    }
}

pub struct CallExpression {
    token: Token,
    function: Box<Expression>,
    arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(token: Token, function: Box<Expression>, arguments: Vec<Expression>) -> Self {
        CallExpression { token, function, arguments }
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.function, self.arguments.iter().map(|s| s.to_string()).collect::<String>())
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn expression() {
        let input = "let var = exp;";
        let program = Program{ statements: vec![
            Statement::LetStatement(LetStatement { 
                token: Token::Keyword(Keyword::Let),
                name: Identifier { token: Token::Identifier("var".to_string()), value: "var".to_string() },
                value: Expression::Identifier(Identifier { token: Token::Identifier("exp".to_string()), value: "exp".to_string() }),
            })
        ] };
        
        assert_eq!(
            input.to_string(),
            program.to_string(),
        )
    }
}
