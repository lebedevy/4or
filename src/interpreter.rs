use std::{
    fmt::Display,
    sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use crate::{
    environment::{Environment, EnvironmentError},
    interpreter::{
        function::{Fun, UserFn, native::PrintFn},
        types::{ReferenceTypes, Types},
    },
    parser::{Expression, Literal, Statement},
    token::{Token, TokenType},
};

pub(super) mod function;
pub(crate) mod types;

struct EnvironmentWrapper {
    environment: Option<Arc<RwLock<Environment>>>,
}

impl EnvironmentWrapper {
    fn new() -> Self {
        let env = Arc::new(RwLock::new(Environment::new()));

        Self {
            environment: Some(env),
        }
    }

    fn set_scope(&mut self, scope: Arc<RwLock<Environment>>) {
        self.environment = Some(scope);
    }

    pub fn define(&mut self, name: &str, value: Types) -> Result<(), InterpreterError> {
        match &mut self.environment {
            Some(scope) => Ok(scope.write()?.define(name, value)?),
            None => {
                return Err(InterpreterError::EnvironmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }

    fn assign(&mut self, name: &str, value: Types) -> Result<(), InterpreterError> {
        match &mut self.environment {
            Some(scope) => Ok(scope.write()?.assign(name, value)?),
            None => {
                return Err(InterpreterError::EnvironmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }

    fn get(&self, name: &str) -> Result<Types, InterpreterError> {
        match &self.environment {
            Some(scope) => Ok(scope.read()?.get(name)?),
            None => {
                return Err(InterpreterError::EnvironmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }

    fn get_closure_ref(&self) -> Result<Arc<RwLock<Environment>>, InterpreterError> {
        match &self.environment {
            Some(closure) => Ok(closure.clone()),
            None => {
                return Err(InterpreterError::EnvironmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }
}

pub struct Interpreter {
    environment: EnvironmentWrapper,
}

enum Return {
    Expression(Types),
    Return(Types),
    None,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut environment = EnvironmentWrapper::new();

        let print_fn = PrintFn::new(environment.get_closure_ref().unwrap());

        environment
            .define(
                &print_fn.get_name().to_string(),
                Types::Reference(ReferenceTypes::Function(Arc::new(print_fn))),
            )
            .unwrap();

        Self { environment }
    }

    pub(super) fn run(&mut self, statements: Vec<Statement>) -> Result<(), InterpreterError> {
        for statement in statements {
            self.execute(&statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> Result<Return, InterpreterError> {
        match statement {
            Statement::Expression(expression) => {
                return Ok(Return::Expression(self.evaluate(&expression)?));
            }
            Statement::Variable(token, expression) => {
                let value = match expression {
                    Some(expression) => self.evaluate(&expression)?,
                    None => Types::Primitive(Literal::Nil),
                };

                let identifier = match &token.token_type {
                    TokenType::Identifier(iden) => iden,
                    _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
                };

                self.environment.define(identifier, value)?;
            }
            Statement::Block(block) => {
                let closure = self.environment.get_closure_ref()?;
                self.environment.set_scope(Arc::new(RwLock::new(
                    Environment::create_with_closure(closure.clone()),
                )));

                for statement in &block.statements {
                    if let Return::Return(r_val) = self.execute(statement)? {
                        self.environment.set_scope(closure);
                        return Ok(Return::Return(r_val));
                    }
                }

                self.environment.set_scope(closure);
            }
            Statement::If(expression, statement, else_statement) => {
                let val = match self.evaluate(&expression)? {
                    Types::Primitive(Literal::Bool(bool)) => bool,
                    val => {
                        return Err(InterpreterError::ExpectedBool(val));
                    }
                };

                let r_val = if val {
                    self.execute(statement)?
                } else if let Some(statement) = else_statement {
                    self.execute(statement)?
                } else {
                    Return::None
                };

                if let Return::Return(r_val) = r_val {
                    return Ok(Return::Return(r_val));
                }
            }
            Statement::While(expression, statement) => loop {
                match self.evaluate(&expression)? {
                    Types::Primitive(Literal::Bool(condition)) => {
                        if !condition {
                            break;
                        }
                    }
                    condition => return Err(InterpreterError::ExpectedBool(condition)),
                }

                if let Return::Return(r_val) = self.execute(statement)? {
                    return Ok(Return::Return(r_val));
                }
            },
            Statement::Function(name, params, body) => {
                // fn declaration
                let name = match &name.token_type {
                    TokenType::Identifier(name) => name,
                    _ => return Err(InterpreterError::InvalidVariableIdentifier(name.clone())),
                };

                let params = params.iter().map(|x| x.clone()).collect();

                self.environment.define(
                    name.as_str(),
                    Types::Reference(ReferenceTypes::Function(Arc::new(UserFn::new(
                        name.as_str(),
                        params,
                        body.clone(),
                        self.environment.get_closure_ref()?,
                    )))),
                )?;
            }
            Statement::Return(_token, statement) => {
                return Ok(Return::Return(match self.execute(statement)? {
                    Return::Expression(val) | Return::Return(val) => val,
                    Return::None => Types::Primitive(Literal::Nil),
                }));
            }
        };

        Ok(Return::None)
    }
}

#[derive(Debug)]
pub(super) enum InterpreterError {
    InvalidUnary(Token),
    InvalidBinary(Token),
    InvalidLogicalOperator(Token),
    InvalidVariableIdentifier(Token),
    UndefinedVariable(String),
    ExpectedBool(Types),
    NotAFunction(Types),
    InvalidFunctionBody(String),
    InvalidFunctionCall(String),
    EnvironmentError(String),
}

impl From<PoisonError<RwLockReadGuard<'_, Environment>>> for InterpreterError {
    fn from(value: PoisonError<RwLockReadGuard<'_, Environment>>) -> Self {
        InterpreterError::EnvironmentError(value.to_string())
    }
}

impl From<PoisonError<RwLockWriteGuard<'_, Environment>>> for InterpreterError {
    fn from(value: PoisonError<RwLockWriteGuard<'_, Environment>>) -> Self {
        InterpreterError::EnvironmentError(value.to_string())
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::InvalidUnary(token) => {
                write!(f, "Interpreter error: Invalid unary - {}", token)?;
            }
            InterpreterError::InvalidBinary(token) => {
                write!(f, "Interpreter error: Invalid binary - {}", token)?;
            }
            InterpreterError::InvalidVariableIdentifier(token) => {
                write!(
                    f,
                    "Interpreter error: Invalid variable identifier - {}",
                    token
                )?;
            }
            InterpreterError::UndefinedVariable(name) => {
                write!(f, "Interpreter error: Undefined variable - {}", name)?;
            }
            InterpreterError::ExpectedBool(literal) => {
                write!(f, "Interpreter error: Expected bool, got '{}'", literal)?
            }
            InterpreterError::InvalidLogicalOperator(token) => write!(
                f,
                "Interpreter error: Expected 'and' or 'or', got '{}'",
                token
            )?,
            InterpreterError::NotAFunction(types) => {
                write!(f, "Attempting to call a non-function object {}", types)?
            }
            InterpreterError::InvalidFunctionBody(name) => {
                write!(f, "Invalid function body for function '{}'", name)?
            }
            InterpreterError::InvalidFunctionCall(err) => write!(f, "{}", err)?,
            InterpreterError::EnvironmentError(err) => write!(f, "{}", err)?,
        };

        Ok(())
    }
}

impl Interpreter {
    fn evaluate(&mut self, expression: &Expression) -> Result<Types, InterpreterError> {
        match expression {
            Expression::Binary(left, token, right) => self.binary(left, token, right),
            Expression::Grouping(expression) => self.grouping(expression),
            // TODO: consider implications of cloning
            Expression::Literal(literal) => Ok(Types::Primitive(literal.clone())),
            Expression::Unary(token, expression) => self.unary(token, expression),
            Expression::Variable(token) => self.variable(token),
            Expression::Assignment(token, expression) => self.assign(token, expression),
            Expression::Logical(left, operator, right) => self.logical(left, operator, right),
            Expression::Call(expression, args) => self.call_function(expression, args),
        }
    }

    fn call_function(
        &mut self,
        expression: &Expression,
        args: &Vec<Expression>,
    ) -> Result<Types, InterpreterError> {
        let callee = self.evaluate(expression)?;

        let callee = match callee {
            Types::Reference(ReferenceTypes::Function(func)) => func,
            _ => return Err(InterpreterError::NotAFunction(callee)),
        };

        let params = callee.get_params();

        if params.len() != args.len() {
            return Err(InterpreterError::InvalidFunctionCall(
                "Invalid arg count".to_string(),
            ));
        }

        let fn_scope = Arc::new(RwLock::new(Environment::create_with_closure(
            callee.get_closure(),
        )));

        for (param, arg) in params.iter().zip(args) {
            let param = match &param.token_type {
                TokenType::Identifier(iden) => iden,
                _ => {
                    return Err(InterpreterError::InvalidFunctionCall(
                        "invalid param defintion".to_string(),
                    ));
                }
            };
            let val = self.evaluate(arg)?;
            fn_scope.write()?.define(param.as_str(), val)?;
        }

        let prev_scope = self.environment.get_closure_ref()?;

        self.environment.set_scope(fn_scope);

        callee.call(self)?;

        self.environment.set_scope(prev_scope);

        Ok(Types::Primitive(Literal::Nil))
    }

    fn logical(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> Result<Types, InterpreterError> {
        let left = match self.evaluate(left)? {
            Types::Primitive(Literal::Bool(val)) => val,
            left => return Err(InterpreterError::ExpectedBool(left)),
        };

        // if and and false, return false; if or and true, return true
        match &operator.token_type {
            TokenType::And => {
                if !left {
                    return Ok(Types::Primitive(Literal::Bool(left)));
                }
            }
            TokenType::Or => {
                if left {
                    return Ok(Types::Primitive(Literal::Bool(left)));
                }
            }
            _ => return Err(InterpreterError::InvalidLogicalOperator(operator.clone())),
        };

        Ok(self.evaluate(right)?)
    }

    fn variable(&mut self, token: &Token) -> Result<Types, InterpreterError> {
        match &token.token_type {
            TokenType::Identifier(iden) => Ok(self.environment.get(iden)?),
            _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
        }
    }

    fn assign(
        &mut self,
        token: &Token,
        expression: &Expression,
    ) -> Result<Types, InterpreterError> {
        let value = self.evaluate(expression)?;

        match &token.token_type {
            TokenType::Identifier(iden) => self.environment.assign(iden, value.clone())?,
            _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
        };

        Ok(value)
    }

    fn binary(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> Result<Types, InterpreterError> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        let (left, right) = match (left, right) {
            (Types::Primitive(left), Types::Primitive(right)) => (left, right),
            _ => return Err(InterpreterError::InvalidBinary(operator.clone())),
        };

        // TODO: int overflow
        let res = match (&operator.token_type, left, right) {
            // Numeric
            (TokenType::Minus, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left - right)
            }
            (TokenType::Plus, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left + right)
            }
            (TokenType::Slash, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left / right)
            }
            (TokenType::Star, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left * right)
            }
            // Strings
            (TokenType::Plus, Literal::String(left), Literal::String(right)) => {
                Literal::String(left + &right)
            }
            // Comparison
            (TokenType::Greater, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left > right)
            }
            (TokenType::GreaterEqual, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left >= right)
            }
            (TokenType::Less, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left < right)
            }
            (TokenType::LessEqual, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left <= right)
            }
            // Equality
            // TODO: Consider the implications; currently just outsourcing the comparison to rust
            (TokenType::EqualEqual, right, left) => Literal::Bool(right == left),
            (TokenType::BangEqual, right, left) => Literal::Bool(right != left),
            _val => return Err(InterpreterError::InvalidBinary(operator.clone())),
        };

        Ok(Types::Primitive(res))
    }

    fn unary(&mut self, token: &Token, expression: &Expression) -> Result<Types, InterpreterError> {
        let right = self.evaluate(expression)?;

        let res = match (&token.token_type, right) {
            (TokenType::Minus, Types::Primitive(Literal::Number(num))) => Literal::Number(-num),
            // We are not allowing the concept of "truthiness"; give me bool or get bonked
            (TokenType::Bang, Types::Primitive(Literal::Bool(val))) => Literal::Bool(!val),
            _val => return Err(InterpreterError::InvalidUnary(token.clone())),
        };

        Ok(Types::Primitive(res))
    }

    fn grouping(&mut self, expression: &Expression) -> Result<Types, InterpreterError> {
        self.evaluate(expression)
    }

    fn get_variable(&mut self, name: &str) -> Result<Types, InterpreterError> {
        Ok(self.environment.get(name)?)
    }
}

impl From<EnvironmentError> for InterpreterError {
    fn from(value: EnvironmentError) -> Self {
        match value {
            EnvironmentError::UndefinedVariable(value) => {
                InterpreterError::UndefinedVariable(value)
            }
            EnvironmentError::ReferencePoison(err) => InterpreterError::EnvironmentError(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        iter::Peekable,
        result::{self, IntoIter},
        sync::{Arc, RwLock},
    };

    use leptos::prelude::RwSignal;

    use crate::{
        Parser, ParserError, ProgramError,
        environment::Environment,
        interpreter::{
            function::Fun,
            types::{ReferenceTypes, Types},
        },
        parser::{Expression, Literal, Statement},
        scanner::Scanner,
        token::{Token, TokenType},
    };

    use super::{Interpreter, InterpreterError};

    fn get_num_exp(num: f64) -> Expression {
        Expression::Literal(Literal::Number(num))
    }

    fn get_token(token_type: TokenType) -> Token {
        Token {
            token_type,
            index: 0,
        }
    }

    fn get_program_from_string(string: &str) -> Result<Vec<Statement>, ParserError> {
        let mut scanner = Scanner::new(string.to_string());
        let tokens = scanner.scan_tokens();
        Ok(Parser::parse(tokens.into_iter().peekable())?)
    }

    #[test]
    fn binary() -> Result<(), InterpreterError> {
        let operations = vec![
            (4.0, TokenType::Minus, 2.0, Literal::Number(2.0)),
            (2.0, TokenType::Plus, 2.0, Literal::Number(4.0)),
            (6.0, TokenType::Slash, 3.0, Literal::Number(2.0)),
            (2.0, TokenType::Star, 3.0, Literal::Number(6.0)),
            (4.0, TokenType::Greater, 2.0, Literal::Bool(true)),
            (2.0, TokenType::GreaterEqual, 2.0, Literal::Bool(true)),
            (4.0, TokenType::Less, 2.0, Literal::Bool(false)),
            (2.0, TokenType::LessEqual, 2.0, Literal::Bool(true)),
        ];

        for (left, token, right, expected) in operations {
            let result = Interpreter::new().binary(
                &get_num_exp(left),
                &get_token(token),
                &get_num_exp(right),
            )?;

            let result = match result {
                Types::Primitive(literal) => literal,
                Types::Reference(reference_types) => {
                    panic!("Unexpected reference '{}'", reference_types)
                }
            };

            assert!(
                result == expected,
                "Did not return expected result; expected '{}' got '{}'",
                result,
                expected
            );
        }

        Ok(())
    }

    // Closure tests
    // TODO: there are arguably integration tests, but they would require me to publically expose way more
    // than I would like to; so leave them here for now

    #[test]
    fn fn_call_resets_closure() -> Result<(), ProgramError> {
        let program = get_program_from_string("let a = 1; fn test() { print(a); } test();")?;

        let mut interpreter = Interpreter::new();

        let env = interpreter.environment.get_closure_ref()?;

        interpreter.run(program)?;

        let env_after = interpreter.environment.get_closure_ref()?;

        assert!(
            std::ptr::eq(&*env.read().unwrap(), &*env_after.read().unwrap()),
            "Scope not reset after function call"
        );

        Ok(())
    }

    struct StoreFn {
        params: Vec<Token>,
        results: RwLock<Vec<f64>>,
        closure: Arc<RwLock<Environment>>,
    }

    impl StoreFn {
        fn new(closure: Arc<RwLock<Environment>>) -> Self {
            Self {
                params: vec![Token::new(TokenType::Identifier("val".to_owned()), 0)],
                results: RwLock::new(vec![]),
                closure,
            }
        }
    }

    impl Fun for StoreFn {
        fn call(&self, interpreter: &mut Interpreter) -> Result<Types, InterpreterError> {
            let iden = match &self.params.first().unwrap().token_type {
                TokenType::Identifier(iden) => iden,
                _ => {
                    return Err(InterpreterError::InvalidFunctionBody(
                        "something something".to_owned(),
                    ));
                }
            };

            let mut arr = self.results.write().unwrap();

            arr.push(match interpreter.get_variable(&iden)? {
                Types::Primitive(Literal::Number(num)) => num,
                _ => {
                    return Err(InterpreterError::InvalidFunctionCall(
                        "Not a number".to_owned(),
                    ));
                }
            });

            Ok(Types::Primitive(Literal::Nil))
        }

        fn get_name(&self) -> &str {
            "store"
        }

        fn get_params(&self) -> &Vec<Token> {
            &self.params
        }

        fn get_closure(&self) -> std::sync::Arc<std::sync::RwLock<Environment>> {
            self.closure.clone()
        }
    }

    #[test]
    fn binds_to_fn_closure() -> Result<(), ProgramError> {
        // TODO: This is probably an integration test
        let program = get_program_from_string(
            "
let a = 1;
fn test() { store(a); }
{ let a = 2; test(); store(a); }
        ",
        )?;

        let mut interpreter = Interpreter::new();
        let store_fn = Arc::new(StoreFn::new(interpreter.environment.get_closure_ref()?));
        interpreter.environment.define(
            store_fn.get_name(),
            Types::Reference(ReferenceTypes::Function(store_fn.clone())),
        )?;

        interpreter.run(program)?;

        let actual = store_fn.results.read().unwrap();
        let expected = vec![1_f64, 2_f64];

        assert!(
            itertools::equal(expected.iter(), actual.iter()),
            "Expected '{:?}' got '{:?}'",
            expected,
            actual
        );

        Ok(())
    }

    #[test]
    fn call_binds_args_to_call_site() -> Result<(), ProgramError> {
        let program = get_program_from_string(
            "
let a = 1;
fn test() { store(a); }
{ let a = 2; store(a); test(); }
        ",
        )?;

        let mut interpreter = Interpreter::new();
        let store_fn = Arc::new(StoreFn::new(interpreter.environment.get_closure_ref()?));
        interpreter.environment.define(
            store_fn.get_name(),
            Types::Reference(ReferenceTypes::Function(store_fn.clone())),
        )?;

        interpreter.run(program)?;

        let actual = store_fn.results.read().unwrap();
        let expected = vec![2_f64, 1_f64];

        assert!(
            itertools::equal(expected.iter(), actual.iter()),
            "Expected '{:?}' got '{:?}'",
            expected,
            actual
        );

        Ok(())
    }

    #[test]
    fn closure_resolves_to_same_binding() -> Result<(), ProgramError> {
        let program = get_program_from_string(
            "
let a = 1;
{
  fn test() {
    store(a);
  }

  test();
  let a = 2;
  test();
}
        ",
        )?;

        let mut interpreter = Interpreter::new();
        let store_fn = Arc::new(StoreFn::new(interpreter.environment.get_closure_ref()?));
        interpreter.environment.define(
            store_fn.get_name(),
            Types::Reference(ReferenceTypes::Function(store_fn.clone())),
        )?;

        interpreter.run(program)?;

        let actual = store_fn.results.read().unwrap();
        let expected = vec![1_f64, 1_f64];

        assert!(
            itertools::equal(expected.iter(), actual.iter()),
            "Expected '{:?}' got '{:?}'",
            expected,
            actual
        );

        Ok(())
    }
}
