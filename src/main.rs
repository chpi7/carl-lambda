#![allow(dead_code)]

use std::collections::{HashMap, HashSet};

/*
 * Grammar:
 * t = CONSTANT | VARIABLE | (t1 t2) | (/x . t)
 */

#[derive(Debug, PartialEq, Clone, Copy)]
enum Token<'a> {
    OpenParen,
    CloseParen,
    Equals,
    Slash,
    Dot,
    Def,
    Semi,
    Identifier(&'a str),
    Number(&'a str),
    EndOfFile,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Number(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Term {
    Constant(Value),
    FreeVariable(String),
    BoundVariable(i32),
    Application(Box<Term>, Box<Term>),
    Abstraction(String, Box<Term>),

    /// This is just for convenience to have reusable names like true and false and so on.
    Definition(String, Box<Term>),
}

fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

fn first_char_is(s: &str, check: fn(char) -> bool) -> bool {
    s.chars().next().map_or(false, check)
}

fn next_token(source: &str) -> (Token, usize) {
    let skipped_ws = source.trim_start();

    if skipped_ws.is_empty() {
        return (Token::EndOfFile, 0);
    }

    // println!("get next token from: {}", source);

    let first_char = skipped_ws
        .chars()
        .next()
        .expect("Expected at least one char.");

    if skipped_ws.starts_with('(') {
        (Token::OpenParen, 1)
    } else if skipped_ws.starts_with(')') {
        (Token::CloseParen, 1)
    } else if skipped_ws.starts_with('=') {
        (Token::Equals, 1)
    } else if skipped_ws.starts_with(';') {
        (Token::Semi, 1)
    } else if skipped_ws.starts_with('/') {
        (Token::Slash, 1)
    } else if skipped_ws.starts_with('.') {
        (Token::Dot, 1)
    } else if skipped_ws.starts_with("def") {
        (Token::Def, 3)
    } else if first_char.is_digit(10) {
        let mut len = 1;
        while first_char_is(&source[len..], |c| c.is_digit(10)) {
            len += 1;
        }
        (Token::Number(&source[..len]), len)
    } else if first_char.is_alphabetic() {
        let mut len = 1;
        while first_char_is(&source[len..], |c| c.is_alphanumeric()) {
            len += 1;
        }
        (Token::Identifier(&source[..len]), len)
    } else {
        (Token::Error, source.len())
    }
}

fn tokenize(source: &str) -> Vec<Token> {
    let mut result = vec![];
    let mut remainder = source.trim_start();

    while !remainder.is_empty() {
        match next_token(remainder) {
            (token, length) => {
                assert!(length > 0, "infinite loop if length is 0");
                result.push(token);
                remainder = remainder.split_at(length).1.trim_start();
            }
        }
    }

    if source.is_empty() {
        result.push(Token::EndOfFile);
    }

    result
}

struct ParserState<'a> {
    remaining_tokens: &'a [Token<'a>],
    abstraction_depth: i32,
    /* variable name -> abstraction depth. */
    bound_variables: HashMap<String, Vec<i32>>,
    named_terms: HashMap<String, Term>,
}

impl ParserState<'_> {
    pub fn new<'a>(tokens: &'a[Token]) -> ParserState<'a>
    {
        ParserState {
            remaining_tokens: tokens,
            abstraction_depth: 0,
            bound_variables: HashMap::new(),
            named_terms: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct ParseError(String);

impl ParserState<'_> {
    /// Peek at the next token after the current one.
    fn peek(&self) -> Option<&Token> {
        if self.remaining_tokens.len() > 2 {
            Some(&self.remaining_tokens[1])
        } else {
            None
        }
    }

    /// Advance the token stream by one token.
    fn advance(&mut self) {
        // println!("advance over {:?}", self.remaining_tokens[0]);
        self.remaining_tokens = &self.remaining_tokens[1..];
    }

    /// Check if the first token matches and call advance if that's the case.
    fn consume(&mut self, sample: Token, message: &str) -> Result<Token, ParseError> {
        match self.remaining_tokens {
            [tv, _rest @ ..] if variant_eq(tv, &sample) => {
                let t = tv.clone();
                self.advance();
                Ok(t)
            }
            [tok, _rest @ ..] => {
                println!("found unexpected token: {:?}", tok);
                Err(ParseError(message.to_string()))
            }
            _ => Err(ParseError(message.to_string())),
        }
    }

    /// Consume an identifier and return the name of it.
    fn consume_ident(&mut self) -> Result<&str, ParseError> {
        match self.consume(Token::Identifier(""), "Expected identifier") {
            Ok(Token::Identifier(s)) => Ok(s),
            Err(err) => Err(err),
            _ => panic!("Consume broken!!"),
        }
    }

    fn push_abstraction_depth(&mut self, name: &str) {
        self.abstraction_depth += 1;
        if !self.bound_variables.contains_key(name) {
            self.bound_variables.insert(name.to_string(), Vec::new());
        }
        self.bound_variables
            .get_mut(name)
            .expect("entry has to exist")
            .push(self.abstraction_depth);
    }

    fn pop_abstraction_depth(&mut self, name: &str) {
        self.bound_variables
            .get_mut(name)
            .expect("entry has to exist")
            .pop();
        self.abstraction_depth -= 1;
    }

    fn relative_depth(&self, name: &str) -> Option<i32> {
        if let Some(v) = self.bound_variables.get(name) {
            if let Some(val) = v.first() {
                return Some(self.abstraction_depth - val);
            }
        }
        None
    }
}

fn parse_program(state: &mut ParserState) -> Result<Term, ParseError> {
    println!("parse program {:?}", state.remaining_tokens);

    while !state.remaining_tokens.is_empty() {
        match state.remaining_tokens {
            [head, _tail @ ..] => match head {
                Token::Def => {
                    state.advance();
                    let name = state.consume_ident()?.to_string();
                    state.consume(Token::Equals, "Expected equals sign after def <name>")?;
                    let term = parse_term(state)?;
                    state.consume(Token::Semi, "Expected semicolon after def <name> = <term>")?;
                    // println!("storing named term: {:?}", term);
                    state.named_terms.insert(name, term);
                },
                _ => break,
            },
            _ => panic!("This should not happen!"),
        }
    }
    
    parse_term(state)
}

/// Parse a term beginning at the start of tokens
fn parse_term(state: &mut ParserState) -> Result<Term, ParseError> {
    println!("parse term {:?}", state.remaining_tokens);

    let parse_error = Err(ParseError("unexpected token in parse_term".to_string()));

    match state.remaining_tokens {
        [head, _tail @ ..] => match head {
            Token::Slash => panic!("should go through ( for now"),
            Token::Def => todo!(),
            Token::Identifier(name) => {
                state.advance();

                if let Some(relative_depth) = state.relative_depth(name) {
                    Ok(Term::BoundVariable(relative_depth))
                } else if let Some(named_term) = state.named_terms.get(*name) {
                    // Just copy paste the named term here.
                    Ok(named_term.clone())
                }
                else {
                    Ok(Term::FreeVariable(name.to_string()))
                }
            }
            Token::Number(text) => {
                let num = u64::from_str_radix(text, 10)
                    .map_err(|_| ParseError("Number could not be parsed to u64".to_string()))?;
                state.advance();
                Ok(Term::Constant(Value::Number(num)))
            }

            Token::OpenParen => match state.peek() {
                Some(Token::Slash) => parse_abstraction(state),
                Some(_) => parse_application(state),
                _ => Err(ParseError("no more tokens after opening paren".to_string())),
            },
            Token::CloseParen => parse_error,
            Token::Equals => parse_error,
            Token::Dot => parse_error,
            Token::Semi => parse_error,
            Token::EndOfFile => parse_error,
            Token::Error => parse_error,
        },
        [] => panic!("This should not happen!"),
    }
}

fn parse_definition(_state: &mut ParserState) -> Result<Term, ParseError> {
    todo!();
}

fn parse_abstraction(state: &mut ParserState) -> Result<Term, ParseError> {
    state.advance(); // consume (

    state.consume(Token::Slash, "Expected / at beginning of abstraction")?;
    let name = state.consume_ident()?.to_string();
    state.consume(Token::Dot, "Expected . after identifier in abstraction")?;

    state.push_abstraction_depth(&name);

    // lambda binds as far to the right as possible (as long as there is no closing paren)
    // BUT right now we have everything strictly enclosed in ( and ), so it should be trivial.
    let term = parse_term(state)?;

    state.pop_abstraction_depth(&name);

    state.consume(Token::CloseParen, "Abstraction should end with )")?;

    Ok(Term::Abstraction(name, Box::new(term)))
}

fn parse_application(state: &mut ParserState) -> Result<Term, ParseError> {
    // Application is just t2 applied to t1. No special syntax whatsoever.

    state.consume(Token::OpenParen, "Application should start with (")?;

    let t1 = parse_term(state)?;
    let t2 = parse_term(state)?;

    state.consume(Token::CloseParen, "Application should end with )")?;

    Ok(Term::Application(Box::new(t1), Box::new(t2)))
}

#[derive(Debug)]
struct EvalError(String);

fn fv(_term: &Term) -> HashSet<String> {
    HashSet::new()
}

/// Implement "s with t for x". Substitutes all !free! occurrence of x for t.
fn substitute(s: Term, t: &Term, x: &str) -> Term {
    match s {
        Term::FreeVariable(name) if name == x => t.clone(),
        Term::FreeVariable(_) => s,
        Term::BoundVariable(_) => s,
        Term::Application(t1, t2) => Term::Application(
            Box::new(substitute(*t1, t, x)),
            Box::new(substitute(*t2, t, x)),
        ),
        /* x is bound from here on -> no more free occurrences to substitute. */
        Term::Abstraction(ref var_name, _) if var_name == x => s,
        /* otherwise substitute in the subterm */
        Term::Abstraction(n, st) => Term::Abstraction(n, Box::new(substitute(*st, t, x))),
        _ => todo!(),
    }
}

fn is_abstraction(t: &Term) -> bool {
    match t {
        Term::Abstraction(_, _) => true,
        _ => false,
    }
}

fn is_reducible(t: &Term) -> bool {
    match t {
        Term::Application(st, _) => is_abstraction(st),
        _ => false,
    }
}

/// Does beta-reduction: For (/x . s) t, replace all occurrences of x in s with t.
fn reduce(t1: Term, t2: &Term) -> Term {
    replace_bound_vars_internal(t1, t2, -1)
}

/// Does beta-reduction: For (/x . s) t, replace all occurrences of x in s with t.
fn replace_bound_vars_internal(t1: Term, t2: &Term, depth: i32) -> Term {
    println!("replace {:?} <- {:?} at {:}", &t1, &t2, depth);
    match t1 {
        Term::FreeVariable(_) => t1,
        Term::BoundVariable(d) if d == depth => t2.clone(),
        Term::BoundVariable(_) => t1,
        Term::Abstraction(_, st) => replace_bound_vars_internal(*st, t2, depth + 1),
        Term::Application(st1, st2) => Term::Application(
            Box::new(replace_bound_vars_internal(*st1, t2, depth)),
            Box::new(replace_bound_vars_internal(*st2, t2, depth)),
        ),
        _ => todo!(),
    }
}

fn evaluate(term: Term) -> Term {
    println!("evaluate {:?}", term);
    match term {
        Term::Application(t1, t2) => {
            let tt1 = evaluate(*t1);
            let tt2 = evaluate(*t2);
            // Assume that when there is an application, it is reducible after
            // evaluating the subterms.
            let res = reduce(tt1, &tt2);

            println!("after replacement: {:?}", res);

            res
        }
        Term::Abstraction(n, s) => Term::Abstraction(n, Box::new(evaluate(*s))),
        _ => term,
    }
}

fn main() {
    let _tmp = "
        def true = /x . /y . x;
        def false = /x . /y . y;
        def if = /z . /x . /y . z x y;

        def fst = /p . p true;
        def snd = /p . p false;
        def pair = /x . /y . /z . z y x;
    ";

    let source = "
        def identity = (/x . x);
        ((identity identity) z)
    ";

    let tokens = tokenize(source);
    println!("tokens: {:?}", tokens);

    let mut parser_state = ParserState::new(&tokens);
    let term = parse_program(&mut parser_state).expect("parse error!!");
    println!("Result term: {:?}", term);
    println!("Named terms: {:?}", parser_state.named_terms);

    let result = evaluate(term);
    println!("result: {:?}", result);

    assert!(Term::FreeVariable("z".to_string()) == result);
}
