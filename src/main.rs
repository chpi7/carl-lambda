#![allow(dead_code)]

use std::io::{self, Write};

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

impl Term {
    fn println(&self) {
        self.print();
        println!("");
    }

    fn print(&self) {
        match self {
            Term::Definition(name, term) => {
                print!("def {:} = ", name);
                term.print();
                println!(";");
            }
            Term::Abstraction(var_name, subterm) => {
                print!("λ{:}.", var_name);
                subterm.print();
            }
            Term::Application(term1, term2) => {
                print!("(");
                term1.print();
                print!(" , ");
                term2.print();
                print!(")");
            }
            Term::BoundVariable(depth) => {
                print!("{:}", depth);
            }
            Term::FreeVariable(name) => {
                print!("{:}", name);
            }
            _ => todo!(),
        }
    }
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
    pub fn empty<'a>() -> ParserState<'a> {
        ParserState {
            remaining_tokens: &[],
            abstraction_depth: 0,
            bound_variables: HashMap::new(),
            named_terms: HashMap::new(),
        }
    }

    pub fn new<'a>(tokens: &'a [Token]) -> ParserState<'a> {
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
    fn current(&self) -> Option<&Token> {
        self.remaining_tokens.first()
    }

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

fn parse_program(state: &mut ParserState) -> Result<Option<Term>, ParseError> {
    // println!("parse program {:?}", state.remaining_tokens);

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
                }
                _ => break,
            },
            _ => panic!("This should not happen!"),
        }
    }

    if !state.remaining_tokens.is_empty() {
        Ok(Some(parse_term(state)?))
    } else {
        Ok(None)
    }
}

/// Parse an identifier into whatever it represents.
fn parse_atom(state: &mut ParserState) -> Result<Term, ParseError> {
    // println!("parse atom {:?}", state.remaining_tokens);
    let name = state.consume_ident()?.to_string();
    if let Some(relative_depth) = state.relative_depth(&name) {
        Ok(Term::BoundVariable(relative_depth))
    } else if let Some(named_term) = state.named_terms.get(&name) {
        Ok(named_term.clone()) // Just copy paste the named term here.
    } else {
        Ok(Term::FreeVariable(name.to_string()))
    }
}

fn can_start_term(t: &Token) -> bool {
    match t {
        Token::OpenParen => true,
        Token::Identifier(_) => true,
        Token::Slash => true,
        _ => false,
    }
}

/// Parses a "basic term". Does not handle applications because an application is just two other
/// kinds of terms combined.
fn parse_term_partial(state: &mut ParserState) -> Result<Term, ParseError> {
    let parse_error = Err(ParseError("unexpected token in parse_term".to_string()));
    match state.remaining_tokens {
        [head, _tail @ ..] => match head {
            Token::Slash => parse_abstraction(state),
            Token::Identifier(_) => parse_atom(state),
            Token::OpenParen => {
                state.advance();
                let result = parse_term(state);
                state.consume(Token::CloseParen, "Expected ) after term enclosed in ()")?;
                result
            }

            /* This is not used atm.
            Token::Number(text) => {
            let num = u64::from_str_radix(text, 10)
            .map_err(|_| ParseError("Number could not be parsed to u64".to_string()))?;
                state.advance();
                Ok(Term::Constant(Value::Number(num)))
                } */
            // A definition is not a term for the moment, maybe we allow that later to make it more
            // programming lanugage.
            Token::Def => return parse_error,
            _ => return parse_error,
        },
        [] => panic!("This should not happen!"),
    }
}

/// Parse a term beginning at the start of tokens.
fn parse_term(state: &mut ParserState) -> Result<Term, ParseError> {
    // println!("parse term {:?}", state.remaining_tokens);

    // Spool as many "basic" terms into a chain of left-associative applications as possible.
    let mut lhs = parse_term_partial(state)?;
    while let Some(token) = state.current() {
        if !can_start_term(token) {
            break;
        }
        let rhs = parse_term_partial(state)?;
        lhs = Term::Application(Box::new(lhs), Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_definition(_state: &mut ParserState) -> Result<Term, ParseError> {
    todo!();
}

fn parse_abstraction(state: &mut ParserState) -> Result<Term, ParseError> {
    // println!("parse abstraction {:?}", state.remaining_tokens);

    state.consume(Token::Slash, "Expected / at beginning of abstraction")?;
    let name = state.consume_ident()?.to_string();
    state.consume(Token::Dot, "Expected . after identifier in abstraction")?;

    state.push_abstraction_depth(&name);

    let term = parse_term(state)?;

    state.pop_abstraction_depth(&name);

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

#[derive(Debug, PartialEq)]
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

/// Implements normal order reduction. (Always reduce the left-most β-redex).
fn reduce(t: Term) -> Term {
    let mut current = t;

    // Always contract left most b-redex:
    loop {
        match current {
            Term::Application(abs, st2) if is_reducible(&current) => {
                match *abs {
                    Term::Abstraction(_, st1) => {
                        /*
                        print!("reduce: ");
                        st1.print();
                        print!(" <- ");
                        st2.println();
                        */
                        current = replace_bound_vars_internal(*st1, &st2, 0);
                        // print!("current = ");
                        // current.println();
                    }
                    _ => panic!("should not happen we checked before"),
                }
            }
            _ => break,
        };
    }

    // println!("loop done");

    // current is no longer reducible
    current = match current {
        Term::Application(st1, st2) => {
            let lhs = reduce(*st1);
            let rhs = reduce(*st2);
            Term::Application(Box::new(lhs), Box::new(rhs))
        }
        Term::Abstraction(n, st) => Term::Abstraction(n, Box::new(reduce(*st))),
        _ => current,
    };

    /*
    print!("current end: ");
    current.println();
    */

    if is_reducible(&current) {
        reduce(current)
    } else {
        current
    }
}

/// Does beta-reduction: For (/x . s) t, replace all occurrences of x in s with t.
fn replace_bound_vars_internal(t1: Term, t2: &Term, depth: i32) -> Term {
    match t1 {
        Term::FreeVariable(_) => t1,
        Term::BoundVariable(d) if d == depth => {
            // print!("substituting {:} for ", d);
            // t2.println();
            t2.clone()
        }
        Term::BoundVariable(_) => t1,
        Term::Abstraction(n, st) => {
            Term::Abstraction(n, Box::new(replace_bound_vars_internal(*st, t2, depth + 1)))
        }
        Term::Application(st1, st2) => Term::Application(
            Box::new(replace_bound_vars_internal(*st1, t2, depth)),
            Box::new(replace_bound_vars_internal(*st2, t2, depth)),
        ),
        _ => todo!(),
    }
}

fn evaluate_src(source: &str) -> Result<Term, EvalError> {
    let tokens = tokenize(source);
    let mut parser_state = ParserState::new(&tokens);
    let term_opt = parse_program(&mut parser_state).map_err(|e| EvalError(e.0))?;

    /*
    println!("Named terms: {:?}", parser_state.named_terms);
    for (n, t) in parser_state.named_terms {
        print!("{:} = ", n);
        t.println();
    }
    println!("Result term: {:?}", term);
    term.println();
    */

    if let Some(term) = term_opt {
        Ok(reduce(term))
    } else {
        Ok(Term::FreeVariable("<empty>".to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reduce_1() {
        let result = evaluate_src("(/x.x (/y. x y y) x) (/z. /w. z)");
        let expected = evaluate_src("/y.y");
        assert!(
            Ok(Term::Abstraction(
                "y".to_string(),
                Box::new(Term::BoundVariable(0))
            )) == result
        );
        assert!(result == expected);
    }

    #[test]
    fn control_flow_if_true_false() {
        let source = "
            def true = /x . /y . x;
            def false = /x . /y . y;
            def if = /z . /x . /y . z x y;
            if true (if false a z) y
        ";
        let result = evaluate_src(source);
        let expected = evaluate_src("z");
        assert!(result == expected);
    }
}

fn main() -> io::Result<()> {
    /*
        def fst = /p . p true;
        def snd = /p . p false;
        def pair = /x . /y . /z . z y x;
    */

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut named_terms: HashMap<String, Term> = HashMap::new();

    let stdlib = "
        def true = /x . /y . x;
        def false = /x . /y . y;
        def if = /z . /x . /y . z x y;

        def fst = /p . p true;
        def snd = /p . p false;
        def pair = /x . /y . /z . z x y;
    ";

    //      /p.p true
    //      /z.z a b
    //      (/z.z a b) true
    //

    loop {
        print!("> ");
        io::stdout().flush().expect("flush failed");
        buffer.clear();
        let line = stdin.read_line(&mut buffer)?;
        if line == 0 {
            continue;
        }

        if buffer.starts_with("#!load_stdlib") {
            let tokens = tokenize(stdlib);
            let mut parser_state = ParserState::new(&tokens);
            let _ = parse_program(&mut parser_state);
            named_terms.extend(parser_state.named_terms);
            continue;
        }

        let tokens = tokenize(&buffer);
        // println!("{:?}", tokens);

        let mut parser_state = ParserState::new(&tokens);
        parser_state.named_terms = named_terms.clone();

        let parse_result = parse_program(&mut parser_state);

        match parse_result {
            Ok(term_opt) => {
                // Store "def"ined term for later reuse
                named_terms.extend(parser_state.named_terms);
                // Then evaluate the main term and print the result:
                if let Some(term) = term_opt {
                    reduce(term).println();
                }
            }
            Err(parse_error) => println!("Error: {:}", parse_error.0),
        }
    }
}
