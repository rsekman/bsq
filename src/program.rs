use std::cell::RefCell;
use std::rc::Rc;

use kuchiki::traits::TendrilSink;

use crate::ast::{program, AST};
use crate::error::Error;
use crate::eval::{Eval, State};
use crate::val::{Val, ValResult};

pub struct Program {
    ast: AST,
}

impl Program {
    pub fn run<I>(&self, input: &mut I) -> ValResult
    where
        I: std::io::Read,
    {
        let document = kuchiki::parse_html()
            .from_utf8()
            .read_from(input)
            .map_err(|e| e.to_string())?;
        let ret = {
            let state = Rc::new(RefCell::new(State::default()));
            self.ast.eval(Val::Node(document).into(), state)
        };
        ret
    }
}

pub fn compile(input: &str) -> Result<Program, Error> {
    let (_, ast) = program(input).map_err(|e| e.to_string())?;
    Ok(Program { ast })
}
