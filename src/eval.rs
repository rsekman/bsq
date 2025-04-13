use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Exp, Identifier, Term, AST};
use crate::builtins::{
    attribute, parent, select, select_all, ATTRIBUTE, PARENT, SELECT, SELECT_ALL,
};
use crate::error::type_error;
use crate::val::{Val, ValRef, ValResult};

// A function is something that takes zero or more evaluable expressions and returns something evaluatable
// We must allow unevaluated expressions as arguments to implement higher-order functions such as
// map, filter, and fold
type ArgList = Rc<Vec<Exp>>;
#[derive(Clone)]
struct Func {
    arity: usize,
    func: Rc<dyn Fn(ArgList) -> Box<dyn Eval>>,
}

pub(crate) struct State {
    funcs: HashMap<Identifier, Func>,
    vars: HashMap<Identifier, Val>,
}
type StateRef = Rc<RefCell<State>>;

impl Default for State {
    fn default() -> Self {
        let builtins = HashMap::<Identifier, Func>::from([
            (
                vec![ATTRIBUTE.to_owned()],
                Func {
                    arity: 1,
                    func: Rc::new(builtins::attribute),
                },
            ),
            (
                vec![PARENT.to_owned()],
                Func {
                    arity: 0,
                    func: Rc::new(builtins::parent),
                },
            ),
            (
                vec![SELECT.to_owned()],
                Func {
                    arity: 1,
                    func: Rc::new(builtins::select),
                },
            ),
            (
                vec![SELECT_ALL.to_owned()],
                Func {
                    arity: 1,
                    func: Rc::new(builtins::select_all),
                },
            ),
        ]);
        State {
            funcs: builtins,
            vars: HashMap::default(),
        }
    }
}

pub(crate) trait Eval {
    fn eval(&self, input: ValRef, state: StateRef) -> ValResult;
}

impl<'a, F> Eval for F
where
    F: Fn(ValRef, StateRef) -> ValResult,
{
    fn eval(&self, input: ValRef, state: StateRef) -> ValResult {
        self(input, state)
    }
}

impl Eval for AST {
    fn eval(&self, input: ValRef, state: StateRef) -> ValResult {
        self.exp.eval(input, state)
    }
}

impl Eval for Exp {
    fn eval(&self, input: ValRef, state: StateRef) -> ValResult {
        match self {
            Exp::Pipe(stages) => {
                let clone = state.clone();
                stages
                    .iter()
                    .try_fold(input, move |v, s| s.eval(v, clone.clone()))
            }
            Exp::Term(term) => term.eval(input, state),
            _ => Ok(Rc::new(Val::default())),
        }
    }
}

impl Eval for Term {
    fn eval(&self, input: ValRef, state: StateRef) -> ValResult {
        match self {
            Term::Call(f, args) => {
                let func = state
                    .borrow()
                    .funcs
                    .get(f)
                    .ok_or(format!(
                        "Identifier {} does not name a function",
                        f.join(crate::keywords::NAMESPACE)
                    ))?
                    .clone();
                if func.arity != args.len() {
                    return Err(format!(
                        "{}: Expected {} arguments, got {}",
                        f.join(crate::keywords::NAMESPACE),
                        func.arity,
                        args.len()
                    ));
                }
                let func = func.func;
                func(args.clone()).eval(input, state)
            }
            Term::Str(s) => Ok(Val::Str(s.clone()).into()),
            Term::Bool(b) => Ok(Val::Bool(b.clone()).into()),
            Term::Int(i) => Ok(Val::Int(i.clone()).into()),
            Term::Float(f) => Ok(Val::Float(f.clone()).into()),
            Term::Parent => parent(input.as_ref()),
            _ => todo!("{self:?}: Not implemented"),
        }
    }
}

mod builtins {
    use super::*;

    pub(crate) fn attribute(args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, state| {
            if let Val::Str(attr) = args[0].eval(input.clone(), state)?.as_ref() {
                super::attribute(input.as_ref(), &attr)
            } else {
                Err(type_error("String", &input))
            }
        };
        Box::new(lambda)
    }

    pub(crate) fn parent(_args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, _| super::parent(input.as_ref());
        Box::new(lambda)
    }

    pub(crate) fn select(args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, state| {
            if let Val::Str(sel) = args[0].eval(input.clone(), state)?.as_ref() {
                super::select(input.as_ref(), &sel)
            } else {
                Err(type_error("String", &input))
            }
        };
        Box::new(lambda)
    }

    pub(crate) fn select_all(args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, state| {
            if let Val::Str(sel) = args[0].eval(input.clone(), state)?.as_ref() {
                super::select_all(input.as_ref(), &sel)
            } else {
                Err(type_error("String", &input))
            }
        };
        Box::new(lambda)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::program;

    use kuchiki::traits::TendrilSink;
    use kuchiki::NodeRef;
    fn parse_html<I>(input: &mut I) -> NodeRef
    where
        I: std::io::Read,
    {
        kuchiki::parse_html()
            .from_utf8()
            .read_from(input)
            .map_err(|e| e.to_string())
            .unwrap()
    }

    fn compile_and_run(src: &str, input: ValRef) -> ValResult
where {
        let (_, p) = program(&src).unwrap();
        let state = Rc::new(RefCell::new(State::default()));
        p.eval(input, state)
    }

    #[test]
    fn string_literal() {
        let foo = "foo".to_string();
        let src = format!("\"{foo}\"");
        let input = Val::default();

        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(Val::Str(foo).into()), output);
    }

    #[test]
    fn select() {
        let src = "select(\"a\")";
        let html = "<a>link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("a").unwrap().as_node().clone());
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn select_all() {
        let src = "select_all(\"li\")";
        let html = "<li>One</li> <li>Two</li>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Vector(
            parsed_html
                .select("li")
                .unwrap()
                .into_iter()
                .map(|n| Val::Node(n.as_node().clone()))
                .collect(),
        );
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn select_all_no_matches() {
        let src = "select_all(\"a\")";
        let html = "<li>One</li> <li>Two</li>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Vector(vec![]);
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn attribute() {
        let src = ".href";
        let url = "https:://example.com".to_string();
        let html = format!("<a href=\"{url}\">link</a>");
        let a = parse_html(&mut html.as_bytes())
            .select_first("a")
            .unwrap()
            .as_node()
            .clone();
        let input = Val::Node(a);

        let expected = Val::Str(url);
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn select_attribute() {
        let src = "select(\"a\") | .href";
        let url = "https:://example.com".to_string();
        let html = format!("<a href=\"{url}\">link</a>");
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Str(url);
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn parent() {
        let src = "select(\"a\") | parent()";
        let html = "<span><a>Link</a></span>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("span").unwrap().as_node().clone());
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn parent_dotdot() {
        let src = "select(\"a\") | ..";
        let html = "<span><a>Link</a></span>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("span").unwrap().as_node().clone());
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn parent_of_root_raises() {
        let src = "..";
        let html = "<span><a>Link</a></span>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(&src, input.into());
        assert!(output.is_err());
    }
}
