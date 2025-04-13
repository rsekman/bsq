use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use casey::upper;

use crate::ast::{Exp, Identifier, Term, AST};
use crate::builtins::{
    attribute, find, find_all, next_element, next_sibling, parent, previous_element,
    previous_sibling, set_attribute, ATTRIBUTE, FIND, FIND_ALL, NEXT_ELEMENT, NEXT_SIBLING, PARENT,
    PREVIOUS_ELEMENT, PREVIOUS_SIBLING, SET_ATTRIBUTE,
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

macro_rules! builtins {
    ( $( ($name:ident, $arity:literal) ),* ) => {[
        $(
            (
                vec![upper!($name).to_owned()],
                Func { arity: $arity, func: Rc::new(builtins::$name) }
            )
        ),*
    ]};
}

impl Default for State {
    fn default() -> Self {
        let builtins = HashMap::<Identifier, Func>::from(builtins![
            (attribute, 1),
            (set_attribute, 2),
            (find, 1),
            (find_all, 1),
            (parent, 0),
            (next_sibling, 0),
            (next_element, 0),
            (previous_sibling, 0),
            (previous_element, 0)
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

impl<F> Eval for F
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
            Term::Bool(b) => Ok(Val::Bool(*b).into()),
            Term::Int(i) => Ok(Val::Int(*i).into()),
            Term::Float(f) => Ok(Val::Float(*f).into()),
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
                super::attribute(input.as_ref(), attr)
            } else {
                Err(type_error("String", &input))
            }
        };
        Box::new(lambda)
    }

    pub(crate) fn set_attribute(args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, state: StateRef| {
            let attr = args[0].eval(input.clone(), state.clone())?;
            let Val::Str(attr) = attr.as_ref() else {
                Err(type_error("String", &input))?
            };
            let value = args[1].eval(input.clone(), state)?;
            let Val::Str(value) = value.as_ref() else {
                Err(type_error("String", &input))?
            };
            super::set_attribute(input.as_ref(), attr, value)
        };
        Box::new(lambda)
    }

    pub(crate) fn parent(_args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, _| super::parent(input.as_ref());
        Box::new(lambda)
    }

    pub(crate) fn next_sibling(_args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, _| super::next_sibling(input.as_ref());
        Box::new(lambda)
    }

    pub(crate) fn next_element(_args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, _| super::next_element(input.as_ref());
        Box::new(lambda)
    }

    pub(crate) fn previous_sibling(_args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, _| super::previous_sibling(input.as_ref());
        Box::new(lambda)
    }

    pub(crate) fn previous_element(_args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, _| super::previous_element(input.as_ref());
        Box::new(lambda)
    }

    pub(crate) fn find(args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, state| {
            if let Val::Str(sel) = args[0].eval(input.clone(), state)?.as_ref() {
                super::find(input.as_ref(), sel)
            } else {
                Err(type_error("String", &input))
            }
        };
        Box::new(lambda)
    }

    pub(crate) fn find_all(args: ArgList) -> Box<dyn Eval> {
        let lambda = move |input: ValRef, state| {
            if let Val::Str(sel) = args[0].eval(input.clone(), state)?.as_ref() {
                super::find_all(input.as_ref(), sel)
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
        let (_, p) = program(src).unwrap();
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
    fn find() {
        let src = "find(\"a\")";
        let html = "<a>link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("a").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn find_all() {
        let src = "find_all(\"li\")";
        let html = "<li>One</li> <li>Two</li>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Vector(
            parsed_html
                .select("li")
                .unwrap()
                .map(|n| Val::Node(n.as_node().clone()))
                .collect(),
        );
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn find_all_no_matches() {
        let src = "find_all(\"a\")";
        let html = "<li>One</li> <li>Two</li>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Vector(vec![]);
        let output = compile_and_run(src, input.into());
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
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn find_attribute() {
        let src = "find(\"a\") | .href";
        let url = "https:://example.com".to_string();
        let html = format!("<a href=\"{url}\">link</a>");
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Str(url);
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn set_attribute() {
        let url_before = "https:://example.com".to_string();
        let url_after = "https:://github.com".to_string();
        let src = format!(".href = \"{url_after}\" | .href");
        let html = format!("<a href=\"{url_before}\">link</a>");
        let a = parse_html(&mut html.as_bytes())
            .select_first("a")
            .unwrap()
            .as_node()
            .clone();
        let input = Val::Node(a);

        let expected = Val::Str(url_after);
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn set_attribute_from_call() {
        let url_before = "https:://example.com".to_string();
        let url_after = "https:://github.com".to_string();
        let src = format!(".href = .id | .href");
        let html = format!("<a id=\"{url_after}\" href=\"{url_before}\">link</a>");
        let a = parse_html(&mut html.as_bytes())
            .select_first("a")
            .unwrap()
            .as_node()
            .clone();
        let input = Val::Node(a);

        let expected = Val::Str(url_after);
        let output = compile_and_run(&src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn parent() {
        let src = "find(\"a\") | parent()";
        let html = "<span><a>Link</a></span>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("span").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn parent_dotdot() {
        let src = "find(\"a\") | ..";
        let html = "<span><a>Link</a></span>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("span").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn parent_of_root_raises() {
        let src = "..";
        let html = "<span><a>Link</a></span>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }

    #[test]
    fn next_sibling() {
        let src = "find(\"span\") | next_sibling()";
        let html = "<span>Some text</span><a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("a").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn next_sibling_at_end_raises() {
        let src = "find(\"a\") | next_sibling()";
        let html = "<span>Some text</span><a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }

    #[test]
    fn next_element() {
        let src = "find(\"span\") | next_element()";
        let html = "<span>Some text</span>Some text outside tags<a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("a").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn next_element_at_end_raises() {
        let src = "find(\"a\") | next_element()";
        let html = "<span>Some text</span>Some text outside tags<a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }

    #[test]
    fn next_element_with_none_raises() {
        let src = "find(\"span\") | next_element()";
        let html = "<span>Some text</span>Some text outside tags";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let (_, compiled) = program(&src).unwrap();
        println!("{:?}", compiled.exp);
        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }

    #[test]
    fn previous_sibling() {
        let src = "find(\"a\") | previous_sibling()";
        let html = "<span>Some text</span><a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("span").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn previous_sibling_at_begin_raises() {
        let src = "find(\"span\") | previous_sibling()";
        let html = "<span>Some text</span><a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }

    #[test]
    fn previous_element() {
        let src = "find(\"a\") | previous_element()";
        let html = "<span>Some text</span>Some text outside tags<a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let expected = Val::Node(parsed_html.select_first("span").unwrap().as_node().clone());
        let output = compile_and_run(src, input.into());
        assert_eq!(Ok(expected.into()), output);
    }

    #[test]
    fn previous_element_at_begin_raises() {
        let src = "find(\"span\") | previous_element()";
        let html = "<span>Some text</span>Some text outside tags<a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }

    #[test]
    fn previous_element_with_none_raises() {
        let src = "find(\"a\") | previous_element()";
        let html = "Some text outside tags<a>Link</a>";
        let parsed_html = parse_html(&mut html.as_bytes());
        let input = Val::Node(parsed_html.clone());

        let output = compile_and_run(src, input.into());
        assert!(output.is_err());
    }
}
