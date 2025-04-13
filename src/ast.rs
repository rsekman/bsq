use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, isize, multispace0, multispace1},
    combinator::{eof, map, peek, recognize, value, verify},
    multi::{many0_count, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};

use crate::builtins::ATTRIBUTE;
use crate::keywords::*;

fn kw_with_ws<W, I, E>(kw: W) -> impl Parser<I, Output = I, Error = E>
where
    I: nom::Input + nom::Compare<W>,
    W: nom::Input + Clone,
    <I as nom::Input>::Item: nom::AsChar + Clone,
    E: nom::error::ParseError<I>,
{
    delimited(multispace0, tag(kw), multispace0)
}

fn is_keyword(w: &str) -> bool {
    KEYWORDS.binary_search(&w).is_ok()
}

fn is_not_keyword(w: &str) -> bool {
    !is_keyword(w)
}

pub(crate) struct AST {
    pub(crate) exp: Exp,
}

#[derive(Clone, PartialEq, Debug)]
/// The lifetime 'a here is the lifetime of the string from which the abstract syntax tree is parsed
/// -- identifiers are slices into that string
pub(crate) enum Exp {
    /// Composition of one or more filters
    Pipe(Vec<Exp>),
    /// List of filters
    List(Vec<Exp>),
    /// Branch construct
    If(Rc<Exp>, Rc<Exp>, ElseBody),
    /// Atomic term
    Term(Term),
}

fn exp(input: &str) -> IResult<&str, Exp> {
    alt((pipe, filter)).parse(input)
}

fn pipe(input: &str) -> IResult<&str, Exp> {
    map(separated_list1(kw_with_ws(PIPE), filter), Exp::Pipe).parse(input)
}

fn filter(input: &str) -> IResult<&str, Exp> {
    alt((if_then_else, list, map(term, Exp::Term))).parse(input)
}

fn list(input: &str) -> IResult<&str, Exp> {
    map(
        delimited(
            kw_with_ws(BRACKET_OPEN),
            separated_list1(kw_with_ws(COMMA), exp),
            kw_with_ws(BRACKET_CLOSE),
        ),
        Exp::List,
    )
    .parse(input)
}

fn if_then_else(input: &str) -> IResult<&str, Exp> {
    let (input, (_, cond, _, then, else_body)) =
        (kw_with_ws(IF), exp, kw_with_ws(THEN), exp, else_body).parse(input)?;
    Ok((input, Exp::If(Rc::new(cond), Rc::new(then), else_body)))
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ElseBody {
    Elif(Rc<Exp>, Rc<Exp>, Rc<ElseBody>),
    Else(Rc<Exp>),
}

fn else_body(input: &str) -> IResult<&str, ElseBody> {
    alt((
        map(
            (kw_with_ws(ELIF), exp, kw_with_ws(THEN), exp, else_body),
            |(_, cond, _, body, else_body)| {
                ElseBody::Elif(Rc::new(cond), Rc::new(body), Rc::new(else_body))
            },
        ),
        map(delimited(kw_with_ws(ELSE), exp, kw_with_ws(END)), |e| {
            ElseBody::Else(Rc::new(e))
        }),
    ))
    .parse(input)
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Term {
    This,
    Parent,
    EmptyDict,
    EmptyList,
    Bool(bool),
    Float(f64),
    Int(isize),
    // This has to be owned, because a string literal may contain escape sequences
    Str(String),
    Identifier(Identifier),
    Call(Identifier, Rc<Args>),
}

fn empty_dict(input: &str) -> IResult<&str, ()> {
    let (input, _) =
        recognize(pair(kw_with_ws(CURLY_OPEN), kw_with_ws(CURLY_CLOSE))).parse(input)?;
    Ok((input, ()))
}

fn empty_list(input: &str) -> IResult<&str, ()> {
    let (input, _) =
        recognize(pair(kw_with_ws(BRACKET_OPEN), kw_with_ws(BRACKET_CLOSE))).parse(input)?;
    Ok((input, ()))
}

fn term_separator(input: &str) -> IResult<&str, &str> {
    alt((
        eof,
        multispace1,
        // These tokens need to be unconsumed so parent parsers will still see them
        peek(kw_with_ws(SEMICOLON)),
        peek(kw_with_ws(COMMA)),
        peek(kw_with_ws(BRACKET_CLOSE)),
        peek(kw_with_ws(PAREN_OPEN)),
        peek(kw_with_ws(PAREN_CLOSE)),
    ))
    .parse(input)
}

fn double(input: &str) -> IResult<&str, f64> {
    nom::number::double().parse_complete(input)
}

macro_rules! terms {
    ($($term:expr),+) => {{
        alt((
            $( terminated(($term), term_separator),)+
        ))
    }};
}

fn term(input: &str) -> IResult<&str, Term> {
    preceded(
        multispace0,
        alt((
            call,
            map(crate::str::quoted_string, Term::Str),
            value(Term::EmptyDict, empty_dict),
            value(Term::EmptyList, empty_list),
            terms![
                attribute_access,
                map(identifier, Term::Identifier),
                value(Term::Bool(false), kw_with_ws(FALSE)),
                value(Term::Bool(true), kw_with_ws(TRUE)),
                map(isize, Term::Int),
                map(double, Term::Float)
            ],
            value(Term::Parent, tag(PARENT)),
            value(Term::This, tag(THIS)),
        )),
    )
    .parse(input)
}

type Args = Vec<Exp>;

pub(crate) type IdentifierPart = String;
pub(crate) type Identifier = Vec<IdentifierPart>;

fn identifier_part(input: &str) -> IResult<&str, IdentifierPart> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        String::from,
    )
    .parse(input)
}

fn identifier(input: &str) -> IResult<&str, Identifier> {
    verify(
        separated_list1(tag(NAMESPACE), identifier_part),
        |v: &Vec<IdentifierPart>| match v.len() {
            0 => false,
            1 => is_not_keyword(v.last().unwrap()),
            _ => true,
        },
    )
    .parse(input)
}

fn call(input: &str) -> IResult<&str, Term> {
    let arg_list = delimited(kw_with_ws(PAREN_OPEN), args, kw_with_ws(PAREN_CLOSE));
    let (input, (identifier, args)) = (identifier, arg_list).parse(input)?;
    Ok((input, Term::Call(identifier, args.into())))
}

fn args(input: &str) -> IResult<&str, Args> {
    separated_list0(kw_with_ws(SEMICOLON), exp).parse(input)
}

fn attribute_access(input: &str) -> IResult<&str, Term> {
    preceded(
        kw_with_ws(THIS),
        alt((
            map(identifier_part, |a| {
                Term::Call(
                    vec![ATTRIBUTE.to_owned()],
                    vec![Exp::Term(Term::Str(a.to_owned()))].into(),
                )
            }),
            map(
                delimited(kw_with_ws(BRACKET_OPEN), exp, kw_with_ws(BRACKET_CLOSE)),
                |e| Term::Call(vec![ATTRIBUTE.to_owned()], vec![e].into()),
            ),
        )),
    )
    .parse(input)
}

pub(crate) fn program(input: &str) -> IResult<&str, AST> {
    map(terminated(exp, eof), |exp| AST { exp }).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! string_vec {
        ($($str:expr),*) => ({
            (vec![$($str.to_string()),*])
        });
    }
    /// Create the AST representation of an identifier from a list of parts
    /// I.e., identifier!["foo", "bar"] is what "foo::bar" should parse into
    macro_rules! identifier {
        ($($str:expr),*) => ({
            Term::Identifier(string_vec![$($str),*])
        });
    }

    /// Create the AST representation of a pipe from a list of parts
    /// I.e., pipe!["foo", "bar"] is what "foo | bar" should parse into
    macro_rules! pipe {
        ($($str:expr),*) => ({
            Exp::Pipe(
                vec![$(Exp::Term(identifier![$str]),)*]
            )
        });
    }

    fn assert_parse<I, T>(expected: T, got: IResult<I, T>)
    where
        T: PartialEq + std::fmt::Debug,
        I: std::fmt::Debug,
    {
        assert!(got.is_ok());

        let (_, got) = got.unwrap();
        assert_eq!(expected, got);
    }

    mod identifier {
        use super::*;

        #[test]
        fn local() {
            let input = "a";
            let identifier = identifier!["a"];
            let parsed = term.parse(input);
            assert_parse(identifier, parsed)
        }

        #[test]
        fn snake_case() {
            let input = "snake_case";
            let identifier = identifier!["snake_case"];
            let parsed = term.parse(input);
            assert_parse(identifier, parsed)
        }

        #[test]
        fn namespaced() {
            let input = "a::b";
            let identifier = identifier!["a", "b"];
            let parsed = term.parse(input);
            assert_parse(identifier, parsed)
        }

        #[test]
        fn invalid() {
            let input = "0a";
            let parsed = identifier.parse(input);
            assert!(parsed.is_err())
        }

        #[test]
        fn reserved() {
            let input = "if";
            let parsed = term.parse(input);
            assert!(parsed.is_err())
        }

        #[test]
        fn reserved_in_namespace() {
            let input = "foo::if";
            let parsed = term.parse(input);
            assert_parse(identifier!["foo", "if"], parsed);
        }

        #[test]
        fn call_one() {
            let input = "foo(bar)";
            let parsed = term.parse(input);
            assert_parse(
                Term::Call(string_vec!["foo"], vec![pipe!["bar"]].into()),
                parsed,
            );
        }

        #[test]
        fn call_two() {
            let input = "foo(bar; baz)";
            let parsed = term.parse(input);
            assert_parse(
                Term::Call(string_vec!["foo"], vec![pipe!["bar"], pipe!["baz"]].into()),
                parsed,
            );
        }

        #[test]
        fn call_namespaced() {
            let input = "foo::bar(baz)";
            let parsed = term.parse(input);
            assert_parse(
                Term::Call(string_vec!["foo", "bar"], vec![pipe!["baz"]].into()),
                parsed,
            );
        }

        #[test]
        fn attribute_access() {
            let input = ".foo";
            let parsed = term.parse(input);
            assert_parse(
                Term::Call(
                    string_vec![ATTRIBUTE],
                    vec![Exp::Term(Term::Str("foo".to_owned()))].into(),
                ),
                parsed,
            );
        }

        #[test]
        fn attribute_access_expr() {
            let input = ".[foo | bar]";
            let parsed = term.parse(input);
            assert_parse(
                Term::Call(string_vec![ATTRIBUTE], vec![pipe!["foo", "bar"]].into()),
                parsed,
            );
        }
    }

    mod pipe {
        use super::*;

        #[test]
        fn trivial() {
            let input = "a";
            let parsed = pipe(input);

            assert_parse(pipe!["a"], parsed);
        }

        #[test]
        fn non_trivial() {
            let input = "a | b";
            let parsed = pipe(input);

            assert_parse(pipe!["a", "b"], parsed);
        }

        #[test]
        fn calls() {
            let input = "f(a) | g(b)";
            let parsed = exp(input);

            let expected = Exp::Pipe(vec![
                Exp::Term(Term::Call(string_vec!["f"], vec![pipe!["a"]].into())),
                Exp::Term(Term::Call(string_vec!["g"], vec![pipe!["b"]].into())),
            ]);

            assert_parse(expected, parsed);
        }
    }

    mod literals {
        use super::*;

        #[test]
        fn string() {
            let s = "foo".to_owned();
            let input = format!("\"{s}\"");
            let parsed = term(&input);

            assert_parse(Term::Str(s), parsed);
        }

        #[test]
        fn empty_dict() {
            let input = "{ }";
            let parsed = term(input);

            assert_parse(Term::EmptyDict, parsed);
        }

        #[test]
        fn empty_list() {
            let input = "[ ]";
            let parsed = term(input);

            assert_parse(Term::EmptyList, parsed);
        }

        #[test]
        fn bool_false() {
            let input = "false";
            let parsed = term(input);

            assert_parse(Term::Bool(false), parsed);
        }
        #[test]
        fn bool_true() {
            let input = "true";
            let parsed = term(input);

            assert_parse(Term::Bool(true), parsed);
        }

        #[test]
        fn int() {
            let input = "1337";
            let parsed = term(input);

            assert_parse(Term::Int(1337isize), parsed);
        }

        #[test]
        fn float() {
            let input = "3.14159";
            let parsed = term(input);

            assert_parse(Term::Float(3.14159), parsed);
        }

        #[test]
        fn this() {
            let input = ".";
            let parsed = term(input);

            assert_parse(Term::This, parsed);
        }

        #[test]
        fn parent() {
            let input = "..";
            let parsed = term(input);

            assert_parse(Term::Parent, parsed);
        }
    }

    mod list {
        use super::*;

        #[test]
        fn list() {
            let input = "[1, 2,3]";
            let parsed = super::list(input);
            let expected = Exp::List(
                vec![1, 2, 3]
                    .into_iter()
                    .map(|i| Exp::Pipe(vec![Exp::Term(Term::Int(i))]))
                    .collect(),
            );

            assert_parse(expected, parsed);
        }
    }

    mod if_then_else {
        use super::*;

        #[test]
        fn else_tail() {
            let input = "else c end";
            let parsed = else_body(input);
            assert_parse(ElseBody::Else(Rc::new(pipe!["c"])), parsed)
        }

        #[test]
        fn elif_body() {
            let input = "elif a then b else c end";
            let parsed = else_body(input);

            let cond = Rc::new(pipe!["a"]);
            let body = Rc::new(pipe!["b"]);
            let tail = ElseBody::Else(Rc::new(pipe!["c"]));
            let expected = ElseBody::Elif(cond, body, Rc::new(tail));
            assert_parse(expected, parsed)
        }

        #[test]
        fn if_then_else() {
            let input = "if a then b else c end";
            let parsed = super::if_then_else(input);

            let cond = Rc::new(pipe!["a"]);
            let body = Rc::new(pipe!["b"]);
            let tail = ElseBody::Else(Rc::new(pipe!["c"]));
            let expected = Exp::If(cond, body, tail);
            assert_parse(expected, parsed)
        }

        #[test]
        fn if_then_elif_then_else() {
            let input = "if a then b elif c then d else e end";
            let parsed = super::if_then_else(input);

            let cond = Rc::new(pipe!["a"]);
            let body = Rc::new(pipe!["b"]);
            let elif_cond = Rc::new(pipe!["c"]);
            let elif_body = Rc::new(pipe!["d"]);
            let tail = ElseBody::Else(Rc::new(pipe!["e"]));
            let elif = ElseBody::Elif(elif_cond, elif_body, Rc::new(tail));
            let expected = Exp::If(cond, body, elif);
            assert_parse(expected, parsed)
        }
    }
}
