pub(crate) const IF: &str = "if";
pub(crate) const ELSE: &str = "else";
pub(crate) const ELIF: &str = "elif";
pub(crate) const THEN: &str = "then";
pub(crate) const END: &str = "end";
pub(crate) const TRUE: &str = "true";
pub(crate) const FALSE: &str = "false";

// sorted array of keywords
pub(crate) const KEYWORDS: [&str; 7] = [ELIF, ELSE, END, FALSE, IF, THEN, TRUE];

// Syntax elements
pub(crate) const SEMICOLON: &str = ";";
pub(crate) const COMMA: &str = ",";
pub(crate) const THIS: &str = ".";
pub(crate) const PARENT: &str = "..";

pub(crate) const PAREN_OPEN: &str = "(";
pub(crate) const PAREN_CLOSE: &str = ")";

pub(crate) const BRACKET_OPEN: &str = "[";
pub(crate) const BRACKET_CLOSE: &str = "]";

pub(crate) const CURLY_OPEN: &str = "{";
pub(crate) const CURLY_CLOSE: &str = "}";

pub(crate) const PIPE: &str = "|";
pub(crate) const NAMESPACE: &str = "::";
