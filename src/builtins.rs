use kuchiki::NodeRef;

use crate::error::type_error;
use crate::val::{Val, ValResult};

pub(crate) static FIND: &str = "find";
pub(crate) fn find<'a>(tree: &Val, selector: &str) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => {
            let n = n.select_first(selector).map_err(|_| {
                format!(
                    "Failed to parse CSS selector {}, or no matching element",
                    selector
                )
            })?;
            Ok(Node(n.as_node().clone()).into())
        }
        v => Err(type_error("Node", &v)),
    }
}
pub(crate) static FIND_ALL: &str = "find_all";
pub(crate) fn find_all<'a>(tree: &Val, selector: &str) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => {
            let ns = n
                .select(selector)
                .map_err(|_| format!("Failed to parse CSS selector {}", selector))?
                .map(|d| Node(d.as_node().clone()));
            Ok(Vector(ns.collect()).into())
        }
        v => Err(type_error("Node", &v)),
    }
}

pub(crate) static ATTRIBUTE: &str = "attribute";
pub(crate) fn attribute(tree: &Val, attribute: &str) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => {
            let element = n
                .as_element()
                .ok_or_else(|| "Expected an Element, got {}".to_owned())?;
            let attrs = element.attributes.borrow();
            let attr = attrs
                .get(attribute)
                .map(|s| Val::Str(s.to_owned()))
                .unwrap_or_default();
            Ok(attr.into())
        }
        v => Err(type_error("Node", &v)),
    }
}

pub(crate) static PARENT: &str = "parent";
pub(crate) fn parent(tree: &Val) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => {
            let n = n
                .parent()
                .ok_or_else(|| "Attempt to take parent of root element".to_owned())?;
            Ok(Node(n).into())
        }
        v => Err(type_error("Node", &v)),
    }
}

pub(crate) static NEXT_SIBLING: &str = "next_sibling";
pub(crate) fn next_sibling(tree: &Val) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => {
            let n = n
                .next_sibling()
                .ok_or_else(|| "Element has no next sibling".to_owned())?;
            Ok(Node(n).into())
        }
        v => Err(type_error("Node", &v)),
    }
}

fn find_element<F>(n: NodeRef, direction: F) -> Result<NodeRef, ()>
where
    F: Fn(NodeRef) -> Option<NodeRef>,
{
    use kuchiki::NodeData;
    use std::ops::ControlFlow::{Break, Continue};
    std::iter::repeat(())
        .try_fold(n, |n, _| match direction(n) {
            Some(node) => match node.data() {
                NodeData::Element(_) => Break(Ok(node.clone())),
                _ => Continue(node.clone()),
            },
            None => Break(Err(())),
        })
        // Safety: because the iterator is infinite, try_fold can only ever return the Break
        // variant
        .break_value()
        .unwrap()
}

pub(crate) static NEXT_ELEMENT: &str = "next_element";
pub(crate) fn next_element(tree: &Val) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => find_element(n.clone(), |n| n.next_sibling())
            .map_err(|_| "Element has no next sibling element".to_owned())
            .map(|n| Node(n).into()),
        v => Err(type_error("Node", &v)),
    }
}

pub(crate) static PREVIOUS_SIBLING: &str = "previous_sibling";
pub(crate) fn previous_sibling(tree: &Val) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => {
            let n = n
                .previous_sibling()
                .ok_or_else(|| "Element has no previous sibling".to_owned())?;
            Ok(Node(n).into())
        }
        v => Err(type_error("Node", &v)),
    }
}

pub(crate) static PREVIOUS_ELEMENT: &str = "previous_element";
pub(crate) fn previous_element(tree: &Val) -> ValResult {
    use Val::*;
    match tree {
        Node(n) => find_element(n.clone(), |n| n.previous_sibling())
            .map_err(|_| "Element has no previous_element element".to_owned())
            .map(|n| Node(n).into()),
        v => Err(type_error("Node", &v)),
    }
}
