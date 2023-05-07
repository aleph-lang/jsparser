use aleph_syntax_tree::syntax::AlephTree as at;
use rslint_parser::{parse_text, Parse, SyntaxNode, SyntaxKind};
use rslint_parser::ast::Script;

fn is_operator(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PLUS | SyntaxKind::MINUS => true,
        _ => false
    }
}

fn process_node(node: &SyntaxNode) {
    match node.kind() {
        SyntaxKind::SCRIPT => {
            println!("Found SCRIPT node");
        }
        SyntaxKind::EXPR_STMT => {
            println!("Found EXPR_STMT node");
        }
        SyntaxKind::BIN_EXPR => {
            let operator = node
                .children_with_tokens()
                .find(|child| is_operator(child.kind()))
                .unwrap();
            let mut operands = node
                .children()
                .filter(|child| child.kind() == SyntaxKind::NAME_REF)
                .map(|child| child.text().to_string());
            println!("Found BIN_EXPR node {:?} {:?} {:?}", operands.next(), operator, operands.next());
        }
        SyntaxKind::NAME_REF => {
            let name = node.text();
            println!("Found NAME_REF node: {}", name);
        }
        _ => {
            println!("Found other node {:?}", node.kind());
        }
    }

    for child in node.children() {
        process_node(&child);
    }
}

fn translate(ast: Parse<Script>) -> at {
    let root_node = ast.syntax();
    process_node(&root_node);

    at::Unit
}

/// Javascript parser
/// #Arguments
/// `source` - String to parse
///
/// # Return
/// This function return an AlephTree
pub fn parse(source: String) -> at {
    let ast = parse_text(&source,0);
    translate(ast)   
}


