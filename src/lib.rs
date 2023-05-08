use aleph_syntax_tree::syntax::AlephTree as at;
use rslint_parser::{parse_text, SyntaxNode, SyntaxKind};

fn is_operator(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PLUS | SyntaxKind::MINUS | SyntaxKind::STAR | SyntaxKind::SLASH => true,
        _ => false
    }
}

fn compute_stmt(node_list: Vec<at>) -> at {
    let mut res = at::Unit;
    for node in node_list {
        res = match res {
            at::Unit => node,
            _ => at::Stmts{expr1: Box::new(res), expr2: Box::new(node)}
        }
    }
    res

}

fn translate_ast(node: SyntaxNode) -> at {
    match node.kind() {
        SyntaxKind::SCRIPT | SyntaxKind::EXPR_STMT | SyntaxKind::BLOCK_STMT => {
            compute_stmt(node.children().map(translate_ast).collect())
        }
        SyntaxKind::RETURN_STMT => {
            compute_stmt(node.children().map(translate_ast).collect())
        }
        SyntaxKind::BIN_EXPR => {
            let operator = node
                .children_with_tokens()
                .find(|child| is_operator(child.kind()))
                .map(|token| token.kind())
                .expect("Missing operator");
            let operands: Vec<at> = node
                .children()
                .filter(|child| child.kind() == SyntaxKind::NAME_REF || child.kind().is_literal())
                .map(translate_ast)
                .collect();
            match operator {
                SyntaxKind::PLUS => at::Add{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::MINUS => at::Sub{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::STAR => at::Mul{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::SLASH => at::Div{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                _ => at::String{value : "Unknown".to_string()}
            }
        }
        SyntaxKind::NAME_REF => {
            let name = node.text();
            at::Var{var: name.to_string(), is_pointer: "False".to_string()}
        }
        SyntaxKind::FN_DECL => {
            let name = node
                .children()
                .find(|child| child.kind() == SyntaxKind::NAME)
                .and_then(|child| Some(child.text()))
                .expect("Missing function name")
                .to_string();

            let parameters: Vec<Box<at>> = node
                .children()
                .find(|child| child.kind() == SyntaxKind::PARAMETER_LIST)
                .expect("Missing parameter list")
                .children()
                .map(|param| Box::new(at::Var{var: param.text().to_string(), is_pointer: "False".to_string()}))
                .collect();

            let body = node
                .children()
                .find(|child| child.kind() == SyntaxKind::BLOCK_STMT)
                .map(|block| Box::new(translate_ast(block)))
                .expect("Missing function body");

            at::LetRec{name: name, args: parameters, body: body}
        }
        _ => {
            println!("Found other node {:?}", node.kind());
            at::String{value: "Unknown".to_string()}
        }
    }
}

/// Javascript parser
/// #Arguments
/// `source` - String to parse
///
/// # Return
/// This function return an AlephTree
pub fn parse(source: String) -> at {
    let ast = parse_text(&source,0);
    //println!("AST {:?}", ast);

    let root_node = ast.syntax();
    translate_ast(root_node)
}


