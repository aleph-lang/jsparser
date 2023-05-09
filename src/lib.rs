use aleph_syntax_tree::syntax::AlephTree as at;
use rslint_parser::{parse_text, SyntaxNode, SyntaxKind};

fn is_operator(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PLUS | SyntaxKind::MINUS | SyntaxKind::STAR | SyntaxKind::SLASH 
        | SyntaxKind::AMP | SyntaxKind::PIPE | SyntaxKind::BANG => true,
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
        SyntaxKind::NAME_REF | SyntaxKind::NAME => {
            let name = node.text();
            at::Var{var: name.to_string(), is_pointer: "False".to_string()}
        }
        SyntaxKind::LITERAL => {
            let value = node.text();
            let f = value.to_string().parse::<f64>();
            match f {
                Ok(_) => {
                    let i = value.to_string().parse::<u64>();
                    match i {
                        Ok(_) => at::Int{value: value.to_string()},
                        _ => at::Float{value: value.to_string()}
                    }
                },
                _ => {
                    match value.to_string().as_ref() {
                        "true" | "t" | "True" | "TRUE" => at::Bool{value: "True".to_string()},
                        "false" | "f" | "False" | "FALSE" => at::Bool{value: "False".to_string()},
                        _ => at::Var{var: value.to_string(), is_pointer: "False".to_string()}
                    }
                }
            }
        }
        SyntaxKind::SCRIPT | SyntaxKind::EXPR_STMT | SyntaxKind::BLOCK_STMT | SyntaxKind::GROUPING_EXPR => {
            compute_stmt(node.children().map(translate_ast).collect())
        }
        SyntaxKind::RETURN_STMT => {
            at::Return{value: Box::new(compute_stmt(node.children().map(translate_ast).collect()))}
        }
        SyntaxKind::UNARY_EXPR => {
            let operator = node
                .children_with_tokens()
                .find(|child| is_operator(child.kind()))
                .map(|token| token.kind())
                .expect("Missing operator");
            let operands: Vec<at> = node
                .children()
                .map(translate_ast)
                .collect();
            match operator {
                SyntaxKind::MINUS => at::Neg{expr: Box::new(operands[0].clone())},
                SyntaxKind::BANG => at::Not{bool_expr: Box::new(operands[0].clone())},
                _ => at::String{value : "Unknown".to_string()}
            }
        }
        SyntaxKind::BIN_EXPR => {
            let operator = node
                .children_with_tokens()
                .find(|child| is_operator(child.kind()))
                .map(|token| token.kind())
                .expect("Missing operator");
            let operands: Vec<at> = node
                .children()
                .map(translate_ast)
                .collect();
            match operator {
                SyntaxKind::PLUS => at::Add{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::MINUS => at::Sub{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::STAR => at::Mul{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::SLASH => at::Div{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                SyntaxKind::AMP => at::And{bool_expr1: Box::new(operands[0].clone()), bool_expr2: Box::new(operands[1].clone())},
                SyntaxKind::PIPE => at::Or{bool_expr1: Box::new(operands[0].clone()), bool_expr2: Box::new(operands[1].clone())},
                _ => at::String{value : "Unknown".to_string()}
            }
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
        SyntaxKind::CALL_EXPR => {
            let callee = node 
                .children()
                .find(|child| child.kind() == SyntaxKind::DOT_EXPR)
                .expect("Missing callee identifier")
                .clone();

            let parameters: Vec<Box<at>> = node
                .children()
                .filter(|child| child.kind() == SyntaxKind::ARG_LIST)
                .flat_map(|arg_list| arg_list.children())
                .map(|arg| Box::new(translate_ast(arg)))
                .collect();

            let at::App{object_name, fun, param_list: _} = translate_ast(callee) else { todo!() };
            at::App{object_name: object_name, fun: fun, param_list: parameters} 
        }
        SyntaxKind::DOT_EXPR => {
            if let Some(lhs) = node.children().next() {
                if let Some(rhs) = node.children().nth(1) {
                    at::App{object_name: lhs.to_string(), fun: Box::new(translate_ast(rhs)), param_list: Vec::new() }
                } else {
                    at::App{object_name: lhs.to_string(), fun: Box::new(at::Unit), param_list: Vec::new() }
                } 
            } else {
                if let Some(rhs) = node.children().nth(1) {
                    at::App{object_name: "".to_string(), fun: Box::new(translate_ast(rhs)), param_list: Vec::new() }
                } else {
                    at::App{object_name: "".to_string(), fun: Box::new(at::Unit), param_list: Vec::new() }
                } 
            }
        }
        _ => {
            println!("Found other node {:?}", node.kind());
            at::String{value: "Unknown_node".to_string()}
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


