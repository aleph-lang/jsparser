use aleph_syntax_tree::syntax::AlephTree as at;
use rslint_parser::{parse_text, SyntaxNode};
use rslint_parser::SyntaxKind as sk;

fn is_operator(kind: sk) -> bool {
    match kind {
        sk::PLUS | sk::MINUS | sk::STAR | sk::SLASH 
            | sk::EQ | sk::EQ2 | sk::EQ3 | sk::NEQ | sk::NEQ2 
            | sk::AMP | sk::PIPE | sk::BANG => true,
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
        sk::EMPTY_STMT => {
            at::Unit
        }
        sk::NAME_REF | sk::NAME => {
            let name = node.text();
            at::Var{var: name.to_string(), is_pointer: "False".to_string()}
        }
        sk::LITERAL => {
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
        sk::SCRIPT | sk::EXPR_STMT | sk::BLOCK_STMT | sk::GROUPING_EXPR => {
            compute_stmt(node.children().map(translate_ast).collect())
        }
        sk::RETURN_STMT => {
            at::Return{value: Box::new(compute_stmt(node.children().map(translate_ast).collect()))}
        }
        sk::UNARY_EXPR => {
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
                sk::MINUS => at::Neg{expr: Box::new(operands[0].clone())},
                sk::BANG => at::Not{bool_expr: Box::new(operands[0].clone())},
                _ => at::String{value : "Unknown".to_string()}
            }
        }
        sk::BIN_EXPR => {
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
                sk::PLUS => at::Add{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                sk::MINUS => at::Sub{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                sk::STAR => at::Mul{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                sk::SLASH => at::Div{number_expr1: Box::new(operands[0].clone()), number_expr2: Box::new(operands[1].clone())},
                sk::AMP => at::And{bool_expr1: Box::new(operands[0].clone()), bool_expr2: Box::new(operands[1].clone())},
                sk::PIPE => at::Or{bool_expr1: Box::new(operands[0].clone()), bool_expr2: Box::new(operands[1].clone())},
                sk::EQ => at::Eq{expr1: Box::new(operands[0].clone()), expr2: Box::new(operands[1].clone())},
                sk::EQ2 => at::Eq{expr1: Box::new(operands[0].clone()), expr2: Box::new(operands[1].clone())},
                sk::EQ3 => at::Eq{expr1: Box::new(operands[0].clone()), expr2: Box::new(operands[1].clone())},
                sk::NEQ => at::Not{bool_expr: Box::new(at::Eq{expr1: Box::new(operands[0].clone()), expr2: Box::new(operands[1].clone())})},
                sk::NEQ2 => at::Not{bool_expr: Box::new(at::Eq{expr1: Box::new(operands[0].clone()), expr2: Box::new(operands[1].clone())})},
                _ => at::String{value : "Unknown".to_string()}
            }
        }
        sk::FN_DECL => {
            let name = node
                .children()
                .find(|child| child.kind() == sk::NAME)
                .and_then(|child| Some(child.text()))
                .expect("Missing function name")
                .to_string();

            let parameters: Vec<Box<at>> = node
                .children()
                .find(|child| child.kind() == sk::PARAMETER_LIST)
                .expect("Missing parameter list")
                .children()
                .map(|param| Box::new(at::Var{var: param.text().to_string(), is_pointer: "False".to_string()}))
                .collect();

            let body = node
                .children()
                .find(|child| child.kind() == sk::BLOCK_STMT)
                .map(|block| Box::new(translate_ast(block)))
                .expect("Missing function body");

            at::LetRec{name: name, args: parameters, body: body}
        }
        sk::CALL_EXPR => {
            let callee = node 
                .children()
                .find(|child| child.kind() == sk::DOT_EXPR)
                .expect("Missing callee identifier")
                .clone();

            let parameters: Vec<Box<at>> = node
                .children()
                .filter(|child| child.kind() == sk::ARG_LIST)
                .flat_map(|arg_list| arg_list.children())
                .map(|arg| Box::new(translate_ast(arg)))
                .collect();

            let at::App{object_name, fun, param_list: _} = translate_ast(callee) else { todo!() };
            at::App{object_name: object_name, fun: fun, param_list: parameters} 
        }
        sk::DOT_EXPR => {
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
        sk::VAR_DECL => {
            if let Some(identifier) = node.children().next() {
                at::Var{var: identifier.text().to_string(), is_pointer: "False".to_string()}
            } else { todo!() }
        }
        sk::ASSIGN_EXPR => {
            if let Some(lhs) = node.children().next() {
                if let Some(rhs) = node.children().nth(1) {
                    at::Let{var: lhs.text().to_string(), is_pointer: "False".to_string(), value: Box::new(translate_ast(rhs)), expr: Box::new(at::Unit)}
                } else { todo!() }
            } else { todo!() }
        }
        sk::IF_STMT => {
            let branches: Vec<Box<at>> = node.children()
                .filter(|child| child.kind() == sk::BLOCK_STMT)
                .map(|arg| Box::new(translate_ast(arg)))
                .collect();

            if let Some(condition) = node.children().find(|child| child.kind() == sk::CONDITION) {
                let mut thn = Box::new(at::Unit);
                if let Some(then_ast) = branches.get(0) {
                    thn = then_ast.clone()
                }
                let mut els = Box::new(at::Unit);
                if let Some(els_ast) = branches.get(1) {
                    els = els_ast.clone()
                }
                at::If{condition: Box::new(translate_ast(condition)), then: thn, els: els}
            } else { todo!() }
        }
        sk::CONDITION => {
            if let Some(condition) = node.children().next() {
                translate_ast(condition)
            } else { todo!() }
        }
        sk::SEMICOLON | sk::COMMA | sk::L_PAREN | sk::R_PAREN | sk::L_CURLY | sk::R_CURLY | sk::BANG
            | sk::L_BRACK | sk::R_BRACK | sk::L_ANGLE | sk::R_ANGLE | sk::TILDE | sk::QUESTION | sk::QUESTION2
            | sk::PLUS | sk::MINUS | sk::STAR | sk::SLASH | sk::EQ | sk::EQ2 | sk::EQ3 | sk::NEQ | sk::NEQ2 
            | sk::AMP | sk::PIPE | sk::QUESTIONDOT | sk::PLUS2 | sk::STAR2 => {
                println!("Found ignoring node {:?}", node.kind());
                at::String{value: "Ignore_node".to_string()}

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


