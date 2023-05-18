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
            | sk::DOT2 | sk::COLON | sk::FAT_ARROW | sk::MINUS2 | sk::LTEQ | sk::GTEQ | sk::PLUSEQ | sk::MINUSEQ | sk::PIPEEQ | sk::AMPEQ
            | sk::CARETEQ | sk::SLASHEQ | sk::STAREQ | sk::PERCENTEQ | sk::AMP2 | sk::PIPE2 | sk::SHL | sk::SHR
            | sk::USHR | sk::SHLEQ | sk::SHREQ | sk::USHREQ | sk::AMP2EQ | sk::PIPE2EQ | sk::STAR2EQ | sk::QUESTION2EQ
            | sk::AT | sk::AWAIT_KW | sk::BREAK_KW | sk::CASE_KW | sk::CATCH_KW | sk::CLASS_KW | sk::CONST_KW | sk::CONTINUE_KW
            | sk::DEBUGGER_KW | sk::DEFAULT_KW | sk::DELETE_KW | sk::DO_KW | sk::ELSE_KW | sk::ENUM_KW | sk::EXPORT_KW
            | sk::EXTENDS_KW | sk::FALSE_KW | sk::FINALLY_KW | sk::FOR_KW | sk::FUNCTION_KW | sk::IF_KW | sk::IN_KW | sk::INSTANCEOF_KW
            | sk::INTERFACE_KW | sk::IMPORT_KW | sk::IMPLEMENTS_KW | sk::NEW_KW | sk::NULL_KW | sk::PACKAGE_KW | sk::PRIVATE_KW
            | sk::PROTECTED_KW | sk::PUBLIC_KW | sk::RETURN_KW | sk::SUPER_KW | sk::SWITCH_KW | sk::THIS_KW | sk::THROW_KW
            | sk::TRY_KW | sk::TRUE_KW | sk::TYPEOF_KW | sk::VAR_KW | sk::VOID_KW | sk::WHILE_KW | sk::WITH_KW | sk::YIELD_KW
            | sk::READONLY_KW | sk::KEYOF_KW | sk::UNIQUE_KW | sk::DECLARE_KW | sk::ABSTRACT_KW | sk::STATIC_KW | sk::ASYNC_KW
            | sk::TYPE_KW | sk::FROM_KW | sk::AS_KW | sk::REQUIRE_KW | sk::NAMESPACE_KW | sk::ASSERT_KW | sk::MODULE_KW | sk::GLOBAL_KW
            | sk::INFER_KW | sk::GET_KW | sk::SET_KW | sk::NUMBER | sk::STRING | sk::REGEX | sk::HASH | sk::TEMPLATE_CHUNK
            | sk::DOLLARCURLY | sk::BACKTICK | sk::ERROR_TOKEN | sk::IDENT | sk::WHITESPACE | sk::COMMENT | sk::SHEBANG
            | sk::MODULE | sk::ERROR | sk::DECLARATOR
            | sk::DO_WHILE_STMT | sk::WHILE_STMT | sk::FOR_STMT | sk::FOR_IN_STMT | sk::CONTINUE_STMT | sk::BREAK_STMT
            | sk::WITH_STMT | sk::SWITCH_STMT | sk::CASE_CLAUSE | sk::DEFAULT_CLAUSE | sk::LABELLED_STMT
            | sk::THROW_STMT | sk::TRY_STMT | sk::CATCH_CLAUSE | sk::FINALIZER | sk::DEBUGGER_STMT
            | sk::PARAMETER_LIST | sk::THIS_EXPR | sk::ARRAY_EXPR | sk::OBJECT_EXPR | sk::LITERAL_PROP | sk::GETTER
            | sk::SETTER | sk::NEW_EXPR | sk::FN_EXPR | sk::BRACKET_EXPR
            | sk::COND_EXPR | sk::SEQUENCE_EXPR | sk::ARG_LIST
            | sk::TEMPLATE | sk::TEMPLATE_ELEMENT | sk::SPREAD_ELEMENT | sk::SUPER_CALL | sk::IMPORT_CALL
            | sk::NEW_TARGET | sk::IMPORT_META | sk::IDENT_PROP | sk::SPREAD_PROP | sk::INITIALIZED_PROP | sk::OBJECT_PATTERN
            | sk::ARRAY_PATTERN | sk::ASSIGN_PATTERN | sk::REST_PATTERN | sk::KEY_VALUE_PATTERN | sk::COMPUTED_PROPERTY_NAME
            | sk::FOR_OF_STMT | sk::SINGLE_PATTERN | sk::ARROW_EXPR | sk::YIELD_EXPR | sk::CLASS_DECL | sk::CLASS_EXPR
            | sk::CLASS_BODY | sk::METHOD | sk::IMPORT_DECL | sk::EXPORT_DECL | sk::EXPORT_NAMED | sk::EXPORT_DEFAULT_DECL
            | sk::EXPORT_DEFAULT_EXPR | sk::EXPORT_WILDCARD | sk::WILDCARD_IMPORT | sk::NAMED_IMPORTS | sk::SPECIFIER
            | sk::AWAIT_EXPR | sk::FOR_STMT_TEST | sk::FOR_STMT_UPDATE | sk::FOR_STMT_INIT | sk::PRIVATE_NAME
            | sk::CLASS_PROP | sk::PRIVATE_PROP | sk::CONSTRUCTOR | sk::CONSTRUCTOR_PARAMETERS| sk::PRIVATE_PROP_ACCESS
            | sk::IMPORT_STRING_SPECIFIER | sk::EXPR_PATTERN | sk::TS_ANY | sk::TS_UNKNOWN | sk::TS_NUMBER | sk::TS_OBJECT
            | sk::TS_BOOLEAN | sk::TS_BIGINT | sk::TS_STRING | sk::TS_SYMBOL | sk::TS_VOID | sk::TS_UNDEFINED | sk::TS_NULL
            | sk::TS_NEVER | sk::TS_THIS | sk::TS_LITERAL | sk::TS_PREDICATE | sk::TS_TUPLE | sk::TS_TUPLE_ELEMENT | sk::TS_PAREN
            | sk::TS_TYPE_REF | sk::TS_QUALIFIED_PATH | sk::TS_TYPE_NAME | sk::TS_TEMPLATE | sk::TS_TEMPLATE_ELEMENT
            | sk::TS_MAPPED_TYPE | sk::TS_MAPPED_TYPE_PARAM | sk::TS_MAPPED_TYPE_READONLY | sk::TS_TYPE_QUERY | sk::TS_TYPE_QUERY_EXPR
            | sk::TS_IMPORT | sk::TS_TYPE_ARGS | sk::TS_ARRAY | sk::TS_INDEXED_ARRAY | sk::TS_TYPE_OPERATOR| sk::TS_INTERSECTION
            | sk::TS_UNION | sk::TS_TYPE_PARAMS | sk::TS_FN_TYPE | sk::TS_CONSTRUCTOR_TYPE | sk::TS_EXTENDS | sk::TS_CONDITIONAL_TYPE
            | sk::TS_CONSTRAINT | sk::TS_DEFAULT | sk::TS_TYPE_PARAM | sk::TS_NON_NULL | sk::TS_ASSERTION | sk::TS_CONST_ASSERTION
            | sk::TS_ENUM | sk::TS_ENUM_MEMBER | sk::TS_TYPE_ALIAS_DECL | sk::TS_NAMESPACE_DECL | sk::TS_MODULE_BLOCK
            | sk::TS_MODULE_DECL | sk::TS_CONSTRUCTOR_PARAM | sk::TS_CALL_SIGNATURE_DECL | sk::TS_CONSTRUCT_SIGNATURE_DECL
            | sk::TS_INDEX_SIGNATURE | sk::TS_METHOD_SIGNATURE | sk::TS_PROPERTY_SIGNATURE | sk::TS_INTERFACE_DECL | sk::TS_ACCESSIBILITY
            | sk::TS_OBJECT_TYPE | sk::TS_EXPR_WITH_TYPE_ARGS | sk::TS_IMPORT_EQUALS_DECL | sk::TS_MODULE_REF | sk::TS_EXTERNAL_MODULE_REF
            | sk::TS_EXPORT_ASSIGNMENT | sk::TS_NAMESPACE_EXPORT_DECL | sk::TS_DECORATOR | sk::TS_INFER
            | sk::AMP | sk::PIPE | sk::QUESTIONDOT | sk::PLUS2 | sk::STAR2 | sk::CARET | sk::PERCENT | sk::DOT => {
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


