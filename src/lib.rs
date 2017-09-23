#![allow(unused_variables)]
#![allow(dead_code)]

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ref {
    pub t: Type,
    pub ident: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Void, /* In the C sense, not in the type theory sense. */
    Int,
    Char,
    Struct(Box<Ref> /* name */, Vec<Ref> /* fields */),
    FuncType(Vec<Ref>, Box<Type>),
    Pointer(Box<Type>),
    ArrayOf(Box<Type>, usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    V(Ref),
    Unsigned(usize),
    Inc(Type, Box<Expr>),
    Dec(Type, Box<Expr>),
    App(Type, Ref, Vec<Expr>),
    Deref(Type, Box<Expr>),
    StructFieldAccess(Type, Ref, String),
    StructExpr(Type, String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Assign(Ref, Expr),
    Conditional(Expr, Vec<Stmt>, Vec<Stmt>),
    StandaloneExpr(Expr),
    Return(Expr),
    VarDecl(Ref),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Top {
    StructDef(Ref, Vec<Ref>),
    StructDecl(Ref),
    FuncDef(Ref, Vec<Ref>, Vec<Stmt>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub tops: Vec<Top>,
}

use self::Type::*;
use self::Expr::*;
use self::Stmt::*;
use self::Top::*;

fn make_tabs(tabs: usize) -> String {
    let mut result = format!("");
    for i in 0..tabs {
        result.push_str("  ");
    }
    result
}

fn format_line(tabs: usize, line: String) -> String {
    let mut result = make_tabs(tabs);
    result.push_str(&line);
    result.push_str("\n");
    result
}

// TODO Consider changing 'emit' to another verb that more clearly
// denotes producing a string, rather than printing a string.

pub fn emit_decl(r: Ref) -> String {
    emit_decl_rec(r.t, Some(r.ident), &mut format!(""), &mut format!(""))
}

// Returns true if t is a type constructor whose adjustment is denoted
// on the right hand side of a C declaration and false otherwise.
fn is_right_side_constructor(t: &Type) -> bool {
    match *t {
        ArrayOf(_, _) | FuncType(_, _) => true,
        Void | Int | Char | Struct(_, _) | Pointer(_) => false,
    }
}

// We have the following precedence issue: Suppose we have a PCF
// function foo of type int -> int -> int. In C code, this should be
// represented as
//
//     int (*foo(int x))(int)
//
// without taking care to add parenthesis, we would naively emit
//
//     int* foo(int x)(int),
//
// which reads as "foo is a function (int) returning function (int)
// returning pointer to int."
//
// Reading C declarations, the rule is "go right when you can,
// otherwise go left." We want to return a pointer to a function (int)
// return int, so we must specify that the "pointer to" get parsed
// prior to the "function (int) returning," as by default, this will
// not happen - "function returning" goes on the right and so will be
// parsed prior to "pointer to." So we must add parenthesis for
// grouping purposes.
//
// This precedence issue will occur any time we want to return a
// function pointer.
//
// More generally, parenthesis are needed any time we have a "left
// side" constructor before a "right side" constructor.

// Returns true if t is a type constructor whose adjustment is denoted
// on the left hand side of a C declaration and false otherwise.
fn is_left_side_constructor(t: &Type) -> bool {
    match *t {
        Pointer(_) => true,
        ArrayOf(_, _) | FuncType(_, _) | Void | Int | Char | Struct(_, _) => {
            false
        }
    }
}

fn has_right_side_constructor_child(t: &Type) -> bool {
    match *t {
        Void | Int | Char | Struct(_, _) => false,
        // We don't care about the domain of f here, since it's within
        // parenthesis.
        FuncType(_, ref t2) |
        Pointer(ref t2) |
        ArrayOf(ref t2, _) => is_right_side_constructor(&**t2),
    }
}

fn emit_decl_rec(
    t: Type,
    ident: Option<String>,
    prefix: &mut String,
    suffix: &mut String,
) -> String {
    // println!("t: {:?}, prefix: {}, suffix: {}", &t, &prefix, &suffix);
    // The prefix of the prefix, as you might expect.
    let mut prefix_prefix = format!("");
    let formatted_ident: String = match ident.clone() {
        Some(name) => {
            match (name.len() > 0, prefix.ends_with("(*")) {
                (false, _) => format!(""),
                (true, true) => name,
                (true, false) => format!(" {}", name),
            }
        }
        None => format!(""),
    };
    if is_left_side_constructor(&t) && has_right_side_constructor_child(&t) {
        prefix_prefix.push_str("(");
        // The ending parenthesis will get added at the last second
        // later on. Yes, this code is gross; I can't think of a
        // better way to write it at the moment :/
    }

    // TODO consider matching on (is_left_side_constructor(&t), t.clone())

    match t.clone() {
        Void => {
            assert!(
                !is_left_side_constructor(&t) && !is_right_side_constructor(&t)
            );
            prefix_prefix.push_str("void");
        }
        Int => {
            assert!(
                !is_left_side_constructor(&t) && !is_right_side_constructor(&t)
            );
            prefix_prefix.push_str("int");
        }
        Char => {
            assert!(
                !is_left_side_constructor(&t) && !is_right_side_constructor(&t)
            );
            prefix_prefix.push_str("char");
        }
        Struct(struct_ref, fields) => {
            assert!(
                !is_left_side_constructor(&t) && !is_right_side_constructor(&t)
            );
            // TODO not so sure about this. How does struct_ref
            // interact with ident?
            prefix_prefix.push_str(&format!("struct {}", struct_ref.ident));
        }
        FuncType(domain, codomain) => {
            assert!(is_right_side_constructor(&t));
            let mut i = 0;
            let length = domain.len();
            suffix.push_str("(");
            for arg_ref in domain {
                suffix.push_str(&emit_decl(arg_ref));
                if i < length - 1 {
                    suffix.push_str(", ");
                }
                i = i + 1;
            }
            suffix.push_str(")");

            return emit_decl_rec(
                *codomain,
                ident.clone(),
                &mut format!("{}{}", prefix_prefix, prefix),
                suffix,
            );
        }
        Pointer(t2) => {
            assert!(is_left_side_constructor(&t));
            prefix_prefix.push_str("*");
            if has_right_side_constructor_child(&t) {
                suffix.push_str(")");
            }
            return emit_decl_rec(
                *t2,
                ident.clone(),
                &mut format!("{}{}", prefix_prefix, prefix),
                suffix,
            );
        }
        ArrayOf(t2, len) => {
            assert!(is_right_side_constructor(&t));
            suffix.push_str(&format!("[{}]", len));
            return emit_decl_rec(
                *t2,
                ident.clone(),
                &mut format!("{}{}", prefix_prefix, prefix),
                suffix,
            );
        }
    };
    return format!("{}{}{}{}", prefix_prefix, prefix, formatted_ident, suffix);
}

fn emit_expr(tabs: usize, e: Expr) -> String {
    match e {
        V(r) => r.ident,
        Unsigned(i) => format!("{}", i),
        Inc(t, e) => format!("{}++", emit_expr(tabs, *e)),
        Dec(t, e) => format!("{}--", emit_expr(tabs, *e)),
        App(t, func_ref, args) => {
            let mut result = format!("{}(", func_ref.ident);
            let mut i = 0;
            let length = args.len();
            for arg in args {
                result.push_str(&emit_expr(tabs, arg));
                if i < length - 1 {
                    result.push_str(", ");
                }
                i = i + 1;
            }
            result.push_str(")");
            result
        }
        Deref(t, expr) => format!("*{}", emit_expr(0, *expr)),
        StructFieldAccess(t, r, ident) => format!("{}.{}", r.ident, ident),
        StructExpr(t, ident, fields) => {
            let mut result = format!("((struct {}) {{", ident);
            let mut i = 0;
            let length = fields.len();
            for field in fields {
                result.push_str(&format!("{}", emit_expr(tabs, field)));
                if i < length - 1 {
                    result.push_str(", ");
                }
                i = i + 1;
            }
            result.push_str(&format!("}})"));
            result
        }
    }
}

fn emit_stmt(tabs: usize, s: Stmt) -> String {
    match s {
        Assign(lhs_ref, rhs) => {
            format_line(
                tabs,
                format!("{} = {};", lhs_ref.ident, emit_expr(0, rhs)),
            )
        }
        Conditional(cond, true_stmts, false_stmts) => {
            let mut result =
                format_line(tabs, format!("if ({}) {{", emit_expr(0, cond)));
            for true_stmt in true_stmts {
                result.push_str(&emit_stmt(tabs + 1, true_stmt));
            }
            result.push_str(&format_line(tabs, "} else {".to_string()));
            for false_stmt in false_stmts {
                result.push_str(&emit_stmt(tabs + 1, false_stmt));
            }
            result.push_str(&format_line(tabs, "}".to_string()));
            result
        }
        StandaloneExpr(e) => {
            format_line(tabs, format!("{};", emit_expr(tabs, e)))
        }
        Return(e) => {
            format_line(tabs, format!("return {};", emit_expr(tabs, e)))
        }
        VarDecl(r) => format_line(tabs, emit_decl(r)),
    }
}

fn emit_top(top: Top) -> String {
    match top {
        StructDef(struct_ref, field_refs) => {
            let mut result = format!("struct {} {{\n", struct_ref.ident);
            for field in field_refs {
                result.push_str(&format_line(1, emit_decl(field)));
            }
            result.push_str("}}\n\n");
            result
        }
        StructDecl(struct_ref) => format!("struct {};\n", struct_ref.ident),
        FuncDef(func_ref, arg_refs, body) => {
            if let FuncType(d, c) = func_ref.t.clone() {
                let mut result = emit_decl(func_ref);
                result.push_str(" {\n");
                for stmt in body {
                    result.push_str(&emit_stmt(1, stmt));
                }
                result.push_str("}\n\n");
                result
            } else {
                assert!(false);
                format!("")
            }
        }
    }
}

pub fn emit_program(t: Program) -> String {
    let mut result = format!("");
    for top in t.tops {
        result.push_str(&emit_top(top));
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    // run 'cargo test -- --nocapture' to see the output of printlns.

    #[test]
    fn test_emit_decl() {
        assert!(
            emit_decl(Ref {
                t: Int,
                ident: format!("foo"),
            }) == format!("int foo")
        );
        assert!(
            emit_decl(Ref {
                t: Pointer(Box::new(Int)),
                ident: format!("bar"),
            }) == format!("int* bar")
        );
        assert!(
            emit_decl(Ref {
                t: Pointer(Box::new(Pointer(Box::new(Struct(
                    Box::new(Ref {
                        t: Void,
                        ident: "baz".to_string(),
                    }),
                    vec![],
                ))))),
                ident: format!("bar"),
            }) == format!("struct baz** bar")
        );
        assert!(
            emit_decl(Ref {
                t: ArrayOf(Box::new(Int), 3),
                ident: format!("qux"),
            }) == format!("int qux[3]")
        );
        assert!(
            emit_decl(Ref {
                t: FuncType(vec![], Box::new(Int)),
                ident: format!("func"),
            }) == format!("int func()")
        );
        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                    ],
                    Box::new(Int),
                ),
                ident: format!("func"),
            }) == format!("int func(int x)")
        );
        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                    ],
                    Box::new(Pointer(Box::new(Int))),
                ),
                ident: format!("func"),
            }) == format!("int* func(int x)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                    ],
                    Box::new(Pointer(Box::new(FuncType(
                        vec![
                            Ref {
                                t: Int,
                                ident: format!("y"),
                            },
                        ],
                        Box::new(Int),
                    )))),
                ),
                ident: format!("func"),
            }) == format!("int(*func(int x))(int y)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                    ],
                    Box::new(Pointer(Box::new(FuncType(
                        vec![
                            Ref {
                                t: Int,
                                ident: format!(""),
                            },
                        ],
                        Box::new(Int),
                    )))),
                ),
                ident: format!("func"),
            }) == format!("int(*func(int x))(int)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                        Ref {
                            t: Int,
                            ident: format!("y"),
                        },
                    ],
                    Box::new(Pointer(Box::new(FuncType(
                        vec![
                            Ref {
                                t: Int,
                                ident: format!(""),
                            },
                        ],
                        Box::new(Int),
                    )))),
                ),
                ident: format!("func"),
            }) == format!("int(*func(int x, int y))(int)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                        Ref {
                            t: Char,
                            ident: format!("y"),
                        },
                    ],
                    Box::new(Pointer(Box::new(FuncType(
                        vec![
                            Ref {
                                t: Int,
                                ident: format!(""),
                            },
                        ],
                        Box::new(Int),
                    )))),
                ),
                ident: format!("func"),
            }) == format!("int(*func(int x, char y))(int)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                        Ref {
                            t: Char,
                            ident: format!("y"),
                        },
                    ],
                    Box::new(Pointer(Box::new(Int))),
                ),
                ident: format!("func"),
            }) == format!("int* func(int x, char y)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                        Ref {
                            t: Char,
                            ident: format!("y"),
                        },
                    ],
                    Box::new(Pointer(Box::new(Int))),
                ),
                ident: format!("func"),
            }) == format!("int* func(int x, char y)")
        );

        assert!(
            emit_decl(Ref {
                t: FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                    ],
                    Box::new(Pointer(Box::new(Int))),
                ),
                ident: format!("func"),
            }) == format!("int* func(int x)")
        );

        assert!(
            emit_decl(Ref {
                t: Pointer(Box::new(FuncType(
                    vec![
                        Ref {
                            t: Int,
                            ident: format!("x"),
                        },
                    ],
                    Box::new(Pointer(Box::new(Int))),
                ))),
                ident: format!("foo"),
            }) == format!("int*(*foo)(int x)")
        );

        assert!(
            emit_decl(Ref {
                t: Pointer(Box::new(FuncType(
                    vec![
                        Ref {
                            t: Pointer(Box::new(Char)),
                            ident: format!("y"),
                        },
                    ],
                    Box::new(Pointer(Box::new(ArrayOf(Box::new(Int), 3)))),
                ))),
                ident: format!("foo"),
            }) == format!("int(*(*foo)(char* y))[3]")
        );

        assert!(
            emit_decl(Ref {
                t: ArrayOf(Box::new(Pointer(
                    Box::new(FuncType(vec![],
                                      Box::new(Pointer(Box::new(
                                          ArrayOf(Box::new(Char), 5)))))
                ))), 3),
                ident: format!("x"),
            }) == format!("char(*(*x[3])())[5]")
        );

        assert!(
            emit_decl(Ref {
                t: Pointer(Box::new(
                    FuncType(vec![
                        Ref { t: Pointer(Box::new(Void)), ident: format!("") }],
                             Box::new(Pointer(Box::new(ArrayOf(
                                 Box::new(Int), 3))))))),
                ident: format!("foo"),
            }) == format!("int(*(*foo)(void*))[3]")
        );

    }
}
