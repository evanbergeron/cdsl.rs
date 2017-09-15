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
    Struct(Box<Ref> /* name */, Vec<Ref> /* fields */),
    FuncPtr(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    V(Ref),
    Unsigned(usize),
    Inc(Type, Box<Expr>),
    Dec(Type, Box<Expr>),
    App(Ref, Vec<Expr>),
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
    Func(Ref, Vec<Ref>, Vec<Stmt>),
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

fn emit_decl(t: Type, ident: Option<String>) -> String {
    emit_decl_rec(t, ident, &mut format!(""), &mut format!(""))
}

fn emit_decl_rec(
    t: Type,
    ident: Option<String>,
    prefix: &mut String,
    suffix: &mut String,
) -> String {
    // The prefix of the prefix, as you might expect.
    let mut prefix_prefix = format!("");
    let result_ident: String = match ident {
        Some(name) => name,
        None => format!(""),
    };
    match t {
        Void => {
            prefix_prefix.push_str("void ");
        }
        Int => {
            prefix_prefix.push_str("int ");
        }
        Struct(struct_ref, fields) => {
            prefix_prefix.push_str(&format!("struct {}", struct_ref.ident));
        }
        FuncPtr(domain, codomain) => {
            prefix_prefix.push_str(&emit_decl(*codomain, None));
            let mut i = 0;
            let length = domain.len();
            suffix.push_str("(");
            for arg_type in domain {
                suffix.push_str(&emit_decl(arg_type, None));
                if i < length {
                    suffix.push_str(",");
                }
                i = i + 1;
            }
            suffix.push_str(")");
        }
    };
    return format!("{}{}{}{}", prefix_prefix, prefix, result_ident, suffix);
}


fn emit_expr(tabs: usize, e: Expr) -> String {
    match e {
        V(r) => r.ident,
        Unsigned(i) => format!("{}", i),
        Inc(t, e) => format!("{}++", emit_expr(tabs, *e)),
        Dec(t, e) => format!("{}--", emit_expr(tabs, *e)),
        App(func_ref, args) => {
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
        VarDecl(r) => format_line(tabs, emit_decl(r.t, Some(r.ident))),
    }
}

fn emit_top(top: Top) -> String {
    match top {
        StructDef(struct_ref, field_refs) => {
            let mut result = format!("struct {} {{\n", struct_ref.ident);
            for field in field_refs {
                result.push_str(
                    &format_line(1, emit_decl(field.t, Some(field.ident))),
                );
            }
            result.push_str("}}\n\n");
            result
        }
        StructDecl(struct_ref) => "TODO".to_string(),
        Func(func_ref, arg_refs, body) => "TODO".to_string(),
    }
}

pub fn emit_program(t: Program) -> String {
    let mut result = format!("");
    for top in t.tops {
        result.push_str(&emit_top(top));
    }
    result
}
