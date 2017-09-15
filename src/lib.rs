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
    FuncPtr(Box<Type>, Box<Type>),
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

// Separate from emit_type because function pointers are weird.
fn emit_ref(r: Ref) -> String {
    match r.t {
        Void => "void".to_string(),
        Int => "int".to_string(),
        Struct(struct_ref, fields) => "TODO".to_string(),
        FuncPtr(domain, codomain) => "TODO".to_string(),
    }
}


fn emit_type(tabs: usize, t: Type) -> String {
    match t {
        Void => "void".to_string(),
        Int => "int".to_string(),
        Struct(struct_ref, fields) => "TODO".to_string(),
        FuncPtr(domain, codomain) => "TODO".to_string(),
    }
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
        Assign(lhs_ref, rhs) => format!("{} = {};", lhs_ref.ident, emit_expr(tabs, rhs)),
        Conditional(cond, true_stmts, false_stmts) => "TODO".to_string(),
        StandaloneExpr(e) => format!("{};", emit_expr(tabs, e)),
        Return(e) => "TODO".to_string(),
        VarDecl(r) => "TODO".to_string(),
    }
}

fn emit_top(top: Top) -> String {
    match top {
        StructDef(struct_ref, field_refs) => {
            let mut result = format!("struct {} {{\n", struct_ref.ident);
            for field in field_refs {
                result.push_str(&format_line(1, emit_ref(field)));
            }
            result.push_str("}}\n\n");
            result
        }
        StructDecl(struct_ref) => "TODO".to_string(),
        Func(func_ref, arg_refs, body) => "TODO".to_string(),
    }
}

pub fn emit_program(t: Program) -> String {
    "TODO".to_string()
}
