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

fn emitType(t: Type) -> String {
    match t {
        Void => "void".to_string(),
        Int => "int".to_string(),
        Struct(structRef, fields) => "TODO".to_string(),
        FuncPtr(domain, codomain) => "TODO".to_string(),
    }
}

fn emitExpr(e: Expr) -> String {
    match e {
        V(r) => r.ident,
        Unsigned(i) => format!("{}", i),
        Inc(t, e) => format!("{}++", emitExpr(*e)),
        Dec(t, e) => format!("{}--", emitExpr(*e)),
        App(funcRef, argExprs) => "TODO".to_string(),
        StructFieldAccess(t, r, ident) => "TODO".to_string(),
        StructExpr(t, ident, structFieldExprs) => "TODO".to_string(),
    }
}

fn emitStmt(s: Stmt) -> String {
    match s {
        Assign(lhsRef, rhs) => "TODO".to_string(),
        Conditional(cond, trueStmts, falseStmts) => "TODO".to_string(),
        StandaloneExpr(e) => format!("{};", emitExpr(e)),
        Return(e) => "TODO".to_string(),
        VarDecl(r) => "TODO".to_string(),
    }
}

fn emitTop(top: Top) -> String {
    match top {
        StructDef(structRef, fieldRefs) => "TODO".to_string(),
        StructDecl(structRef) => "TODO".to_string(),
        Func(funcRef, argRefs, body) => "TODO".to_string(),
    }
}

pub fn emitProgram(t: Program) -> String {
    "TODO".to_string()
}
