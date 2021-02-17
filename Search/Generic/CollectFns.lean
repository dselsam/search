/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Lean.Compiler.IR
import Search.Inspect.Basic
import Std.Data.HashSet
import Std.Data.HashMap

namespace Search
namespace CollectFns

open Lean
open Search.Inspect (Object)

open Std (HashSet HashMap)

structure Context where
  env      : Environment
  str2name : HashMap String Name

structure State where
  todo    : Array Name   := #[]
  visited : HashSet Name := {}

abbrev CollectFnsM := ReaderT Context (StateRefT State IO)

def pushFn (f : Name) : CollectFnsM Unit := do
  modify λ s => { s with todo := s.todo.push f }

def visitExpr (e : IR.Expr) : CollectFnsM Unit := do
  match e with
  | IR.Expr.fap f ys       => pushFn f
  | IR.Expr.pap f ys       => pushFn f
  | _                      => pure ()

partial def visitFnBody (body : IR.FnBody) : CollectFnsM Unit := do
  match body with
  | IR.FnBody.vdecl x ty e b    => visitExpr e
  | IR.FnBody.jdecl j xs v b    => visitFnBody b
  | IR.FnBody.set x i y b       => visitFnBody b
  | IR.FnBody.setTag x cidx b   => visitFnBody b
  | IR.FnBody.uset x i y b      => visitFnBody b
  | IR.FnBody.sset x i _ y ty b => visitFnBody b
  | IR.FnBody.inc x n c _ b     => visitFnBody b
  | IR.FnBody.dec x n c _ b     => visitFnBody b
  | IR.FnBody.del x b           => visitFnBody b
  | IR.FnBody.mdata d b         => visitFnBody b
  | IR.FnBody.case tid x _ cs   => pure ()
  | IR.FnBody.ret x             => pure ()
  | IR.FnBody.jmp j ys          => pure ()
  | IR.FnBody.unreachable       => pure ()

def visitFn (f : Name) : CollectFnsM Unit := do
  if (← get).visited.contains f then return ()
  match IR.findEnvDecl (← read).env f with
  | none   => println! "[collectFns] WARNING: decl {f} not found"
  | some d => do
    match d with
    | IR.Decl.fdecl  _ xs type body _ => visitFnBody body
    | IR.Decl.extern f xs type ext    => pure ()
    modify λ s => { s with visited := s.visited.insert f }

def visitFnString (f : String) : CollectFnsM Unit := do
  match (← read).str2name.find? f with
  | none   => println! "[collectFns] WARNING: mangled name {f} not found"
  | some f => visitFn f

partial def processTodo : CollectFnsM Unit := do
  if (← get).todo.isEmpty then return ()
  let f := (← get).todo.back
  modify λ s => { s with todo := s.todo.pop }
  visitFn f
  processTodo

partial def visitObject (x : Object) : CollectFnsM Unit := do
  match x with
  | Inspect.Object.scalar _                  => pure ()
  | Inspect.Object.ctor _ args               => for arg in args do visitObject arg
  | Inspect.Object.closure _ (some f) _ args => visitFnString f *> for arg in args do visitObject arg
  | Inspect.Object.closure _ none _ args     => for arg in args do visitObject arg
  | Inspect.Object.array elems               => for elem in elems do visitObject elem
  | Inspect.Object.sarray                    => pure ()
  | Inspect.Object.string _                  => pure ()
  | Inspect.Object.unsupported               => pure ()

def collectFns (env : Environment) (str2name : HashMap String Name) (obj : Object) : IO (Array Name) := do
  let act : CollectFnsM (Array Name) := do
    visitObject obj
    processTodo
    pure (← get).visited.toArray
  act { env := env, str2name := str2name } |>.run' {}

end CollectFns

export CollectFns (collectFns)
end Search
