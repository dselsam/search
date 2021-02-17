/-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Search.Transform.Classic
import Search.SaveRestore
import Search.Heuristic
import Search.Generic.Heuristic
import Search.Algorithm.BestFirst
import Lean

namespace Search
namespace NTactic

open Lean Lean.Meta Lean.Elab Lean.Elab.Term Lean.Elab.Tactic

structure BState where
  mctx  : MetavarContext
  goals : List MVarId

def saveBState : TacticM BState := do
  pure { mctx := (← getMCtx), goals := (← get).goals }

def BState.restore (b : BState) : TacticM Unit := do
  setMCtx b.mctx
  modify fun s => { s with goals := b.goals }

deriving instance Inhabited for BState

instance : SaveRestore TacticM BState where
  save    := saveBState
  restore := BState.restore

abbrev NTacticM := SearchT TacticM

syntax (name := search) "search " ident : tactic

@[tactic search] unsafe def evalSearch : Tactic := fun stx => do
  let (g, gs) ← Tactic.getMainGoal
  if !gs.isEmpty then throwError s!"`search` expecting only a single goal"
  let n : Name ← resolveGlobalConstNoOverload stx[1].getId
  match (← getEnv).evalConst (NTacticM Unit) {} n with
  | Except.error err => throwError err
  | Except.ok ntac   =>
    let ϕ : Heuristic TacticM (NTacticM Unit) := Generic.mkHeuristic (← getEnv)
    match (← bestFirstRestoring ϕ ntac) with
    | none        => throwError s!"search {n} failed to solve"
    | some (s, _) => restore s

end NTactic
end Search
