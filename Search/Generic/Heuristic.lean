/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Search.Heuristic
import Search.Generic.CollectFns
import Search.Inspect.Basic
import Std.Data.HashMap

namespace Search
namespace Generic

open Lean
open Std (HashMap mkHashMap)

private def buildStr2Name (env : Environment) : HashMap String Name := do
  (Lean.IR.declMapExt.getState env).fold (init := mkHashMap) λ str2name name _ =>
    str2name.insert name.mangle name

variable {m : Type → Type} [Monad m] [MonadLiftT IO m]
variable {α : Type}

unsafe def mkHeuristic (env : Environment) : Heuristic m α := do
  let str2name := buildStr2Name env
  let score (choices : Array α) : m Scores := do
    println! "[choicepoint] {choices.size} {str2name.size}"
    for choice in choices do
      let thing ← liftM $ inspect (unsafeCast choice : PNonScalar)
      let names ← collectFns env str2name thing
      println! "  [choice] {names}"
    return { policy := choices.map λ _ => 1.0, value := 0.5 }
  pure ⟨score⟩

end Generic

end Search
