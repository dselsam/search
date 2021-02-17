/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Search.Inspect.Basic
import Search.NTactic.Basic
import Lean

namespace Search
namespace NTactic

def chooseBoolsDoNothing : NTacticM Unit := do
  Lean.Meta.setMCtx {}
  let x ← choice #[false, true]
  if x ∧ ¬ x then deadend else pure ()

example : ∀ (n : Nat), 2 * n + 1 < 3 * n + 2 := by
  search chooseBoolsDoNothing

end NTactic
end Search
