/-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Search.Transform.Classic
import Search.Heuristic

namespace Search
namespace Algorithm
namespace BestFirst

variable {m : Type → Type} {α : Type}
variable [Inhabited α] [Monad m] [MonadLiftT IO m]

def bestFirst (ϕ : Heuristic m (SearchT m α)) (ψ : SearchT m α) (fuel : Nat := 1000) : m (Option α) := do
  let mut todo : Array (SearchT m α) := #[ψ]

  for _ in [:fuel] do
    if todo.isEmpty then return none
    let ψ := todo.back
    todo := todo.pop
    match (← ψ.unpack) with
    | Status.done x    => return some x
    | Status.choice ψs =>
      let scores ← ϕ.score ψs
      println! "  [scores] {repr scores}"
      -- TODO: insert into PQ (for now just want to collect funs)
      todo := todo ++ ψs.reverse

  return none

end BestFirst
end Algorithm

export Algorithm.BestFirst (bestFirst)

end Search
