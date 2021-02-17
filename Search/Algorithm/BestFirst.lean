/-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Search.Transform.Classic
import Search.Heuristic
import Search.SaveRestore

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

variable {σ : Type} [Inhabited σ] [SaveRestore m σ]

def bestFirstRestoring (ϕ : Heuristic m (SearchT m α)) (ψ : SearchT m α) (fuel : Nat := 1000) : m (Option (σ × α)) := do
  let mut todo : Array (σ × SearchT m α) := #[(← save, ψ)]

  for _ in [:fuel] do
    if todo.isEmpty then return none
    let (s, ψ) := todo.back
    todo := todo.pop
    match ← (restore s *> ψ.unpack) with
    | Status.done x    => return some (← save, x)
    | Status.choice ψs =>
      let s ← save
      let scores ← ϕ.score (ψs.map λ ψ => ((liftM (restore s : m Unit) : SearchT m Unit) *> ψ))
      println! "  [scores] {repr scores}"
      -- TODO: insert into PQ (for now just want to collect funs)
      todo := todo ++ ψs.reverse.map λ ψ => (s, ψ)

  return none

end BestFirst
end Algorithm

export Algorithm.BestFirst (bestFirst bestFirstRestoring)

end Search
