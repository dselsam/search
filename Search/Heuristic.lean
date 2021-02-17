/-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Simple best-first-search enumeration
-/
namespace Search

structure Scores : Type where
  policy : Array Float
  value  : Float
  deriving Repr

structure Heuristic (m : Type → Type) (α : Type) where
  score : Array α → m Scores

end Search
