/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
import Search.Transform.Classic
import Search.Heuristic
import Search.Generic.Heuristic
import Search.Algorithm.BestFirst
import Lean

open Search
open Search.Transform

def SearchPath   : String := "build/"
def Lean4LibPath : String := "/home/dselsam/omega/lean4/build/release/stage1/lib/lean"

def exampleSearchT : SearchT IO (List Bool) := do
  let x₁ : Bool  ← choice #[false, true]
  let x₂ : Bool  ← choiceM (#[false, true].map λ b => pure b)
  let s : String ← choice #["s1", "s2"]
  if x₁ ∨ x₂ then deadend else pure [x₁, x₂]

unsafe def main : IO Unit := do
  println! "Welcome to *search*"

  Lean.initSearchPath s!"{Lean4LibPath}:{SearchPath}"
  let imports : List Lean.Import := [{ module := `Search }]
  Lean.withImportModules imports (opts := {}) (trustLevel := 0) $ λ env => do
    let ϕ : Heuristic IO (SearchT IO (List Bool)) := Generic.mkHeuristic env
    let _ ← bestFirst ϕ exampleSearchT
    pure ()
