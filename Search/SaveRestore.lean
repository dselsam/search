/-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
namespace Search

class SaveRestore (m : Type → Type) (σ : outParam Type) where
  save    : m σ
  restore : σ → m Unit

instance {σ : Type} : SaveRestore (StateM σ) σ := {
  save    := get,
  restore := set
}

export SaveRestore (save restore)

end Search
