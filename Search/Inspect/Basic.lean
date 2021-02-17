/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
namespace Search
namespace Inspect

inductive Object : Type
  | ctor        : Nat → Array Object → Object
  | closure     : (fileName symbolName : Option String) → Nat → Array Object → Object
  | scalar      : Nat → Object
  | unsupported : Object /- TODO(dselsam): other kinds -/
  deriving Repr, Inhabited, BEq

@[extern "lean_inspect"]
constant inspect (thing : PNonScalar) : IO Object

end Inspect

export Inspect (inspect)
end Search
