/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
namespace Search
namespace Inspect

inductive Object : Type
  | scalar      : Nat → Object
  | ctor        : Nat → Array Object → Object
  | closure     : (fileName symbolName : Option String) → Nat → Array Object → Object
  | array       : Array Object → Object
  | sarray      : /- Array Object → -/ Object -- TODO: what are these?
  | string      : String → Object
  | unsupported : Object /- TODO(dselsam): other kinds -/
  deriving Repr, Inhabited, BEq

partial def Object.toCompactString : Object → String
  | scalar n                     => s!"{n}"
  | ctor n args                  => s!"T{n}({arrayToString args})"
  | closure  _ _ arity args      => s!"C{arity}({arrayToString args})"
  | array elems                  => s!"A({arrayToString elems})"
  | sarray                       => s!"X"
  | string s                     => s!"S"
  | unsupported                  => s!"U"
  where
    arrayToString args := @ToString.toString _ (@Array.instToStringArray _ ⟨toCompactString⟩) args

@[extern "lean_inspect"]
constant inspect (thing : PNonScalar) : IO Object

end Inspect

export Inspect (inspect)
end Search
