/-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-/
namespace Search
namespace Transform
namespace Classic

inductive Status (β α : Type)
  | done   : α → Status β α
  | choice : Array β → Status β α

inductive SearchT (m : Type → Type) (α : Type) : Type
  | mk : m (Status PNonScalar α) → SearchT m α

variable {m : Type → Type} {α : Type}

@[inline] unsafe def Status.toClear   (x : Status PNonScalar α)    : Status (SearchT m α) α := unsafeCast x
@[inline] unsafe def Status.toOpaque  (x : Status (SearchT m α) α) : Status PNonScalar α    := unsafeCast x
@[inline] unsafe def SearchT.toOpaque (x : SearchT m α)            : PNonScalar             := unsafeCast x

variable [Monad m]

instance : Inhabited (SearchT m α)                := ⟨SearchT.mk $ pure (Status.choice #[])⟩
instance : Inhabited (m (Status (SearchT m α) α)) := ⟨pure (Status.choice #[])⟩

instance : Pure (SearchT m) := ⟨λ x => SearchT.mk $ pure (Status.done x)⟩

private unsafe def bindUnsafe {α β : Type} : SearchT m α → (α → SearchT m β) → SearchT m β
  | SearchT.mk comp, ψ₂ =>
    SearchT.mk $ comp >>= λ result =>
      match result.toClear with
      | Status.done x => let (SearchT.mk r) := ψ₂ x; r
      | Status.choice cs => pure $ Status.choice $ cs.map $ λ ψₖ => (bindUnsafe ψₖ ψ₂).toOpaque

@[implementedBy bindUnsafe]
constant bind {α β : Type} (x : SearchT m α) (ψ₂ : α → SearchT m β) : SearchT m β

instance : Bind (SearchT m)  := ⟨bind⟩
instance : Monad (SearchT m) := {}

private unsafe def unpackUnsafe : SearchT m α → m (Status (SearchT m α) α)
  | SearchT.mk comp => do (← comp).toClear

@[implementedBy unpackUnsafe]
constant SearchT.unpack : SearchT m α → m (Status (SearchT m α) α)

private unsafe def choiceMUnsafe (cs : Array (SearchT m α)) : SearchT m α :=
  SearchT.mk $ pure (Status.choice cs).toOpaque

@[implementedBy choiceMUnsafe]
constant choiceM (cs : Array (SearchT m α)) : SearchT m α

def choice (cs : Array α) : SearchT m α := choiceM $ cs.map pure

def deadend : SearchT m α := choice #[]

def lift (x : m α) : SearchT m α :=
  SearchT.mk $ do pure $ Status.done (← x)

instance : MonadLift m (SearchT m) where
  monadLift x := lift x

end Classic
end Transform

export Transform.Classic (Status.done Status.choice SearchT choiceM choice deadend)

end Search
