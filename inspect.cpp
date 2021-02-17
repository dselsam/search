/*
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
*/
#include "lean/object.h"
#include "lean/io.h"
#include <dlfcn.h>

using namespace lean;

enum class object_kind { Scalar, Ctor, Closure, Array, SArray, String, Unsupported };
object_kind object_tag(object * thing) { return static_cast<object_kind>(cnstr_tag(thing)); }

/*
The following function, `lean_inspect`, is extern for `Lean.inspect`:

  @[extern "lean_inspect"]
  constant inspect {α : Type} (thing : α) : IO Inspect.Object

It traverses a runtime object to convert it to a `Lean.Inspect.Object`.
We hardcode constructor tags here, so this implementation must
be kept in sync with the definition of Lean.Inspect.Object:

  inductive Object : Type
    | scalar      : Nat → Object
    | ctor        : Nat → Array Object → Object
    | closure     : (fileName symbolName : Option String) → Nat → Array Object → Object
    | array       : Array Object → Object
    | sarray      : Array Object → Object -- TODO: explicit scalars
    | string      : String → Object
    | ref         : Object → Object
    | thunk       : Object → Object → Object
    | task        : Object → Object
    | unsupported : Object /- TODO(dselsam): other kinds -/
*/
static object * inspect_core(object * thing) {
    if (is_scalar(thing)) {
        // (This check must be first since header-checks will fail on scalars)
        object * result = lean_alloc_ctor(static_cast<unsigned>(object_kind::Scalar), 1, 0);
        // TODO: parse scalar info?
        lean_ctor_set(result, 0, usize_to_nat(lean_scalar_to_int64(thing)));
        return result;
    } else if (is_cnstr(thing)) {
        unsigned tag = cnstr_tag(thing);
        unsigned n = cnstr_num_objs(thing);
        object * args = array_mk_empty();
        for (unsigned i = 0; i < n; ++i) {
            args = array_push(args, inspect_core(cnstr_get(thing, i)));
        }
        object * result = lean_alloc_ctor(static_cast<unsigned>(object_kind::Ctor), 2, 0);
        lean_ctor_set(result, 0, mk_nat_obj(tag));
        lean_ctor_set(result, 1, args);
        return result;
    } else if (is_closure(thing)) {
        unsigned arity = closure_arity(thing);
        unsigned num_fixed = closure_num_fixed(thing);
        object * fixed = array_mk_empty();
        for (unsigned i = 0; i < num_fixed; ++i) {
            fixed = array_push(fixed, inspect_core(closure_get(thing, i)));
        }
        object * result = lean_alloc_ctor(static_cast<unsigned>(object_kind::Closure), 4, 0);

        Dl_info info;
        int dl_result = dladdr(closure_fun(thing), &info);
        if (dl_result && info.dli_fname) {
            lean_ctor_set(result, 0, mk_option_some(mk_string(info.dli_fname)));
        } else {
            lean_ctor_set(result, 0, mk_option_none());
        }

        if (dl_result && info.dli_sname) {
            lean_ctor_set(result, 1, mk_option_some(mk_string(info.dli_sname)));
        } else {
            lean_ctor_set(result, 1, mk_option_none());
        }

        lean_ctor_set(result, 2, mk_nat_obj(arity));
        lean_ctor_set(result, 3, fixed);
        return result;
    } else if (is_array(thing)) {
        object * result = lean_alloc_ctor(static_cast<unsigned>(object_kind::Array), 1, 0);
        object * elems = array_mk_empty();
        for (size_t i = 0; i < array_size(thing); ++i) {
            elems = array_push(elems, inspect_core(array_get(thing, i)));
        }
        lean_ctor_set(result, 0, elems);
        return result;
    } else if (is_sarray(thing)) {
        std::cout << "[inspect] sarray" << std::endl;
        object * result = lean_alloc_ctor(static_cast<unsigned>(object_kind::SArray), 0, 0);
        return result;
    } else if (is_string(thing)) {
        object * result = lean_alloc_ctor(static_cast<unsigned>(object_kind::String), 1, 0);
        lean_ctor_set(result, 0, thing);
        return result;
    } else {
        std::cout << "[inspect] unsupported" << std::endl;
        return lean_alloc_ctor(static_cast<unsigned>(object_kind::Unsupported), 0, 0);
    }
}

extern "C" object * lean_inspect(object * thing, object * /* world */) {
    object * result = inspect_core(thing);
    return io_result_mk_ok(result);
}
