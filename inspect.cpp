/*
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
*/
#include "lean/object.h"
#include "lean/io.h"
#include <dlfcn.h>

using namespace lean;

/*
The following function, `lean_inspect`, is extern for `Lean.inspect`:

  @[extern "lean_inspect"]
  constant inspect {α : Type} (thing : α) : IO Inspect.Object

It traverses a runtime object to convert it to a `Lean.Inspect.Object`.
We hardcode constructor tags here, so this implementation must
be kept in sync with the definition of Lean.Inspect.Object:

  inductive Object : Type
    | ctor        : Nat → Array Object → Object
    | closure     : (fileName symbolName : Option String) → Nat → Array Object → Object
    | scalar      : Nat → Object
    | unsupported : Object
*/

static object * inspect_core(object * thing) {
    std::cout << "[inspect] " << thing << std::endl;
    if (is_scalar(thing)) {
        std::cout << "[inspect] scalar" << std::endl;
        // (This check must be first since header-checks will fail on scalars)
        // Object.scalar
        object * result = lean_alloc_ctor(2, 1, 0);
        // TODO: parse scalar info?
        lean_ctor_set(result, 0, usize_to_nat((usize) thing));
        return result;
    } else if (is_cnstr(thing)) {
        std::cout << "[inspect] cnstr" << std::endl;
        unsigned tag = cnstr_tag(thing);
        unsigned n = cnstr_num_objs(thing);
        object * args = array_mk_empty();
        for (unsigned i = 0; i < n; ++i) {
            args = array_push(args, inspect_core(cnstr_get(thing, i)));
        }
        // Object.ctor
        object * result = lean_alloc_ctor(0, 2, 0);
        lean_ctor_set(result, 0, mk_nat_obj(tag));
        lean_ctor_set(result, 1, args);
        return result;
    } else if (is_closure(thing)) {
        std::cout << "[inspect] closure" << std::endl;
        unsigned arity = closure_arity(thing);
        unsigned num_fixed = closure_num_fixed(thing);
        object * fixed = array_mk_empty();
        for (unsigned i = 0; i < num_fixed; ++i) {
            fixed = array_push(fixed, inspect_core(closure_get(thing, i)));
        }
        // Object.closure
        object * result = lean_alloc_ctor(1, 4, 0);

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
    } else {
        std::cout << "[inspect] unsupported" << std::endl;
        // TODO(dselsam): support other kinds
        // Object.unsupported
        return lean_alloc_ctor(3, 0, 0);
    }
}

extern "C" object * lean_inspect(object * thing, object * /* world */) {
    object * result = inspect_core(thing);
    return io_result_mk_ok(result);
}
