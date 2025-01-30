#include<caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

Assert_mixed_block_layout_v1;
// #define Foo_t_x(foo) (*(int32_t*)&Field(foo, 0))
// #define Foo_t_y(foo) (*(int32_t*)&Field(foo, 1))


// boxed_to is { seed : Int64.t; odd_gamma : Int64.t }
// unboxed_from is { seed : int64# ; odd_gamma : int64# }
CAMLprim value restore_from(value boxed_to, value unboxed_from){
    CAMLparam2(boxed_to,unboxed_from);
    int64_t seed = *(int64_t*)&Field(unboxed_from,0);
    int64_t odd_gamma = *(int64_t*)&Field(unboxed_from,1);
    Int64_val(Field(boxed_to,0)) = seed;
    CAMLreturn(Val_unit);
}

// CAMLprim value sm_seed(value sm)
// {
//     CAMLparam1(sm);
//     CAMLreturn(Field(sm,0));
// }

// CAMLprim value sm_odd_gamma(value sm)
// {
//     CAMLparam1(sm);
//     CAMLreturn(Field(sm,1));
// }

// CAMLprim value sm_clobber_seed(value sm,value sd)
// {
//     CAMLparam2(sm,sd);
//     Store_field(sm,0,sd);
//     CAMLreturn(Val_unit);
// }