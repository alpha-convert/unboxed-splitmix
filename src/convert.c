#include<caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim value sm_seed(value sm)
{
    CAMLparam1(sm);
    CAMLreturn(Field(sm,0));
}

CAMLprim value sm_odd_gamma(value sm)
{
    CAMLparam1(sm);
    CAMLreturn(Field(sm,1));
}

CAMLprim value sm_clobber_seed(value sm,value sd)
{
    CAMLparam2(sm,sd);
    Store_field(sm,0,sd);
    CAMLreturn(Val_unit);
}