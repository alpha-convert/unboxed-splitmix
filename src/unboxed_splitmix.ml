(*
This is a direct port of the Jane Street Splittable_random, using unboxed types instead of boxed ones.
When we're generating lots of random data in a tight loop, the unboxed int64 arithmetic here makes a big difference!
*)

open Core
module I = Stdlib_upstream_compatible.Int64_u
module F = Stdlib_upstream_compatible.Float_u

(* odd_gamma doesn't technically need to be mutable, but we make it mutable so that the "global state" in DropIn works... *)
type t = { mutable seed : int64#; mutable odd_gamma : int64# }


let of_int seed = { seed = I.of_int seed; odd_gamma = #0x9e37_79b9_7f4a_7c15L}

let copy { seed; odd_gamma } = { seed; odd_gamma }

(* we specialize three different versions of this to ensure statically that the `int` argument to I.shift_right_logical is
never boxed in the library. *)
let [@inline always] mix_bits_33 z = I.logxor z (I.shift_right_logical z 33)
let [@inline always] mix_bits_27 z = I.logxor z (I.shift_right_logical z 27)
let [@inline always] mix_bits_30 z = I.logxor z (I.shift_right_logical z 30)
let [@inline always] mix_bits_31 z = I.logxor z (I.shift_right_logical z 31)

let mix64 z =
  let z = I.mul (mix_bits_33 z) #0xff51_afd7_ed55_8ccdL in
  let z = I.mul (mix_bits_33 z) #0xc4ce_b9fe_1a85_ec53L in
  mix_bits_33 z
;;

let mix64_variant13 z =
  let z = I.mul (mix_bits_30 z) #0xbf58_476d_1ce4_e5b9L in
  let z = I.mul (mix_bits_27 z) #0x94d0_49bb_1331_11ebL in
  mix_bits_31 z
;;

let [@zero_alloc] mix_odd_gamma z =
  let z = I.logor (mix64_variant13 z) #1L in
  (* TODO use immediate popcount here... ocaml intrinsics isn't building on arm64. *)
  let n = Core.Int64.popcount (I.to_int64 (I.logxor z (I.shift_right_logical z 1))) in
  if n < 24 then I.logxor z #0xaaaa_aaaa_aaaa_aaaaL else z

let [@zero_alloc] next_seed t =
  let next = I.add t.seed t.odd_gamma in
  t.seed <- next;
  next
;;

let of_seed_and_gamma ~seed ~gamma =
  let seed = mix64 seed in
  let odd_gamma = mix_odd_gamma gamma in
  { seed; odd_gamma }

let random_int64u random_state =
  I.of_int64 (Core.Random.State.int64_incl random_state Core.Int64.min_value Core.Int64.max_value)

let create random_state =
  let seed = random_int64u random_state in
  let gamma = random_int64u random_state in
  of_seed_and_gamma ~seed ~gamma

let split t =
  let seed = next_seed t in
  let gamma = next_seed t in
  of_seed_and_gamma ~seed ~gamma

let next_int64 t = mix64 (next_seed t)

let [@zero_alloc] perturb t salt =
  let next = I.add t.seed (mix64 (I.of_int salt)) in
  t.seed <- next
;;

let [@zero_alloc] bool (state) = 
  I.equal (I.logand (next_int64 state) #1L) #0L

let [@zero_alloc] remainder_is_unbiased ~draw ~remainder ~draw_maximum ~remainder_maximum =
    I.compare (I.sub draw remainder) (I.sub draw_maximum remainder_maximum) <= 0

let [@zero_alloc] rec between state ~lo ~hi =
    let draw = next_int64 state in
    if I.compare lo draw <= 0 && I.compare draw hi <= 0 then draw else between state ~lo ~hi

let rec non_negative_up_to state maximum =
    let draw = I.logand (next_int64 state) (I.of_int64 Int64.max_value) in
    let remainder = I.rem draw (I.succ maximum) in
    if remainder_is_unbiased
          ~draw
          ~remainder
          ~draw_maximum:(I.of_int64 Int64.max_value)
          ~remainder_maximum:maximum
    then remainder
    else non_negative_up_to state maximum

let [@zero_alloc] int64u (state @local) ~lo ~hi =
    if I.compare lo hi > 0
    then Error.raise (Error.t_of_sexp (Sexplib0.Sexp.message "int64: crossed bounds" ["",Int64.sexp_of_t (I.to_int64 lo);"",Int64.sexp_of_t (I.to_int64 hi)] ));
    (* TODO: fix this roundtrip through Int64. *)
    let i64_max = I.of_int64 (Int64.max_value) in
    let diff = I.sub hi lo in
    if I.equal diff i64_max
    then I.add (I.logand (next_int64 state) i64_max) lo
    else if I.compare diff #0L >= 0
    then I.add (non_negative_up_to state diff)lo
    else between state ~lo ~hi

let double_ulp = 2. **. -53.


(* TODO: fix this roundtrip through boxed float... *)
let unit_floatu_from_int64u int64u = F.of_float (I.to_float (I.shift_right_logical int64u 11) *. double_ulp)

let unit_floatu state = unit_floatu_from_int64u (next_int64 state)

let rec finite_float state ~lo ~hi =
    let range = F.sub hi lo in
    if F.is_finite range
    then F.add lo (F.mul (unit_floatu state) range)
    else (
      let mid = F.div (F.add hi lo) #2. in
      if bool state
      then finite_float state ~lo ~hi:mid
      else finite_float state ~lo:mid ~hi)

let [@zero_alloc] floatu =
  fun (state) ~lo ~hi ->
    if not (F.is_finite lo && F.is_finite hi) then
      Error.raise (Error.t_of_sexp (Sexplib0.Sexp.message "float: bounds are not finite numbers" ["",Float.sexp_of_t (F.to_float lo);"",Float.sexp_of_t (F.to_float hi)] ));
    if F.compare lo hi > 0 then
      Error.raise (Error.t_of_sexp (Sexplib0.Sexp.message "float: bounds are crossed" ["",Float.sexp_of_t (F.to_float lo);"",Float.sexp_of_t (F.to_float hi)] ));
    finite_float state ~lo ~hi
;;

let float =
  fun (state) ~lo ~hi -> F.to_float (floatu state ~lo:(F.of_float lo) ~hi:(F.of_float hi))
;;

let int64 =
  fun (state @ local) ~lo ~hi -> I.to_int64 (int64u state ~lo:(I.of_int64 lo) ~hi:(I.of_int64 hi))
;;

let int state ~lo ~hi =
  I.to_int (int64u state ~lo:(I.of_int lo) ~hi:(I.of_int hi))
;;

(* external sm_seed : Splittable_random.t -> Int64.t = "sm_seed" *)
(* external sm_odd_gamma : Splittable_random.t -> Int64.t = "sm_odd_gamma" *)
(* external clobber_seed : Splittable_random.t -> Int64.t -> unit = "sm_clobber_seed" *)

module DropIn = struct
  (*
    To use unboxed splitmix drop-in replacement for splitmix, we need to
    (1) convert a normal splittable_random.t into an unboxed_splitmix.t
    (2) call the sampling function
    (3) convert the updated unboxed_splitmix.t back into a splittable_random.t

  *)
  
  let global_dropin_seed: t = { seed = I.of_int64 0L; odd_gamma = I.of_int64 0L }

  type srt_repr = { srt_seed : Int64.t; srt_odd_gamma : Int64.t }

  external restore_from : Splittable_random.t -> t -> unit = "restore_from"
  
  let [@zero_alloc] replace_gds (srt : Splittable_random.t) =
    let srt_repr : srt_repr = Obj.magic srt in
    global_dropin_seed.seed <- I.of_int64 srt_repr.srt_seed;
    global_dropin_seed.odd_gamma <- I.of_int64 srt_repr.srt_odd_gamma;
    ()

  let [@zero_alloc assume] restore_from_gds (srt : Splittable_random.t) =
    restore_from srt global_dropin_seed

  let int (sm : Splittable_random.t) ~lo ~hi =
    replace_gds sm;
    let i = int global_dropin_seed ~lo ~hi in
    restore_from_gds sm;
    i

  let [@zero_alloc] bool (sm : Splittable_random.t) : bool =
    replace_gds sm;
    let b = bool global_dropin_seed in
    restore_from_gds sm;
    b

  let float (sm : Splittable_random.t) ~lo ~hi =
    replace_gds sm;
    let f = float global_dropin_seed ~lo ~hi in
    restore_from_gds sm;
    f

  let int64 (sm : Splittable_random.t) ~lo ~hi =
    replace_gds sm;
    let i64 = int64 global_dropin_seed ~lo ~hi in
    restore_from_gds sm;
    i64

end