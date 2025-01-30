open Core;;
open Core_bench;;

module BQ = struct
  let rec gen_list_bool_faster  ~size ~random  = 
    let n = size in
    if n <= 0 then []
    else 
      let x = Splittable_random.bool random in
      let xs = gen_list_bool_faster ~size:(size - 1) ~random in
      x::xs

  let rec gen_list_bool_faster_dropin  ~size ~random  = 
    let n = size in
    if n <= 0 then []
    else 
      let x = Unboxed_splitmix.DropIn.bool random in
      let xs = gen_list_bool_faster_dropin ~size:(size - 1) ~random in
      x::xs

end

module JoeSplitmix = struct
  let rec gen_list_bool_faster ~(size @ local) ~(random)  = 
    if size <= 0 then []
    else 
      let x = Unboxed_splitmix.bool random in
      x::(gen_list_bool_faster ~size:(size - 1) ~random)

    end

let sizes = [10;50;100;1000;10000]

let () = Bench.bench
  ~run_config:(Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 5000) ())
  [
    Bench.Test.create ~name:"system-random" (
      Random.init 55;
      fun () -> ignore (Random.int64 100L)
    );
    Bench.Test.create ~name:"splittable-random" (
      let st = Splittable_random.of_int 55 in
      fun () -> ignore (Splittable_random.int64 st ~lo:0L ~hi:100L)
    );
    Bench.Test.create ~name:"unboxed-splitmix" (
      let st = Unboxed_splitmix.of_int 55 in
      fun () -> ignore (Unboxed_splitmix.int64u st ~lo:#0L ~hi:#100L)
    )
  ]

let () = Bench.bench 
  ~run_config:(Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 5) ())
  [
  (* Bench.Test.create_indexed ~name:"bq-gen-list-basic" ~args:sizes (
    fun n -> Staged.stage @@ 
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> Quickcheck.Generator.generate BQ.gen_list_bool ~random ~size:n
  ); *)
  (* Bench.Test.create_indexed ~name:"bq-gen-list-fast" ~args:sizes (
    fun n -> Staged.stage @@ 
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> Quickcheck.Generator.generate BQ.gen_list_bool_fast ~random ~size:n
  ); *)
  Bench.Test.create_indexed ~name:"bq-gen-list-faster" ~args:sizes (
    fun n -> Staged.stage @@
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> BQ.gen_list_bool_faster ~size:n ~random
  );
  (* Bench.Test.create_indexed ~name:"bq-gen-list-faster-det" ~args:sizes (
    fun n -> Staged.stage @@
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> BQ.gen_list_bool_faster_det ~size:n ~random
  ); *)
  (* Bench.Test.create_indexed ~name:"bq-gen-list-trmc" ~args:sizes (
    fun n -> Staged.stage @@
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> BQ.gen_list_bool_trmc ~size:n ~random
  ); *)
  (* Bench.Test.create_indexed ~name:"bq-gen-list-imp" ~args:sizes (
    fun n -> Staged.stage @@
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> BQ.gen_list_bool_imp ~size:n ~random
  );
  Bench.Test.create_indexed ~name:"qc-gen-list-basic" ~args:sizes (
    fun n -> Staged.stage @@ 
    let g = QC.gen_list_bool n in
    fun () -> (QCheck2.Gen.generate1 g)
  ); *)
  Bench.Test.create_indexed ~name:"sm-gen-list-faster" ~args:sizes (
    fun n -> Staged.stage @@ 
    let u = Random.State.make_self_init () in
    let s = Unboxed_splitmix.of_int (Random.State.int u Int.max_value) in
    fun () -> (JoeSplitmix.gen_list_bool_faster ~size:n ~random:s)
  );


  Bench.Test.create_indexed ~name:"bq-gen-list-faster-dropin" ~args:sizes (
    fun n -> Staged.stage @@
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> BQ.gen_list_bool_faster_dropin ~size:n ~random
  );



  (* Bench.Test.create_indexed ~name:"sm-gen-list-imp" ~args:sizes (
    fun n -> Staged.stage @@ 
    let u = Random.State.make_self_init () in
    let s = Unboxed_splitmix.of_int (Random.State.int u Int.max_value) in
    fun () -> (JoeSplitmix.gen_list_bool_imp ~size:n ~random:s)
  ); *)
]

(* let () = Bench.bench 
  ~run_config:(Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 30) ())
  [
  Bench.Test.create_indexed ~name:"rbt-staged" ~args:sizes (
    fun n -> Staged.stage @@ 
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> (RBTStaged.gt ~size:n ~random:random)
  );
  Bench.Test.create_indexed ~name:"rbt-staged-fast-int" ~args:sizes (
    fun n -> Staged.stage @@ 
    let random = Splittable_random.create (Random.State.make_self_init ()) in
    fun () -> (RBTStagedFastInt.gt ~size:n ~random:random)
  );

  Bench.Test.create_indexed ~name:"rbt-staged-fast-int-direct" ~args:sizes (
    fun n -> Staged.stage @@ 
    let u = Random.State.make_self_init () in
    let random = Unboxed_splitmix.of_int (Random.State.int u Int.max_value) in
    fun () -> (RBTStagedFastIntDirect.gt ~size:n ~random:random)
  );
  ] *)