type t

val of_int : int @ local -> t
val copy : t @ local -> t
val create : Core.Random.State.t -> t
val split : t @ local -> t
val perturb : t @ local -> int -> unit

val bool : t @ local -> bool @ local [@@zero_alloc]
val int64u : t @ local -> lo:int64# -> hi:int64# -> int64# [@@zero_alloc]
val floatu : t @ local -> lo:float# -> hi:float# -> float# [@@zero_alloc]