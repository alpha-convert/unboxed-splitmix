type t

val of_int : int @ local -> t
val copy : t @ local -> t
val create : Core.Random.State.t -> t
val split : t @ local -> t
val perturb : t @ local -> int -> unit [@@zero_alloc]

val bool : t -> bool @ local
val int64u : t @ local -> lo:int64# -> hi:int64# -> int64#
val floatu : t -> lo:float# -> hi:float# -> float#

val int : t @ local -> lo:int -> hi:int -> int
val int64 : t @ local -> lo:int64 -> hi:int64 -> int64
val float : t -> lo:float -> hi:float -> float

module DropIn : sig
    val bool : Splittable_random.t -> bool
    val int : Splittable_random.t -> lo:int -> hi:int -> int
    val int64 : Splittable_random.t -> lo:int64 -> hi:int64 -> int64
    val float : Splittable_random.t -> lo:float -> hi:float -> float
end