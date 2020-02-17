open Base
open Higher_kinded_intf

type ('a, 'witness) t
type ('a, 'b, 'witness) t2 = ('a, ('b, 'witness) t) t
type ('a, 'b, 'c, 'witness) t3 = ('a, 'b, ('c, 'witness) t) t2
type ('a, 'b, 'c, 'd, 'witness) t4 = ('a, 'b, 'c, ('d, 'witness) t) t3
type ('a, 'b, 'c, 'd, 'e, 'witness) t5 = ('a, 'b, 'c, 'd, ('e, 'witness) t) t4
type ('a, 'b, 'c, 'd, 'e, 'f, 'witness) t6 = ('a, 'b, 'c, 'd, 'e, ('f, 'witness) t) t5

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'witness) t7 =
  ('a, 'b, 'c, 'd, 'e, 'f, ('g, 'witness) t) t6

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'witness) t8 =
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, ('h, 'witness) t) t7

module type S = S with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S2 = S2 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S3 = S3 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S4 = S4 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S5 = S5 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S6 = S6 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S7 = S7 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
module type S8 = S8 with type ('a, 'witness) higher_kinded := ('a, 'witness) t

module Make (X : T1) : S with type 'a t := 'a X.t = struct
  type witness1
  type 'a witness = ('a, witness1) t

  external inject : 'a X.t -> 'a witness = "%identity"
  external project : 'a witness -> 'a X.t = "%identity"
end

module Make2 (X : T2) : S2 with type ('a, 'z) t := ('a, 'z) X.t = struct
  type witness2
  type 'z witness1 = ('z, witness2) t
  type ('a, 'z) witness = ('a, 'z witness1) t

  external inject : ('a, 'z) X.t -> ('a, 'z) witness = "%identity"
  external project : ('a, 'z) witness -> ('a, 'z) X.t = "%identity"
end

module Make3 (X : T3) : S3 with type ('a, 'y, 'z) t := ('a, 'y, 'z) X.t = struct
  type witness3
  type 'z witness2 = ('z, witness3) t
  type ('y, 'z) witness1 = ('y, 'z witness2) t
  type ('a, 'y, 'z) witness = ('a, ('y, 'z) witness1) t

  external inject : ('a, 'y, 'z) X.t -> ('a, 'y, 'z) witness = "%identity"
  external project : ('a, 'y, 'z) witness -> ('a, 'y, 'z) X.t = "%identity"
end

module Make4 (X : sig
    type (_, _, _, _) t
  end) : S4 with type ('a, 'x, 'y, 'z) t := ('a, 'x, 'y, 'z) X.t = struct
  type witness4
  type 'z witness3 = ('z, witness4) t
  type ('y, 'z) witness2 = ('y, 'z witness3) t
  type ('x, 'y, 'z) witness1 = ('x, ('y, 'z) witness2) t
  type ('a, 'x, 'y, 'z) witness = ('a, ('x, 'y, 'z) witness1) t

  external inject : ('a, 'x, 'y, 'z) X.t -> ('a, 'x, 'y, 'z) witness = "%identity"
  external project : ('a, 'x, 'y, 'z) witness -> ('a, 'x, 'y, 'z) X.t = "%identity"
end

module Make5 (X : sig
    type (_, _, _, _, _) t
  end) : S5 with type ('a, 'w, 'x, 'y, 'z) t := ('a, 'w, 'x, 'y, 'z) X.t = struct
  type witness5
  type 'z witness4 = ('z, witness5) t
  type ('y, 'z) witness3 = ('y, 'z witness4) t
  type ('x, 'y, 'z) witness2 = ('x, ('y, 'z) witness3) t
  type ('a, 'x, 'y, 'z) witness1 = ('a, ('x, 'y, 'z) witness2) t
  type ('a, 'w, 'x, 'y, 'z) witness = ('a, ('w, 'x, 'y, 'z) witness1) t

  external inject
    :  ('a, 'w, 'x, 'y, 'z) X.t
    -> ('a, 'w, 'x, 'y, 'z) witness
    = "%identity"

  external project
    :  ('a, 'w, 'x, 'y, 'z) witness
    -> ('a, 'w, 'x, 'y, 'z) X.t
    = "%identity"
end

module Make6 (X : sig
    type (_, _, _, _, _, _) t
  end) : S6 with type ('a, 'v, 'w, 'x, 'y, 'z) t := ('a, 'v, 'w, 'x, 'y, 'z) X.t = struct
  type witness6
  type 'z witness5 = ('z, witness6) t
  type ('y, 'z) witness4 = ('y, 'z witness5) t
  type ('x, 'y, 'z) witness3 = ('x, ('y, 'z) witness4) t
  type ('a, 'x, 'y, 'z) witness2 = ('a, ('x, 'y, 'z) witness3) t
  type ('a, 'w, 'x, 'y, 'z) witness1 = ('a, ('w, 'x, 'y, 'z) witness2) t
  type ('a, 'v, 'w, 'x, 'y, 'z) witness = ('a, ('v, 'w, 'x, 'y, 'z) witness1) t

  external inject
    :  ('a, 'v, 'w, 'x, 'y, 'z) X.t
    -> ('a, 'v, 'w, 'x, 'y, 'z) witness
    = "%identity"

  external project
    :  ('a, 'v, 'w, 'x, 'y, 'z) witness
    -> ('a, 'v, 'w, 'x, 'y, 'z) X.t
    = "%identity"
end

module Make7 (X : sig
    type (_, _, _, _, _, _, _) t
  end) : S7 with type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t =
struct
  type witness7
  type 'z witness6 = ('z, witness7) t
  type ('y, 'z) witness5 = ('y, 'z witness6) t
  type ('x, 'y, 'z) witness4 = ('x, ('y, 'z) witness5) t
  type ('a, 'x, 'y, 'z) witness3 = ('a, ('x, 'y, 'z) witness4) t
  type ('a, 'w, 'x, 'y, 'z) witness2 = ('a, ('w, 'x, 'y, 'z) witness3) t
  type ('a, 'v, 'w, 'x, 'y, 'z) witness1 = ('a, ('v, 'w, 'x, 'y, 'z) witness2) t
  type ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness = ('a, ('u, 'v, 'w, 'x, 'y, 'z) witness1) t

  external inject
    :  ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    -> ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness
    = "%identity"

  external project
    :  ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness
    -> ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    = "%identity"
end

module Make8 (X : sig
    type (_, _, _, _, _, _, _, _) t
  end) :
  S8 with type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t =
struct
  type witness8
  type 'z witness7 = ('z, witness8) t
  type ('y, 'z) witness6 = ('y, 'z witness7) t
  type ('x, 'y, 'z) witness5 = ('x, ('y, 'z) witness6) t
  type ('a, 'x, 'y, 'z) witness4 = ('a, ('x, 'y, 'z) witness5) t
  type ('a, 'w, 'x, 'y, 'z) witness3 = ('a, ('w, 'x, 'y, 'z) witness4) t
  type ('a, 'v, 'w, 'x, 'y, 'z) witness2 = ('a, ('v, 'w, 'x, 'y, 'z) witness3) t
  type ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness1 = ('a, ('u, 'v, 'w, 'x, 'y, 'z) witness2) t

  type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) witness =
    ('a, ('t, 'u, 'v, 'w, 'x, 'y, 'z) witness1) t

  external inject
    :  ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    -> ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) witness
    = "%identity"

  external project
    :  ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) witness
    -> ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    = "%identity"
end

include Make2 (struct
    type nonrec ('a, 'witness) t = ('a, 'witness) t
  end)

module Array = Make (Array)
module Either = Make2 (Either)
module Hash_set = Make (Hash_set)
module Hashtbl = Make2 (Hashtbl)
module Ident = Make (Monad.Ident)
module Lazy = Make (Lazy)
module List = Make (List)
module Map = Make3 (Map)
module Option = Make (Option)
module Queue = Make (Queue)
module Ref = Make (Ref)
module Result = Make2 (Result)
module Set = Make2 (Set)
module Sequence = Make (Sequence)
module Type_equal = Make2 (Type_equal)
