open Base

module type S = sig
  type (_, _) higher_kinded
  type 'a t
  type witness1
  type 'a witness = ('a, witness1) higher_kinded

  val inject : 'a t -> 'a witness
  val project : 'a witness -> 'a t
end

module type S2 = sig
  type (_, _) higher_kinded
  type ('a, 'z) t
  type witness2
  type 'z witness1 = ('z, witness2) higher_kinded
  type ('a, 'z) witness = ('a, 'z witness1) higher_kinded

  val inject : ('a, 'z) t -> ('a, 'z) witness
  val project : ('a, 'z) witness -> ('a, 'z) t
end

module type S3 = sig
  type (_, _) higher_kinded
  type ('a, 'y, 'z) t
  type witness3
  type 'z witness2 = ('z, witness3) higher_kinded
  type ('y, 'z) witness1 = ('y, 'z witness2) higher_kinded
  type ('a, 'y, 'z) witness = ('a, ('y, 'z) witness1) higher_kinded

  val inject : ('a, 'y, 'z) t -> ('a, 'y, 'z) witness
  val project : ('a, 'y, 'z) witness -> ('a, 'y, 'z) t
end

module type S4 = sig
  type (_, _) higher_kinded
  type ('a, 'x, 'y, 'z) t
  type witness4
  type 'z witness3 = ('z, witness4) higher_kinded
  type ('y, 'z) witness2 = ('y, 'z witness3) higher_kinded
  type ('x, 'y, 'z) witness1 = ('x, ('y, 'z) witness2) higher_kinded
  type ('a, 'x, 'y, 'z) witness = ('a, ('x, 'y, 'z) witness1) higher_kinded

  val inject : ('a, 'x, 'y, 'z) t -> ('a, 'x, 'y, 'z) witness
  val project : ('a, 'x, 'y, 'z) witness -> ('a, 'x, 'y, 'z) t
end

module type S5 = sig
  type (_, _) higher_kinded
  type ('a, 'w, 'x, 'y, 'z) t
  type witness5
  type 'z witness4 = ('z, witness5) higher_kinded
  type ('y, 'z) witness3 = ('y, 'z witness4) higher_kinded
  type ('x, 'y, 'z) witness2 = ('x, ('y, 'z) witness3) higher_kinded
  type ('a, 'x, 'y, 'z) witness1 = ('a, ('x, 'y, 'z) witness2) higher_kinded
  type ('a, 'w, 'x, 'y, 'z) witness = ('a, ('w, 'x, 'y, 'z) witness1) higher_kinded

  val inject : ('a, 'w, 'x, 'y, 'z) t -> ('a, 'w, 'x, 'y, 'z) witness
  val project : ('a, 'w, 'x, 'y, 'z) witness -> ('a, 'w, 'x, 'y, 'z) t
end

module type S6 = sig
  type (_, _) higher_kinded
  type ('a, 'v, 'w, 'x, 'y, 'z) t
  type witness6
  type 'z witness5 = ('z, witness6) higher_kinded
  type ('y, 'z) witness4 = ('y, 'z witness5) higher_kinded
  type ('x, 'y, 'z) witness3 = ('x, ('y, 'z) witness4) higher_kinded
  type ('a, 'x, 'y, 'z) witness2 = ('a, ('x, 'y, 'z) witness3) higher_kinded
  type ('a, 'w, 'x, 'y, 'z) witness1 = ('a, ('w, 'x, 'y, 'z) witness2) higher_kinded

  type ('a, 'v, 'w, 'x, 'y, 'z) witness =
    ('a, ('v, 'w, 'x, 'y, 'z) witness1) higher_kinded

  val inject : ('a, 'v, 'w, 'x, 'y, 'z) t -> ('a, 'v, 'w, 'x, 'y, 'z) witness
  val project : ('a, 'v, 'w, 'x, 'y, 'z) witness -> ('a, 'v, 'w, 'x, 'y, 'z) t
end

module type S7 = sig
  type (_, _) higher_kinded
  type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
  type witness7
  type 'z witness6 = ('z, witness7) higher_kinded
  type ('y, 'z) witness5 = ('y, 'z witness6) higher_kinded
  type ('x, 'y, 'z) witness4 = ('x, ('y, 'z) witness5) higher_kinded
  type ('a, 'x, 'y, 'z) witness3 = ('a, ('x, 'y, 'z) witness4) higher_kinded
  type ('a, 'w, 'x, 'y, 'z) witness2 = ('a, ('w, 'x, 'y, 'z) witness3) higher_kinded

  type ('a, 'v, 'w, 'x, 'y, 'z) witness1 =
    ('a, ('v, 'w, 'x, 'y, 'z) witness2) higher_kinded

  type ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness =
    ('a, ('u, 'v, 'w, 'x, 'y, 'z) witness1) higher_kinded

  val inject : ('a, 'u, 'v, 'w, 'x, 'y, 'z) t -> ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness
  val project : ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness -> ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
end

module type S8 = sig
  type (_, _) higher_kinded
  type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
  type witness8
  type 'z witness7 = ('z, witness8) higher_kinded
  type ('y, 'z) witness6 = ('y, 'z witness7) higher_kinded
  type ('x, 'y, 'z) witness5 = ('x, ('y, 'z) witness6) higher_kinded
  type ('a, 'x, 'y, 'z) witness4 = ('a, ('x, 'y, 'z) witness5) higher_kinded
  type ('a, 'w, 'x, 'y, 'z) witness3 = ('a, ('w, 'x, 'y, 'z) witness4) higher_kinded

  type ('a, 'v, 'w, 'x, 'y, 'z) witness2 =
    ('a, ('v, 'w, 'x, 'y, 'z) witness3) higher_kinded

  type ('a, 'u, 'v, 'w, 'x, 'y, 'z) witness1 =
    ('a, ('u, 'v, 'w, 'x, 'y, 'z) witness2) higher_kinded

  type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) witness =
    ('a, ('t, 'u, 'v, 'w, 'x, 'y, 'z) witness1) higher_kinded

  val inject
    :  ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
    -> ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) witness

  val project
    :  ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) witness
    -> ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
end

module type Higher_kinded = sig
  (** This library allows you to use higher-kinded types in OCaml. See the README for a
      short tutorial on what that means and how to use it. *)


  (** {2 Types} *)

  (** If [A] implements the signature [S], [(a, A.witness1) t] is equivalent to [a A.t].
  *)
  type ('a, 'witness) t

  (** If [A] implements the signature [S2], [(a, b, A.witness2) t2] is equivalent to [(a,
      b) A.t]. *)
  type ('a, 'b, 'witness) t2 = ('a, ('b, 'witness) t) t

  (** If [A] implements the signature [S3], [(a, b, c, A.witness3) t3] is equivalent to
      [(a, b, c) A.t]. *)
  type ('a, 'b, 'c, 'witness) t3 = ('a, 'b, ('c, 'witness) t) t2

  (** If [A] implements the signature [S4], [(a, b, c, d, A.witness4) t4] is equivalent to
      [(a, b, c, d) A.t]. *)
  type ('a, 'b, 'c, 'd, 'witness) t4 = ('a, 'b, 'c, ('d, 'witness) t) t3

  (** If [A] implements the signature [S5], [(a, b, c, d, e, A.witness5) t5] is equivalent
      to [(a, b, c, d, e) A.t]. *)
  type ('a, 'b, 'c, 'd, 'e, 'witness) t5 = ('a, 'b, 'c, 'd, ('e, 'witness) t) t4

  (** If [A] implements the signature [S6], [(a, b, c, d, e, f, A.witness6) t6] is
      equivalent to [(a, b, c, d, e, f) A.t]. *)
  type ('a, 'b, 'c, 'd, 'e, 'f, 'witness) t6 = ('a, 'b, 'c, 'd, 'e, ('f, 'witness) t) t5

  (** If [A] implements the signature [S7], [(a, b, c, d, e, f, g, A.witness7) t7] is
      equivalent to [(a, b, c, d, e, f, g) A.t]. *)
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'witness) t7 =
    ('a, 'b, 'c, 'd, 'e, 'f, ('g, 'witness) t) t6

  (** If [A] implements the signature [S8], [(a, b, c, d, e, f, g, h, A.witness8) t8] is
      equivalent to [(a, b, c, d, e, f, g, h) A.t]. *)
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'witness) t8 =
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, ('h, 'witness) t) t7

  (** {2 Signatures} *)

  (** These are the signatures implemented by the [Make] family of functors. *)

  module type S = S with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S2 = S2 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S3 = S3 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S4 = S4 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S5 = S5 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S6 = S6 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S7 = S7 with type ('a, 'witness) higher_kinded := ('a, 'witness) t
  module type S8 = S8 with type ('a, 'witness) higher_kinded := ('a, 'witness) t

  (** {2 Functors} *)

  (** This is the meat of the library. Use these functors to implement the higher_kinded
      interface. *)

  module Make (X : T1) : S with type 'a t := 'a X.t
  module Make2 (X : T2) : S2 with type ('a, 'z) t := ('a, 'z) X.t
  module Make3 (X : T3) : S3 with type ('a, 'y, 'z) t := ('a, 'y, 'z) X.t

  module Make4 (X : sig
      type (_, _, _, _) t
    end) : S4 with type ('a, 'x, 'y, 'z) t := ('a, 'x, 'y, 'z) X.t

  module Make5 (X : sig
      type (_, _, _, _, _) t
    end) : S5 with type ('a, 'w, 'x, 'y, 'z) t := ('a, 'w, 'x, 'y, 'z) X.t

  module Make6 (X : sig
      type (_, _, _, _, _, _) t
    end) : S6 with type ('a, 'v, 'w, 'x, 'y, 'z) t := ('a, 'v, 'w, 'x, 'y, 'z) X.t

  module Make7 (X : sig
      type (_, _, _, _, _, _, _) t
    end) : S7 with type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t

  module Make8 (X : sig
      type (_, _, _, _, _, _, _, _) t
    end) :
    S8
    with type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t :=
      ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t

  (** {2 Implementations} *)

  (** [Base], [Core], and [Async] don't depend on [Higher_kinded], so we put these
      implementations here instead of in the respective modules where they might have been
      a nicer fit. *)

  module Ident : S with type 'a t := 'a
  module Array : S with type 'a t := 'a Array.t
  module Either : S2 with type ('a, 'b) t := ('a, 'b) Either.t
  module Hash_set : S with type 'a t := 'a Hash_set.t
  module Hashtbl : S2 with type ('a, 'b) t := ('a, 'b) Hashtbl.t
  module Lazy : S with type 'a t := 'a Lazy.t
  module List : S with type 'a t := 'a List.t
  module Map : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) Map.t
  module Option : S with type 'a t := 'a Option.t
  module Queue : S with type 'a t := 'a Queue.t
  module Ref : S with type 'a t := 'a Ref.t
  module Result : S2 with type ('a, 'e) t := ('a, 'e) Result.t
  module Set : S2 with type ('a, 'b) t := ('a, 'b) Set.t
  module Sequence : S with type 'a t := 'a Sequence.t
  module Type_equal : S2 with type ('a, 'b) t := ('a, 'b) Type_equal.t

  (** [t] itself has two type parameters, so we might as well implement [S2] right here.
  *)
  include
    S2 with type ('a, 'witness) t := ('a, 'witness) t
end
