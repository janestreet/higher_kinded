open Base

module Higher_kinded_module_types (Higher_kinded : T1) = struct
  (** These are the signatures implemented by the [Make] family of functors. *)

  (*$ Higher_kinded_cinaps.print_module_type_s () *)
  module type S = sig @@ portable
    type 'a t
    type higher_kinded

    val inject : 'a t -> ('a -> higher_kinded) Higher_kinded.t
    val project : ('a -> higher_kinded) Higher_kinded.t -> 'a t
  end

  module type S2 = sig @@ portable
    type ('a, 'z) t
    type higher_kinded

    val inject : ('a, 'z) t -> ('a -> 'z -> higher_kinded) Higher_kinded.t
    val project : ('a -> 'z -> higher_kinded) Higher_kinded.t -> ('a, 'z) t
  end

  module type S3 = sig @@ portable
    type ('a, 'y, 'z) t
    type higher_kinded

    val inject : ('a, 'y, 'z) t -> ('a -> 'y -> 'z -> higher_kinded) Higher_kinded.t
    val project : ('a -> 'y -> 'z -> higher_kinded) Higher_kinded.t -> ('a, 'y, 'z) t
  end

  module type S4 = sig @@ portable
    type ('a, 'x, 'y, 'z) t
    type higher_kinded

    val inject
      :  ('a, 'x, 'y, 'z) t
      -> ('a -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t

    val project
      :  ('a -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t
      -> ('a, 'x, 'y, 'z) t
  end

  module type S5 = sig @@ portable
    type ('a, 'w, 'x, 'y, 'z) t
    type higher_kinded

    val inject
      :  ('a, 'w, 'x, 'y, 'z) t
      -> ('a -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t

    val project
      :  ('a -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t
      -> ('a, 'w, 'x, 'y, 'z) t
  end

  module type S6 = sig @@ portable
    type ('a, 'v, 'w, 'x, 'y, 'z) t
    type higher_kinded

    val inject
      :  ('a, 'v, 'w, 'x, 'y, 'z) t
      -> ('a -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t

    val project
      :  ('a -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t
      -> ('a, 'v, 'w, 'x, 'y, 'z) t
  end

  module type S7 = sig @@ portable
    type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
    type higher_kinded

    val inject
      :  ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
      -> ('a -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t

    val project
      :  ('a -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t
      -> ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
  end

  module type S8 = sig @@ portable
    type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
    type higher_kinded

    val inject
      :  ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
      -> ('a -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t

    val project
      :  ('a -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) Higher_kinded.t
      -> ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
  end
  (*$*)

  (** These are the signatures implemented by the [Make_monad] and
      [Make_monad_using_witness] families of functors. *)

  (*$ Higher_kinded_cinaps.print_module_type_monad () *)
  module type Monad = sig
    include S
    include Monad.S with type 'a t := ('a -> higher_kinded) Higher_kinded.t
  end

  module type Monad2 = sig
    include S2
    include Monad.S2 with type ('a, 'b) t := ('a -> 'b -> higher_kinded) Higher_kinded.t
  end

  module type Monad3 = sig
    include S3

    include
      Monad.S3
      with type ('a, 'b, 'c) t := ('a -> 'b -> 'c -> higher_kinded) Higher_kinded.t
  end
  (*$*)
end

module type Higher_kinded = sig
  (** This library allows you to use higher-kinded types in OCaml. See the README for a
      short tutorial on what that means and how to use it. *)

  (** {2 Types} *)

  (** If [A] implements the signature [S], [(a, A.witness1) t] is equivalent to [a A.t]. *)
  type 'a t

  (*$ Higher_kinded_cinaps.print_type_aliases ~include_comments:true *)

  (** If [A] implements the signature [S], [('a, A.higher_kinded) t1] is equivalent to
      ['a A.t]. *)

  type ('a, 'witness) t1 = ('a -> 'witness) t

  (** If [A] implements the signature [S2], [('a, 'b, A.higher_kinded) t2] is equivalent
      to [('a, 'b) A.t]. *)

  type ('a, 'b, 'witness) t2 = ('a, 'b -> 'witness) t1

  (** If [A] implements the signature [S3], [('a, 'b, 'c, A.higher_kinded) t3] is
      equivalent to [('a, 'b, 'c) A.t]. *)

  type ('a, 'b, 'c, 'witness) t3 = ('a, 'b, 'c -> 'witness) t2

  (** If [A] implements the signature [S4], [('a, 'b, 'c, 'd, A.higher_kinded) t4] is
      equivalent to [('a, 'b, 'c, 'd) A.t]. *)

  type ('a, 'b, 'c, 'd, 'witness) t4 = ('a, 'b, 'c, 'd -> 'witness) t3

  (** If [A] implements the signature [S5], [('a, 'b, 'c, 'd, 'e, A.higher_kinded) t5] is
      equivalent to [('a, 'b, 'c, 'd, 'e) A.t]. *)

  type ('a, 'b, 'c, 'd, 'e, 'witness) t5 = ('a, 'b, 'c, 'd, 'e -> 'witness) t4

  (** If [A] implements the signature [S6], [('a, 'b, 'c, 'd, 'e, 'f, A.higher_kinded) t6]
      is equivalent to [('a, 'b, 'c, 'd, 'e, 'f) A.t]. *)

  type ('a, 'b, 'c, 'd, 'e, 'f, 'witness) t6 = ('a, 'b, 'c, 'd, 'e, 'f -> 'witness) t5

  (** If [A] implements the signature [S7],
      [('a, 'b, 'c, 'd, 'e, 'f, 'g, A.higher_kinded) t7] is equivalent to
      [('a, 'b, 'c, 'd, 'e, 'f, 'g) A.t]. *)

  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'witness) t7 =
    ('a, 'b, 'c, 'd, 'e, 'f, 'g -> 'witness) t6

  (** If [A] implements the signature [S8],
      [('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, A.higher_kinded) t8] is equivalent to
      [('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) A.t]. *)

  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'witness) t8 =
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h -> 'witness) t7

  (*$*)

  (** {2 Signatures} *)

  include module type of Higher_kinded_module_types (struct
      type nonrec 'a t = 'a t
    end)

  (** {2 Functors} *)

  (** This is the meat of the library. Use these functors to implement the higher_kinded
      interface. *)

  (*$ Higher_kinded_cinaps.print_functor_types () *)
  module Make (X : sig
      type 'a t
    end) : S with type 'a t := 'a X.t

  module Make2 (X : sig
      type ('a, 'z) t
    end) : S2 with type ('a, 'z) t := ('a, 'z) X.t

  module Make3 (X : sig
      type ('a, 'y, 'z) t
    end) : S3 with type ('a, 'y, 'z) t := ('a, 'y, 'z) X.t

  module Make4 (X : sig
      type ('a, 'x, 'y, 'z) t
    end) : S4 with type ('a, 'x, 'y, 'z) t := ('a, 'x, 'y, 'z) X.t

  module Make5 (X : sig
      type ('a, 'w, 'x, 'y, 'z) t
    end) : S5 with type ('a, 'w, 'x, 'y, 'z) t := ('a, 'w, 'x, 'y, 'z) X.t

  module Make6 (X : sig
      type ('a, 'v, 'w, 'x, 'y, 'z) t
    end) : S6 with type ('a, 'v, 'w, 'x, 'y, 'z) t := ('a, 'v, 'w, 'x, 'y, 'z) X.t

  module Make7 (X : sig
      type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
    end) : S7 with type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t

  module Make8 (X : sig
      type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
    end) :
    S8
    with type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t

  module Make_monad (M : Monad.S) : Monad with type 'a t := 'a M.t
  module Make_monad2 (M : Monad.S2) : Monad2 with type ('a, 'b) t := ('a, 'b) M.t
  module Make_monad3 (M : Monad.S3) : Monad3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t

  module Make_monad_using_witness (M : Monad.S) (X : S with type 'a t := 'a M.t) :
    Monad with type 'a t := 'a M.t with type higher_kinded := X.higher_kinded

  module Make_monad_using_witness2
      (M : Monad.S2)
      (X : S2 with type ('a, 'b) t := ('a, 'b) M.t) :
    Monad2 with type ('a, 'b) t := ('a, 'b) M.t with type higher_kinded := X.higher_kinded

  module Make_monad_using_witness3
      (M : Monad.S3)
      (X : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t) :
    Monad3
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t
    with type higher_kinded := X.higher_kinded
  (*$*)

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

  (** [t] itself has one type parameter, so we might as well implement [S] right here. *)
  include S with type 'a t := 'a t
end
