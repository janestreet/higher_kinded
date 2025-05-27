open Base
open Higher_kinded_intf

type 'a t

(*$ Higher_kinded_cinaps.print_type_aliases ~include_comments:false *)

type ('a, 'witness) t1 = ('a -> 'witness) t
type ('a, 'b, 'witness) t2 = ('a, 'b -> 'witness) t1
type ('a, 'b, 'c, 'witness) t3 = ('a, 'b, 'c -> 'witness) t2
type ('a, 'b, 'c, 'd, 'witness) t4 = ('a, 'b, 'c, 'd -> 'witness) t3
type ('a, 'b, 'c, 'd, 'e, 'witness) t5 = ('a, 'b, 'c, 'd, 'e -> 'witness) t4
type ('a, 'b, 'c, 'd, 'e, 'f, 'witness) t6 = ('a, 'b, 'c, 'd, 'e, 'f -> 'witness) t5

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'witness) t7 =
  ('a, 'b, 'c, 'd, 'e, 'f, 'g -> 'witness) t6

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'witness) t8 =
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h -> 'witness) t7

(*$*)

include Higher_kinded_module_types (struct
    type nonrec 'a t = 'a t
  end)

module Monad_of_monad3
    (M : Monad.S3)
    (T : sig
       type ('a, 'b, 'c) t

       val to_monad : ('a, 'b, 'c) t -> ('a, 'b, 'c) M.t
       val of_monad : ('a, 'b, 'c) M.t -> ('a, 'b, 'c) t
     end) =
Monad.Make3 (struct
    type ('a, 'b, 'c) t = ('a, 'b, 'c) T.t

    let return a = T.of_monad (M.return a)
    let bind t ~f = M.bind (T.to_monad t) ~f:(fun a -> T.to_monad (f a)) |> T.of_monad
    let map = `Custom (fun t ~f -> M.map (T.to_monad t) ~f |> T.of_monad)
  end)

module Monad_of_monad2
    (M : Monad.S2)
    (T : sig
       type ('a, 'b) t

       val to_monad : ('a, 'b) t -> ('a, 'b) M.t
       val of_monad : ('a, 'b) M.t -> ('a, 'b) t
     end) =
  Monad_of_monad3
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.t

      include (M : module type of M with type ('a, 'b) t := ('a, 'b) M.t)
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) T.t

      include (T : module type of T with type ('a, 'b) t := ('a, 'b) T.t)
    end)

module Monad_of_monad
    (M : Monad.S)
    (T : sig
       type 'a t

       val to_monad : 'a t -> 'a M.t
       val of_monad : 'a M.t -> 'a t
     end) =
  Monad_of_monad2
    (struct
      type ('a, 'b) t = 'a M.t

      include (M : module type of M with type 'a t := 'a M.t)
    end)
    (struct
      type ('a, 'b) t = 'a T.t

      include (T : module type of T with type 'a t := 'a T.t)
    end)

(*$ Higher_kinded_cinaps.print_functor_implementations () *)
module Make (X : sig
    type 'a t
  end) : S with type 'a t := 'a X.t = struct
  type higher_kinded

  external inject : 'a X.t -> ('a -> higher_kinded) t @@ portable = "%identity"
  external project : ('a -> higher_kinded) t -> 'a X.t @@ portable = "%identity"
end

module Make2 (X : sig
    type ('a, 'z) t
  end) : S2 with type ('a, 'z) t := ('a, 'z) X.t = struct
  type higher_kinded

  external inject
    :  ('a, 'z) X.t
    -> ('a -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 'z -> higher_kinded) t
    -> ('a, 'z) X.t
    @@ portable
    = "%identity"
end

module Make3 (X : sig
    type ('a, 'y, 'z) t
  end) : S3 with type ('a, 'y, 'z) t := ('a, 'y, 'z) X.t = struct
  type higher_kinded

  external inject
    :  ('a, 'y, 'z) X.t
    -> ('a -> 'y -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 'y -> 'z -> higher_kinded) t
    -> ('a, 'y, 'z) X.t
    @@ portable
    = "%identity"
end

module Make4 (X : sig
    type ('a, 'x, 'y, 'z) t
  end) : S4 with type ('a, 'x, 'y, 'z) t := ('a, 'x, 'y, 'z) X.t = struct
  type higher_kinded

  external inject
    :  ('a, 'x, 'y, 'z) X.t
    -> ('a -> 'x -> 'y -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 'x -> 'y -> 'z -> higher_kinded) t
    -> ('a, 'x, 'y, 'z) X.t
    @@ portable
    = "%identity"
end

module Make5 (X : sig
    type ('a, 'w, 'x, 'y, 'z) t
  end) : S5 with type ('a, 'w, 'x, 'y, 'z) t := ('a, 'w, 'x, 'y, 'z) X.t = struct
  type higher_kinded

  external inject
    :  ('a, 'w, 'x, 'y, 'z) X.t
    -> ('a -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    -> ('a, 'w, 'x, 'y, 'z) X.t
    @@ portable
    = "%identity"
end

module Make6 (X : sig
    type ('a, 'v, 'w, 'x, 'y, 'z) t
  end) : S6 with type ('a, 'v, 'w, 'x, 'y, 'z) t := ('a, 'v, 'w, 'x, 'y, 'z) X.t = struct
  type higher_kinded

  external inject
    :  ('a, 'v, 'w, 'x, 'y, 'z) X.t
    -> ('a -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    -> ('a, 'v, 'w, 'x, 'y, 'z) X.t
    @@ portable
    = "%identity"
end

module Make7 (X : sig
    type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t
  end) : S7 with type ('a, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t =
struct
  type higher_kinded

  external inject
    :  ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    -> ('a -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    -> ('a, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    @@ portable
    = "%identity"
end

module Make8 (X : sig
    type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t
  end) :
  S8 with type ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) t := ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t =
struct
  type higher_kinded

  external inject
    :  ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    -> ('a -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    @@ portable
    = "%identity"

  external project
    :  ('a -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> higher_kinded) t
    -> ('a, 't, 'u, 'v, 'w, 'x, 'y, 'z) X.t
    @@ portable
    = "%identity"
end

module Make_monad_using_witness (M : Monad.S) (X : S with type 'a t := 'a M.t) = struct
  include X

  include
    Monad_of_monad
      (M)
      (struct
        type nonrec 'a t = ('a -> higher_kinded) t

        let to_monad = project
        let of_monad = inject
      end)
end

module Make_monad_using_witness2
    (M : Monad.S2)
    (X : S2 with type ('a, 'b) t := ('a, 'b) M.t) =
struct
  include X

  include
    Monad_of_monad2
      (M)
      (struct
        type nonrec ('a, 'b) t = ('a -> 'b -> higher_kinded) t

        let to_monad = project
        let of_monad = inject
      end)
end

module Make_monad_using_witness3
    (M : Monad.S3)
    (X : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t) =
struct
  include X

  include
    Monad_of_monad3
      (M)
      (struct
        type nonrec ('a, 'b, 'c) t = ('a -> 'b -> 'c -> higher_kinded) t

        let to_monad = project
        let of_monad = inject
      end)
end

module Make_monad (M : Monad.S) = Make_monad_using_witness (M) (Make (M))
module Make_monad2 (M : Monad.S2) = Make_monad_using_witness2 (M) (Make2 (M))
module Make_monad3 (M : Monad.S3) = Make_monad_using_witness3 (M) (Make3 (M))

(*$*)
include Make (struct
    type nonrec 'a t = 'a t
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
