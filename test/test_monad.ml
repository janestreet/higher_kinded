open! Core
open! Expect_test_helpers_base

(* Given a monad [M], [Make_monad] returns a monad [H] s.t.

   1. [H.return] is [M.return], modulo [inject]/[project].
   2. [H.bind] is [M.bind], modulo [inject]/[project]. *)

module type S = sig
  type 'a t [@@deriving equal, quickcheck, sexp_of]

  include Monad.S with type 'a t := 'a t
end

module%test Option = Test (Option)

module Short_list = struct
  include List

  let quickcheck_generator generate_a =
    let%bind.Base_quickcheck.Generator length =
      Base_quickcheck.Generator.int_inclusive 0 5
    in
    Base_quickcheck.Generator.list_with_length generate_a ~length
  ;;
end

(* js_of_ocaml is too slow to test with long lists. *)
module%test [@tags "64-bits-only"] List = Test (List)
module%test [@tags "js-only"] List = Test (Short_list)
