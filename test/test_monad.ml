open! Core
open! Expect_test_helpers_base

(* Given a monad [M], [Make_monad] returns a monad [H] s.t.

   1. [H.return] is [M.return], modulo [inject]/[project].
   2. [H.bind] is [M.bind], modulo [inject]/[project]. *)

module type S = sig
  type 'a t [@@deriving equal, quickcheck, sexp_of]

  include Monad.S with type 'a t := 'a t
end

module Test (M : S) : sig end = struct
  module H = Higher_kinded.Make_monad (M)

  module type Bisimulation = sig
    type t [@@deriving quickcheck, sexp_of]

    module Output : sig
      type t [@@deriving equal, sexp_of]
    end

    val f1 : t -> Output.t
    val f2 : t -> Output.t
  end

  let bisimulate (module M : Bisimulation) =
    require_does_not_raise (fun () ->
      Base_quickcheck.Test.run_exn (module M) ~f:(fun x ->
        require_equal (module M.Output) (M.f1 x) (M.f2 x)))
  ;;

  let%expect_test "[H.return] is [M.return] modulo [inject]/[project]" =
    bisimulate
      (module struct
        type t = int [@@deriving quickcheck, sexp_of]

        module Output = struct
          type t = int M.t [@@deriving equal, sexp_of]
        end

        let f1 = M.return
        let f2 a = H.return a |> H.project
      end);
    [%expect {| |}]
  ;;

  let%expect_test "[H.bind] is [M.bind] modulo [inject]/[project]" =
    bisimulate
      (module struct
        type t =
          { m : int M.t
          ; f : int -> string M.t
          }
        [@@deriving quickcheck, sexp_of]

        module Output = struct
          type t = string M.t [@@deriving equal, sexp_of]
        end

        let f1 { m; f } =
          let open M in
          m >>= f
        ;;

        let f2 { m; f } =
          let open H in
          inject m >>= Fn.id (fun x -> inject (f x)) |> project
        ;;
      end);
    [%expect {| |}]
  ;;
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
module%test [@tags "no-js"] List = Test (List)
module%test [@tags "js-only"] List = Test (Short_list)
