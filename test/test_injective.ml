open! Base

(* This tests that we can match on a [Higher_kinded.t] in a GADT. Which requires that the
   type be marked injective. If [Higher_kinded.t] is not injective, this will fail with:
   "In the GADT constructor Succ : ('args, 'witness, 'value_args Higher_kinded.t) t -> ('a
   -> 'args, 'witness, ('a -> 'value_args) Higher_kinded.t) t the type variable
   'value_args cannot be deduced from the type parameters." *)
module Arity = struct
  type (_, _, _) t =
    | Zero : (unit, 'witness, 'witness Higher_kinded.t) t
    | Succ :
        ('args, 'witness, 'value_args Higher_kinded.t) t
        -> ('a -> 'args, 'witness, ('a -> 'value_args) Higher_kinded.t) t

  let rec type_equal
    : type args witness value_a value_b.
      (args, witness, value_a) t
      -> (args, witness, value_b) t
      -> (value_a, value_b) Type_equal.t
    =
    fun a b ->
    match a, b with
    | Zero, Zero -> T
    | Succ a, Succ b ->
      (match type_equal a b with
       | T -> T)
  ;;
end

module Packed = struct
  type (_, _) t = T : ('args, 'witness, 'value) Arity.t * 'value -> ('args, 'witness) t

  let unpack
    : type args witness value.
      (args, witness, value) Arity.t -> (args, witness) t -> value
    =
    fun arity_a -> function
    | T (arity_b, value) ->
      (match Arity.type_equal arity_a arity_b with
       | T -> value)
  ;;
end

let%expect_test "pack_unpack" =
  let x = Packed.T (Succ Zero, Higher_kinded.List.inject [ "hello"; "world" ]) in
  let y = Higher_kinded.List.project (Packed.unpack (Succ Zero) x) in
  Core.print_s (List.sexp_of_t String.sexp_of_t y);
  [%expect {| (hello world) |}]
;;
