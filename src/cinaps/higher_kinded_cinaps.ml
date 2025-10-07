open! Base
open! Ppxlib
open! Cinaps_helpers

let tyvars n = Core_type.var "a" :: Core_type.vars_n (Int.neg (n - 1))

let witness name vars =
  [ Core_type.arrow vars (Core_type.constr "higher_kinded" []) ]
  |> Core_type.constr name
  |> Core_type.to_string
;;

let print_module_type_s () =
  print_n ~first:1 ~last:8 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    let type_ name = Core_type.constr name (tyvars n) |> Core_type.to_string in
    [%string
      {|
module type S%{nth} = sig @@ portable
  type %{type_ "t"}
  type higher_kinded
  val inject : %{type_ "t"} -> %{witness "Higher_kinded.t" (tyvars n)}
  val project : %{witness "Higher_kinded.t" (tyvars n)} -> %{type_ "t"}
end
|}])
;;

let print_module_type_monad () =
  print_n ~first:1 ~last:3 ~f:(fun n ->
    let type_ name = Core_type.constr_n name n |> Core_type.to_string in
    let nth = numbered n ~unless:1 in
    [%string
      {|
module type Monad%{nth} = sig
  include S%{nth}
  include Monad.S%{nth} with type %{type_ "t"} := %{witness "Higher_kinded.t" (Core_type.vars_n n)}
end
|}])
;;

let module_type_T n =
  [%string
    {|
sig
  type %{Core_type.constr "t" (tyvars n)#Core_type}
end
|}]
;;

let print_functor_types () =
  print_n ~first:1 ~last:8 ~f:(fun n ->
    let vars = tyvars n in
    let nth = numbered n ~unless:1 in
    [%string
      {|
module Make%{nth}
  (X : %{module_type_T n})
  : S%{nth}
      with type %{Core_type.constr "t" vars#Core_type}
                  := %{Core_type.constr "X.t" vars#Core_type}
|}]);
  let type_ name n = Core_type.constr_n name n |> Core_type.to_string in
  print_n ~first:1 ~last:3 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    [%string
      {|
module Make_monad%{nth}
  (M : Monad.S%{nth})
  : Monad%{nth} with type %{type_ "t" n} := %{type_ "M.t" n}
|}]);
  print_n ~first:1 ~last:3 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    [%string
      {|
module Make_monad_using_witness%{nth}
  (M : Monad.S%{nth})
  (X : S%{nth} with type %{type_ "t" n} := %{type_ "M.t" n})
  : Monad%{nth}
      with type %{type_ "t" n} := %{type_ "M.t" n}
      with type higher_kinded := X.higher_kinded
|}])
;;

let print_functor_implementations () =
  print_n ~first:1 ~last:8 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    let type_ name = Core_type.constr name (tyvars n) |> Core_type.to_string in
    [%string
      {|
module Make%{nth} (X : %{module_type_T n})
  : S%{nth} with type %{type_ "t"} := %{type_ "X.t"}
  = struct
    type higher_kinded
    external inject : %{type_ "X.t"} -> %{witness "t" (tyvars n) } @@ portable = "%identity"
    external project : %{witness "t" (tyvars n)} -> %{type_ "X.t"} @@ portable = "%identity"
  end
|}]);
  let type_ name n = Core_type.constr_n name n |> Core_type.to_string in
  print_n ~first:1 ~last:3 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    [%string
      {|
module Make_monad_using_witness%{nth}
  (M : Monad.S%{nth})
  (X : S%{nth} with type %{type_ "t" n} := %{type_ "M.t" n})
  = struct
      include X

      include Monad_of_monad%{nth} (M) (struct
        type nonrec %{type_ "t" n} = %{witness "t" (Core_type.vars_n n)}

        let to_monad = project
        let of_monad = inject
      end)
  end
|}]);
  print_n ~first:1 ~last:3 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    [%string
      {|
module Make_monad%{nth} (M : Monad.S%{nth}) =
  Make_monad_using_witness%{nth} (M) (Make%{nth} (M))
|}])
;;

let print_type_aliases ~include_comments =
  print_n ~first:1 ~last:8 ~f:(fun n ->
    let nth = numbered n ~unless:1 in
    let t k witness =
      let kth = numbered k ~unless:0 in
      Core_type.constr [%string "t%{kth}"] (Core_type.vars_n k @ [ witness ])
    in
    let a_t = t n (Core_type.constr "A.higher_kinded" []) in
    let a = Core_type.constr_n "A.t" n in
    let witness = Core_type.var "witness" in
    let this_t = t n witness in
    let prev_t =
      t (n - 1) (Core_type.arrow [ List.last_exn (Core_type.vars_n n) ] witness)
    in
    let comment =
      match include_comments with
      | false -> ""
      | true ->
        [%string
          {|
(** If [A] implements the signature [S%{nth}],
    [%{a_t#Core_type}]
    is equivalent to
    [%{a#Core_type}]. *)
|}]
    in
    [%string
      {|

%{comment}
type %{this_t#Core_type} = %{prev_t#Core_type}

|}])
;;
