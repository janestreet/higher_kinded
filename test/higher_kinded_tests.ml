open Core_kernel

let%test_module "documentation examples" =
  (module struct
    module Option : sig
      type 'a t =
        | None
        | Some of 'a

      include Higher_kinded.S with type 'a t := 'a t
    end = struct
      module T = struct
        type 'a t =
          | None
          | Some of 'a
      end

      include T
      include Higher_kinded.Make (T)
    end

    type 't mytype = (unit, 't) Higher_kinded.t

    let none0 : unit Option.t = Option.None
    and some0 : unit Option.t = Option.Some ()

    let none1 : (unit, Option.witness1) Higher_kinded.t = Option.inject none0
    and some1 : (unit, Option.witness1) Higher_kinded.t = Option.inject some0

    let none2 : unit Option.t = Option.project none1
    and some2 : unit Option.t = Option.project some1

    let none1 : Option.witness1 mytype = none1
    and some1 : Option.witness1 mytype = some1

    let none1 : unit Option.witness = none1
    and some1 : unit Option.witness = some1

    let%test_unit "equalities" =
      assert (phys_same none0 none1);
      assert (phys_equal none0 none2);
      assert (phys_same some0 some1);
      assert (phys_equal some0 some2)
    ;;
  end)
;;

let%test_module "tests" =
  (module struct
    type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t =
      | Immediate
      | Allocated of
          { a : 'a
          ; b : 'b
          ; c : 'c
          ; d : 'd
          ; e : 'e
          ; f : 'f
          ; g : 'g
          ; h : 'h
          }
    [@@deriving compare, quickcheck, sexp_of]

    module type S = sig
      type injected

      val inject : (int, int, int, int, int, int, int, int) t -> injected
      val project : injected -> (int, int, int, int, int, int, int, int) t
    end

    let test (type injected) (module M : S with type injected = injected) =
      Quickcheck.test
        [%generator: (int, int, int, int, int, int, int, int) t]
        ~sexp_of:[%sexp_of: (int, int, int, int, int, int, int, int) t]
        ~shrinker:[%shrinker: (int, int, int, int, int, int, int, int) t]
        ~f:(fun orig ->
          let injected = M.inject orig in
          assert (phys_same orig injected);
          let projected = M.project injected in
          assert (phys_same injected projected);
          assert (
            [%compare.equal: (int, int, int, int, int, int, int, int) t] orig projected))
    ;;

    module T1 = struct
      include Higher_kinded.Make (struct
          type nonrec 'a t = ('a, int, int, int, int, int, int, int) t
        end)

      type injected = int witness
    end

    module T2 = struct
      include Higher_kinded.Make2 (struct
          type nonrec ('a, 'b) t = ('a, 'b, int, int, int, int, int, int) t
        end)

      type injected = (int, int) witness
    end

    module T3 = struct
      include Higher_kinded.Make3 (struct
          type nonrec ('a, 'b, 'c) t = ('a, 'b, 'c, int, int, int, int, int) t
        end)

      type injected = (int, int, int) witness
    end

    module T4 = struct
      include Higher_kinded.Make4 (struct
          type nonrec ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd, int, int, int, int) t
        end)

      type injected = (int, int, int, int) witness
    end

    module T5 = struct
      include Higher_kinded.Make5 (struct
          type nonrec ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e, int, int, int) t
        end)

      type injected = (int, int, int, int, int) witness
    end

    module T6 = struct
      include Higher_kinded.Make6 (struct
          type nonrec ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, 'b, 'c, 'd, 'e, 'f, int, int) t
        end)

      type injected = (int, int, int, int, int, int) witness
    end

    module T7 = struct
      include Higher_kinded.Make7 (struct
          type nonrec ('a, 'b, 'c, 'd, 'e, 'f, 'g) t = ('a, 'b, 'c, 'd, 'e, 'f, 'g, int) t
        end)

      type injected = (int, int, int, int, int, int, int) witness
    end

    module T8 = struct
      include Higher_kinded.Make8 (struct
          type nonrec ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t =
            ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t
        end)

      type injected = (int, int, int, int, int, int, int, int) witness
    end

    let%test_unit "Make" = test (module T1)
    let%test_unit "Make2" = test (module T2)
    let%test_unit "Make3" = test (module T3)
    let%test_unit "Make4" = test (module T4)
    let%test_unit "Make5" = test (module T5)
    let%test_unit "Make6" = test (module T6)
    let%test_unit "Make7" = test (module T7)
    let%test_unit "Make8" = test (module T8)
  end)
;;
