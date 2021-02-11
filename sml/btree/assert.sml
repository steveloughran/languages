(*
Assertions
*)
(* Assertion not satisfied *)

exception Unsatisfied of string;

(* not yet done this *)
exception Todo of string;

fun assert(_, true) = true
  | assert(message, false) = raise Unsatisfied(message);

fun assertEq(x, y) =
    if not (x = y) then
      raise Unsatisfied("Expected " ^ x ^ " but got " ^ y)
    else x;
