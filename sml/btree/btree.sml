(* Btree *)

(* use https://linux.die.net/man/1/rlwrap rlwrap for REPL *)

(*
datatype (''a, 'b) indexentry =
    Entry of ''a * (''a, 'b) tree
and
datatype (''a, 'b) tree =
    EmptyRoot of int
  | IndexNode of ''a * ''b * int * (indexentry) list;
*)

exception Unsatisfied of string;

fun assert(_, true) = true
  | assert(message, false) = raise Unsatisfied(message);

fun assertEq(x, y) =
    if not (x = y) then
      raise Unsatisfied("Expected " ^ x ^ " but got " ^ y)
    else x;

(* A tree *)
datatype ('b) tree =
    EmptyRoot of int
  | DataNode of int * string * 'b
  | IndexNode of int * int * (string * ('b) tree) list;

(* Create an empty tree of a given branch size *)
fun mktree (branching) = EmptyRoot(branching);

(* Datanode for a key *)
fun datanodeelt (branching, key, entry)
  = (key, DataNode(branching, key, entry));

(* Failed attempt at functional programming *)
(*
fun any (NONE, NONE) = NONE
  | any (SOME(x), _) = SOME(x)
  | any (NONE, SOME(y)) = SOME(y);


fun ap (op, [], x) = NONE
  | ap (op, (h :: t), x) = SOME(op);

fun apply (op, []) = NONE
  | apply (op, (h :: t)) = SOME(op);
*)
(*

    let v = op(h)
    in
     if v = NONE
      then apply(op, t)
      else v
    end;
*)
(*
    case op(h) of
      NONE => applyToEntry(op, t)
      SOME(v) => SOME(v);
*)

(* Find an entry which matches a key *)

fun find(key, EmptyRoot(_)) =
    (* Empty root: no match. *)
     NONE
 |  find(key, DataNode(_, k, entry)) =
      (* datanode: match iff the key and node key are equal *)
      if key = k then SOME(entry) else NONE
 |  find(key, IndexNode(_, _, [] )) = NONE
    (* Index node with no children *)
 |  find(key, IndexNode(a, b, (l, tr) :: t )) =
      (* Index node: look through all the children. *)

      let
        val headF = find(key, tr)
      in
      case headF of
        SOME(entry) => SOME(entry)
        | None => find(key, IndexNode(a, b, t))
      end;




(*
 Insert an entry into tree.
 This initial impl just adds into index nodes so is just adding
 them to a list. Flawed.
 *)


fun insert (EmptyRoot(branching), key, entry) =
      (* Empty root: replace with a simple data node *)
      DataNode(branching, key, entry)

  | insert (dn as DataNode(branching, k, e), key, entry) =
      (* Data Node: create an index node above it *)

      IndexNode(branching, branching,
       [
        (k, dn) ,
        datanodeelt(branching, key, entry)
        ]
       )
  | insert (idx as IndexNode(branching, capacity, l), key, entry) =
    (* Index node: add another index node. *)
      IndexNode(branching, capacity - 1,
        datanodeelt(branching, key, entry) :: l);

(* given a list of (key, entry) tuples, add all to the existing tree. *)

fun insertall (t, []) = t
  | insertall (t, (key, entry) :: tail) =
      insertall(insert(t, key, entry), tail);

val samples =
  [ ("a", 1), ("b", 2), ("m", 5), ("c", 2) ];

val stree = insertall(mktree(3), samples);

