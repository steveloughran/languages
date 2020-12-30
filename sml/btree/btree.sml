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


(* the different comparison outcomes *)
datatype comparison = LessThan | IsEqual | GreaterThan;

fun ('a) mkcomparator (feq: 'a * 'a -> bool,
  flt: 'a * 'a -> bool) =
  let
    fun compare(x, y) =
      if feq(x, y) then IsEqual
      else if flt(x, y) then LessThan
      else GreaterThan
   in
     compare
   end;

(* The comparison code for an integer *)
val intorder = mkcomparator(
  fn (x: int, y) => x = y,
  fn (x: int, y) => x < y);

(* static Binding info preserved across all nodes *)
datatype ('a) binding_info =
  Binding of int * ('a * 'a -> comparison);



(* a tree
  empty
  data of key + val
  index of: highest-key, capacity, entries.

*)
datatype ('a, 'b) tree =
    EmptyRoot of ('a) binding_info
  | DataNode of ('a) binding_info * 'a * 'b
  | IndexNode of ('a) binding_info * int * 'a * (('a, 'b) tree) list;

(* Create an empty tree of a given branch size *)
fun mktree (binding) = EmptyRoot(binding);

(* Datanode for a key *)
fun datanodeelt (binding, key, entry)
  = DataNode(binding, key, entry);


(*
 Insert an entry into tree.
 This initial impl just adds into index nodes so is just adding
 them to a list. Flawed.
 *)


fun insert (EmptyRoot(binding), key, entry) =
      (* Empty root: replace with a simple data node *)
      DataNode(binding, key, entry)

  | insert (dn as DataNode(binding, k, e), key, entry) =
      (* Data Node: create an index node above it *)

      IndexNode(binding, key,
       [
        dn,
        datanodeelt(binding, key, entry)
        ]
       )
  | insert (idx as IndexNode(binding, capacity, l), key, entry) =
    (* Index node: add another index node. *)
      IndexNode(binding, capacity - 1,
        datanodeelt(binding, key, entry) :: l);

(* given a list of (key, entry) tuples, add all to the existing tree. *)

fun insertall (t, []) = t
  | insertall (t, (key, entry) :: tail) =
      insertall(insert(t, key, entry), tail);

val samples =
  [ ("a", 1), ("b", 2), ("m", 5), ("c", 2) ];

val stree = insertall(mktree(3), samples);


(* Find an entry which matches a key *)
(*
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
*)


