(* Btree *)

(* use https://linux.die.net/man/1/rlwrap rlwrap for REPL

rlwrap /usr/local/smlnj/bin/sml

use("btree.sml");

 *)

(*

Architectre

EmptyPage
RefPage: 1..order of int
LeafPage: 1..order of records

insert(emptypage, record) -> RefPage(LeafPage(record))
insert(leafPage, record)
  if has_capacity then
    add record in right place
  else
    add ref page with two leaf pages

insert(refPage, record)
  if has_capacity then
     add new leafPage in right place
  else
    find closest ref page
     add under (This will be ref or leaf)
     maybe: update index
   if has increased range of page, update parent


*)


(*
datatype (''a, 'b) indexentry =
    Entry of ''a * (''a, 'b) tree
and
datatype (''a, 'b) tree =
    EmptyRoot of int
  | IndexNode of ''a * ''b * int * (indexentry) list;
*)

use("assert.sml");

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
val compareint = mkcomparator(
  fn (x: int, y) => x = y,
  fn (x: int, y) => x < y);

(* static Binding info preserved across all nodes *)
datatype binding_info =
  Binding of int; (* order of the tree *)



(* a tree
  empty
  data of key + val
  index of: highest-key, order, entries.

*)
datatype ('b) tree =
    EmptyRoot of binding_info
  | DataNode of binding_info * int * 'b
  | IndexNode of binding_info * int * int * (('b) tree) list;

(* Create an empty tree of a given branch order *)
fun mktree (order) = EmptyRoot(Binding(order));

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

  | insert (dn as DataNode(binding as Binding(order), dnKey, dnEntry),
      key, entry) =
      (*
        Data Node: create an index node above it unless it is actually a replacement
       *)
       let
        val elt = datanodeelt(binding, key, entry)
       in
       case compareint(key, dnKey) of
            IsEqual =>
              (* replace *)
              DataNode(binding, key, entry)
          | LessThan =>
            IndexNode(binding, order - 1, dnKey, [elt, dn])
          | GreaterThan =>
            IndexNode(binding, order - 1, key, [dn, elt])
       end
  | insert (idx as IndexNode(binding, order, indexkey, l), key, entry) =
    (* Index node: add another index node. *)
      if order > 0 then
      (* TODO: where to insert *)
        IndexNode(binding, order - 1, indexkey,
          datanodeelt(binding, key, entry) :: l)
      else
        (* TODO: grow underneath *)
        raise Todo("No order");``



(* given a list of (key, entry) tuples, add all to the existing tree. *)

fun insertall (t, []) = t
  | insertall (t, (key, entry) :: tail) =
      insertall(insert(t, key, entry), tail);

val samples =
  [ (100, "a"), (200, "b"), (100, "m"), (3, "c") ];

val stree = insertall(mktree(2), samples);


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


