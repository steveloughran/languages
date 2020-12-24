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

datatype ('b) tree =
    EmptyRoot of int
  | DataNode of int * string * 'b
  | IndexNode of int * int * (string * ('b) tree) list;

fun mktree (branching) = EmptyRoot(branching);

fun datanodeelt (branching, key, entry)
  = (key, DataNode(branching, key, entry));

fun insert (EmptyRoot(branching), key, entry) =
      DataNode(branching, key, entry)

  | insert (dn as DataNode(branching, k, e), key, entry) =
      IndexNode(branching, branching,
       [
        (k, dn) ,
        datanodeelt(branching, key, entry)
        ]
       )
  | insert (idx as IndexNode(branching, capacity, l), key, entry) =
      IndexNode(branching, capacity - 1,
        datanodeelt(branching, key, entry) :: l);

fun insertall (t, []) = t
  | insertall (t, (key, entry) :: tail) =
      insertall(insert(t, key, entry), tail);



