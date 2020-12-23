(* Binary Tree *)

fun fact 1 = 1
    | fact x = x * fact (x-1);


datatype ('a, 'b) sum = In1 of 'a | In2 of 'b;


datatype 'a tree = empty
    | node of 'a * 'a tree * 'a tree;

datatype ('a, 'b) btree = leaf('b)
    | bnode of 'a * ('a, 'b) btree * ('a, 'b) btree;

fun size empty = 0
  | size (node(v, l, r)) = 1+ size(l) + size(r);

(*
fun depth empty = 0
  | depth (node(_, l, r) =
    let d1 = depth l;
        d2 = depth r;
    in
      if (d1 < d2)
        then d2
        else d1
     end;
 *)

fun depth empty = 0
  | depth (node(_, l, r)) = 1+ Int.max(depth l, depth r);


fun mktree (k, n) =
  if n = 0 then empty
           else node(k, mktree(k * 2, n-1), mktree(k*2+1, n-1));



fun reflect (empty)  = empty
 |  reflect (node(v, l, r))  = node(v, reflect r, reflect l);

fun reflected (empty, empty) = true
 |  reflected (node(v, l, r), node(v2, l2, r2)) =
      (if (v = v2) then
        (reflected(l, r2) andalso reflected(r, l2))
        else false)
  | reflected (_,_) = false;


val t1 = node(1, empty, node(4, empty, empty));
val t2 = mktree(1, 5);

