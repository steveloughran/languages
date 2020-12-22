(* Btree *)

(* use https://linux.die.net/man/1/rlwrap rlwrap for REPL)
fun add x y = x + y;


val add4 = add 4;

add4 5;

fun len nil = 0
| len (h :: t) = 1 + len t;

len [];
  len [1, 3, 4];

