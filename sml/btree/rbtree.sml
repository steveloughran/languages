use("assert.sml");

(* the different comparison outcomes *)
datatype Color = R | B;

datatype Tree =
    Empty
  | T of Color * Tree  * string * Tree;

fun empty() = Empty;

fun isEmpty(Empty) = true
 | isEmpty(_) = false;

assert("emptiness", isEmpty(empty()));

fun red(key, l, r) = T(R, l, key, r);
fun black(key, l, r) = T(B, l, key, r);

fun isRed(Empty) = false
    | isRed(T(R, _, _, _)) = true
    | isRed(_) = false;

fun isFalse(f) = not f;

fun isNotRed(f: Tree) = isFalse(isRed(f));

fun member(key, Empty) = false
  | member(key, T(_, l, k, r)) =
    if key = k then
      true
    else if key < k then
      member(key, l)
    else
      member(key, r);

fun noRedHasARedParent(Empty) = false
  | noRedHasARedParent(T(B, l, _, r)) =
      noRedHasARedParent(l) andalso noRedHasARedParent(r)
  | noRedHasARedParent(T(R, l, _, r)) =
      isNotRed(l) andalso isNotRed(r)
      andalso noRedHasARedParent(l) andalso noRedHasARedParent(r);
