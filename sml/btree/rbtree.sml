use("assert.sml");

(* the different comparison outcomes *)
datatype Colour = Red | Black;

datatype Tree =
    Empty
  | T of Colour * Tree  * string * Tree;

fun empty() = Empty;

fun isEmpty(Empty) = true
 | isEmpty(_) = false;

assert("emptiness", isEmpty(empty()));

fun red(key, left, right) = T(Red, left, key, right);
fun black(key, left, right) = T(Black, left, key, right);

fun isRed(Empty) = false
    | isRed(T(Red, _, _, _)) = true
    | isRed(_) = false;

fun isFalse(f) = not f;

(* operand typecon errors I don't understand yet *)
fun isNotRed(f: Tree) = if isRed(f) then false else true;

(* Membership probe *)
fun member(key, Empty) = false
  | member(key, T(_, left, k, right)) =
    if key = k then
      true
    else if key < k then
      member(key, left)
    else
      member(key, right);

(* pattern match for the RR invariant *)
fun noRedHasARedParent(Empty) = true
  | noRedHasARedParent(T(Red, T(Red, _, _, _), _, _)) =
      false
  | noRedHasARedParent(T(Red, _, _, T(Red, _, _, _))) =
      false
  | noRedHasARedParent(T(_, left, _, right)) =
      noRedHasARedParent(left) andalso noRedHasARedParent(right);

(* colour to black node count *)
fun blackCount(Black) = 1
  | blackCount(Red) = 0;

(*
 Black nodes must balance
 Returns depth of black nodes on a balanced tree,
 and whether or not the tree is balanced.
 If the tree is unbalanced then the
 depth element of the tuple is invalid,

*)
fun blacknodesBalance(Empty) : bool * int =
      (true, 1)  (* empty considered black *)
  | blacknodesBalance(T(colour, left, _ , right)) =
      let
        (* Both sides must be valid, and each side's black count must be equal *)
        val (validL, depthL) = blacknodesBalance(left);
        val (validR, depthR) = blacknodesBalance(right)
      in
        (
          (* Validity *)
          depthL = depthR andalso validL andalso validR,
           (* Depth increments depending on the colour. correct iff L and R are balanced *)
          blackCount(colour) + depthL
        )
      end;

fun makeBlack(T(_, a, y, b)) = T(Black, a, y, b)
  | makeBlack(Empty) = Empty; 

fun resolve(a, b, c, d, x, y, z) =
     T(Red,
        T(Black, a, x, b),
        y,
        T(Black, c, z, d));

fun balance (Black, T(Red, T(Red, a, x, b), y, c), z, d) = resolve(a, b, c, d, x, y, z)
  | balance (Black, T(Red, a, x, T(Red, b, y, c)), z, d) = resolve(a, b, c, d, x, y, z)
  | balance (Black, a, x, T(Red, b, y, T(Red, c, z, d))) = resolve(a, b, c, d, x, y, z)
  | balance (Black, a, x, T(Red, T(Red, b, y, c), z, d)) = resolve(a, b, c, d, x, y, z)
  | balance (colour, a, x, b) = T(colour, a, x, b);

(* Insertion *)

fun ins (k, Empty) = red(k, Empty, Empty)
 |  ins (k, T(colour, a, y, b)) =
      if (k < y) then
        balance(colour, (ins(k, a)), y, b)
      else if (k > y) then
        balance(colour, a, y, (ins(k, b)))
     else (* k = y *)
        T(colour, a, y, b);


fun insert (k, t) = makeBlack(ins(k, t));

