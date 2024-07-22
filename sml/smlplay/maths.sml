(* http://homepages.inf.ed.ac.uk/mfourman/teaching/mlCourse/ *)

(* power. See http://homepages.inf.ed.ac.uk/mfourman/teaching/mlCourse/notes/practicals/p1.html *)



fun power(x, 0) = x
  | power(x, n) = x * power(x, n -1);
  
power(2, 34);