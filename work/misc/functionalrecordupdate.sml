

structure FunctionalTupleUpdate =
struct

(* updateTuple_4_1 : ('a * 'b * 'c * 'd) * 'a -> 'a * 'b * 'c * 'd *)
fun updateTuple_4_1 ((a: 'a,  b: 'b, c: 'c, d: 'd), newa: 'a) = (newa, b, c, d);

(* updateTuple_4_2 : ('a * 'b * 'c * 'd) * 'b -> 'a * 'b * 'c * 'd *)
fun updateTuple_4_2 ((a: 'a,  b: 'b, c: 'c, d: 'd), newb: 'b) = (a, newb, c, d);

(* and so on for updateTuple_4_3, updateTuple_4_4, and similar families of functions for
 * updating components of 2-tuples, 3-tuples, 5-tuples, etc. *)

end (* structure FunctionalTupleUpdate *)

(* An n-ary _record schema_ is determined by a _set_ of n field labels.  Remember that the *order*
 * of the fields is immaterial for type equality: {a: ta, b:tb} and {b: tb, a: ta} are the same
 * record type.
 *
 * We can represent an n-ary record schema as a type function with n arguments, but
 * note that the order of type arguments of r4 below indirectly determines a particular
 * ordering of the fields. *)

(* r: a particular record schema with field names a, b, c, d, represented as a type function. *)
type ('a, 'b, 'c, 'd) r = {a: 'a, b: 'b, c: 'c, d: 'd} 

(* the following functions are defined relative to a particular record schema, in this case schema r. *)

(* recordToTuple_r : ('a, 'b, 'c, 'd) r4 -> 'a * 'b * 'c * 'd) *)
fun recordToTuple_r ({a, b, c, d}: ('a, 'b, 'c, 'd) r4 = (a,b,c,d)

(* tupleToRecord_r : 'a * 'b * 'c * 'd -> ('a,'b,'c,'d) r *)
fun tupleToRecord_r (a,b,c,d) = {a = a, b = b, c = c, d = d}
								       
(* undate_r_a:  ('a, 'b, 'c, 'd) r * 'a -> ('a, 'b, 'c, 'd) r *)
fun update_r_b (r: ('a,'b,'c,'d) r, b : 'b) =
    tupleToRecord_r (updateTuple_4_1 (recordToTuple__r r, b))

(* undate_r_b:  ('a, 'b, 'c, 'd) r4 * 'b -> ('a, 'b, 'c, 'd) r4 *)
fun update_r_b (r: ('a,'b,'c,'d) r, b : 'b) =
    tupleToRecord_r (updateTuple_4_2 (recordToTuple_r, b))

(* Note that update_r4_a, etc. can operate on any record type that is an instance of the record type
 * schema r. Thus it is "generic" relative to the record schema r, i.e. that set of field names
 * {a, b, c, d}. *)

(* So, for any particular record schema (set of field names) represented as a type function, say s,
 * we need to define corresponding recordToTuple and tupleToRecord functions and update_s_f functions
 * for whatever fields "f" we will need to update. *)
