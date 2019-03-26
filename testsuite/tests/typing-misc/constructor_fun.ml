(* TEST
   * expect
*)

let some = Some;;
[%%expect{|
val some : 'a -> 'a option = <fun>
|}]

let _ = Some 1 = (Some) 1;;
[%%expect{|
- : bool = true
|}]

let _ = List.map Some [ 1 ] = [ some 1 ];;
[%%expect{|
- : bool = true
|}]

type t = Constr of int * float
let constr = Constr;;
[%%expect{|
type t = Constr of int * float
val constr : int -> float -> t = <fun>
|}]

let _ = constr "a";;
[%%expect{|
Line 1, characters 15-18:
1 | let _ = constr "a";;
                   ^^^
Error: This expression has type string but an expression was expected of type
         int
|}]

(* Inline record *)
type t = Constr of { a : int; b : float }
let constr = Constr;;
[%%expect{|
type t = Constr of { a : int; b : float; }
val constr : a:int -> b:float -> t = <fun>
|}]

let _ = constr ~a:2 ~b:3.;;
[%%expect{|
- : t = Constr {a = 2; b = 3.}
|}]
