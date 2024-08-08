(* TEST
 expect;
*)

module type Vector_space = sig
  type t
  type scalar
  val scale : scalar -> t -> t
end;;
[%%expect{|
module type Vector_space =
  sig type t type scalar val scale : scalar -> t -> t end
|}];;

module type Scalar = sig
  type t
  include Vector_space with type t := t
                        and type scalar = t
end;;
[%%expect{|
module type Scalar =
  sig type t type scalar = t val scale : scalar -> t -> t end
|}];;

module type Linear_map = sig
  type ('a, 'b) t
  val scale :
    (module Vector_space with type t = 'a and type scalar = 'l) ->
    'l -> ('a, 'a) t
end;;
[%%expect{|
Uncaught exception: Invalid_argument("compare: Longident detected")

|}];;

module Primitive(Linear_map : Linear_map) = struct
  let f (type s) (s : (module Scalar with type t = s)) x =
    Linear_map.scale s x
end;;
[%%expect{|
Line 1, characters 30-40:
1 | module Primitive(Linear_map : Linear_map) = struct
                                  ^^^^^^^^^^
Error: Unbound module type "Linear_map"
|}];;
