(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)
[@@@alert "-deprecated"]

module L = Longident
let mknoloc = Location.mknoloc

[%%expect {|
module L = Longident
val mknoloc : 'a -> 'a Location.loc = <fun>
|}]

let flatten_ident = L.flatten (L.Lident (mknoloc "foo"))
[%%expect {|
Line 1, characters 30-56:
1 | let flatten_ident = L.flatten (L.Lident (mknoloc "foo"))
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}]
let flatten_dot = L.flatten (L.Ldot (L.Lident(mknoloc "M"), mknoloc "foo"))
[%%expect {|
Line 1, characters 28-75:
1 | let flatten_dot = L.flatten (L.Ldot (L.Lident(mknoloc "M"), mknoloc "foo"))
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}]
let flatten_apply = L.flatten (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")))
[%%expect {|
Line 1, characters 30-89:
1 | let flatten_apply = L.flatten (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")))
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}]

let unflatten_empty = L.unflatten []
[%%expect {|
val unflatten_empty : L.t option = None
|}]
let unflatten_sing = L.unflatten ["foo"]
[%%expect {|
val unflatten_sing : L.t option =
  Some
   (`Lident
      {Location.txt = "foo";
       loc =
        {Location.loc_start =
          {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}})
|}]
let unflatten_dot = L.unflatten ["M"; "N"; "foo"]
[%%expect {|
val unflatten_dot : L.t option =
  Some
   (`Ldot
      (`Ldot
         (`Lident
            {Location.txt = "M";
             loc =
              {Location.loc_start =
                {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                 pos_cnum = -1};
               loc_end =
                {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                 pos_cnum = -1};
               loc_ghost = true}},
          {Location.txt = "N";
           loc =
            {Location.loc_start =
              {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
               pos_cnum = -1};
             loc_end =
              {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
               pos_cnum = -1};
             loc_ghost = true}}),
       {Location.txt = "foo";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}))
|}]

let last_ident = L.last (L.Lident (mknoloc "foo"))
[%%expect {|
Line 1, characters 24-50:
1 | let last_ident = L.last (L.Lident (mknoloc "foo"))
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}]
let last_dot = L.last (L.Ldot (L.Lident (mknoloc "M"), mknoloc "foo"))
[%%expect {|
Line 1, characters 22-70:
1 | let last_dot = L.last (L.Ldot (L.Lident (mknoloc "M"), mknoloc "foo"))
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}]
let last_apply = L.last (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")))
[%%expect {|
Line 1, characters 24-83:
1 | let last_apply = L.last (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")))
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}]
let last_dot_apply = L.last
    (L.Ldot (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")), mknoloc "foo"))
[%%expect {|
Line 2, characters 4-87:
2 |     (L.Ldot (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")), mknoloc "foo"))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is "L.t"
|}];;

type parse_result = { flat: L.t; spec:L.t; any_is_correct:bool }
let test specialized s =
  let spec = specialized (Lexing.from_string s) in
  { flat = L.parse s;
    spec;
    any_is_correct = Parse.longident (Lexing.from_string s) = spec;
  }

let parse_empty = L.parse ""
let parse_empty_val = Parse.longident (Lexing.from_string "")
[%%expect {|
type parse_result = { flat : L.t; spec : L.t; any_is_correct : bool; }
val test : (Lexing.lexbuf -> L.t) -> string -> parse_result = <fun>
val parse_empty : L.t =
  `Lident
    {Location.txt = "";
     loc =
      {Location.loc_start =
        {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
         pos_cnum = -1};
       loc_end =
        {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
         pos_cnum = -1};
       loc_ghost = true}}
Exception:
Syntaxerr.Error
 (Syntaxerr.Other
   {Location.loc_start =
     {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    loc_end =
     {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    loc_ghost = false}).
|}]
let parse_ident = test Parse.val_ident "foo"
[%%expect {|
Exception: Invalid_argument "compare: Longident detected".
|}]
let parse_dot = test Parse.val_ident "M.foo"
[%%expect {|
Exception: Invalid_argument "compare: Longident detected".
|}]
let parse_path = test Parse.val_ident "M.N.foo"
[%%expect {|
Exception: Invalid_argument "compare: Longident detected".
|}]
let parse_complex = test  Parse.type_ident "M.F(M.N).N.foo"
(* the result below is a known misbehavior of Longident.parse
   which does not handle applications properly. *)
[%%expect {|
Exception: Invalid_argument "compare: Longident detected".
|}]

let parse_op = test Parse.val_ident "M.(.%.()<-)"
(* the result below is another known misbehavior of Longident.parse. *)
[%%expect {|
Exception: Invalid_argument "compare: Longident detected".
|}]


let parse_let_op = test Parse.val_ident "M.(let+*!)"
[%%expect {|
Exception: Invalid_argument "compare: Longident detected".
|}]

let constr = test Parse.constr_ident "true"
[%%expect{|
Exception: Invalid_argument "compare: Longident detected".
|}]

let prefix_constr = test Parse.constr_ident "A.B.C.(::)"
[%%expect{|
Exception: Invalid_argument "compare: Longident detected".
|}]



let mod_ext = test Parse.extended_module_path "A.F(B.C(X)).G(Y).D"
[%%expect{|
Exception: Invalid_argument "compare: Longident detected".
|}]


let string_of_longident lid = Format.asprintf "%a" Pprintast.longident lid
[%%expect{|
val string_of_longident : Longident.t -> string = <fun>
|}]
let str_empty   = string_of_longident parse_empty
[%%expect {|
val str_empty : string = ""
|}]
let str_ident   = string_of_longident parse_ident.flat
[%%expect {|
Line 1, characters 38-49:
1 | let str_ident   = string_of_longident parse_ident.flat
                                          ^^^^^^^^^^^
Error: Unbound value "parse_ident"
|}]
let str_dot     = string_of_longident parse_dot.flat
[%%expect {|
Line 1, characters 38-47:
1 | let str_dot     = string_of_longident parse_dot.flat
                                          ^^^^^^^^^
Error: Unbound value "parse_dot"
|}]
let str_path    = string_of_longident parse_path.flat
[%%expect {|
Line 1, characters 38-48:
1 | let str_path    = string_of_longident parse_path.flat
                                          ^^^^^^^^^^
Error: Unbound value "parse_path"
|}]


let str_complex = string_of_longident
   (let (&.) p word = L.Ldot(p, mknoloc word) in
    L.Lapply(L.Lident (mknoloc "M") &. "F", L.Lident (mknoloc "M") &. "N") &. "N" &. "foo")
[%%expect{|
Line 2, characters 22-28:
2 |    (let (&.) p word = L.Ldot(p, mknoloc word) in
                          ^^^^^^
Error: Unbound constructor "L.Ldot"
|}]
