/* Representations of JSON documents */

/************************************************************************/
/*  ocplib-json-typed                                                   */
/*                                                                      */
/*    Copyright 2014 OCamlPro                                           */
/*                                                                      */
/*  This file is distributed under the terms of the GNU Lesser General  */
/*  Public License as published by the Free Software Foundation; either */
/*  version 2.1 of the License, or (at your option) any later version,  */
/*  with the OCaml static compilation exception.                        */
/*                                                                      */
/*  ocplib-json-typed is distributed in the hope that it will be useful,*/
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*  GNU General Public License for more details.                        */
/*                                                                      */
/************************************************************************/

type view('a) = [
  | `O(list((string, 'a)))
  | `A(list('a))
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `Null
];

type repr_uid('a) = ref(option('a));
/* This is used for limiting conversions. When a value is converted
   from a representation to another, which mostly happens when using
   the {!type:any} boxing, such as when writing custom encodings, the
   original value is usually traversed using the [view] of the
   original representation, and recreated using the [repr] of the
   destination representation. When converting from a representation
   to itself, we want to optimize out this transformation, that is a
   deep copy, and just get the same value. For this, we have to prove
   to OCaml that it is indeed a value from the same representation.
   To do that, we use the following trick. Each representation has a
   bucket, the uid below. When converting from the original
   representation, we put the value in its bucket. Then, we check the
   bucket of the destination, and if it happens to be occupied, we
   find in it the original value, under the destination type. Voilà. */
let repr_uid = () => ref(None);
let eq_repr_uid: ('a, repr_uid('a), repr_uid('b)) => option('b) = (
  (a, ta, tb) => {
    tb := None;
    ta := Some(a);
    tb^;
  }:
    ('a, repr_uid('a), repr_uid('b)) => option('b)
);

module type Repr = {
  type value;
  let view: value => view(value);
  let repr: view(value) => value;
  let repr_uid: repr_uid(value);
};

module Ezjsonm = {
  type value = [
    | `O(list((string, value)))
    | `A(list(value))
    | `Bool(bool)
    | `Float(float)
    | `String(string)
    | `Null
  ];
  let view = v => v;
  let repr = v => v;
  let repr_uid = repr_uid();
};

type ezjsonm = Ezjsonm.value;

module Yojson = {
  type value = [
    | `Bool(bool)
    | `Assoc(list((string, value)))
    | `Float(float)
    | `Int(int)
    | `Intlit(string)
    | `List(list(value))
    | `Null
    | `String(string)
    | `Tuple(list(value))
    | `Variant(string, option(value))
  ];
  let view =
    fun
    | `Intlit(i) => `String(i)
    | `Tuple(l) => `A(l)
    | `Variant(label, Some(x)) => `A([`String(label), x])
    | `Variant(label, None) => `String(label)
    | `Assoc(l) => `O(l)
    | `List(l) => `A(l)
    | `Int(i) => `Float(float(i))
    | `Float(f) => `Float(f)
    | `String(s) => `String(s)
    | `Null => `Null
    | `Bool(b) => `Bool(b);
  let repr =
    fun
    | `O(l) => `Assoc(l)
    | `A(l) => `List(l)
    | `Bool(b) => `Bool(b)
    | `Float(f) => `Float(f)
    | `String(s) => `String(s)
    | `Null => `Null;
  let repr_uid = repr_uid();
};

type yojson = Yojson.value;

let convert:
  type tt tf.
    (
      (module Repr with type value = tf),
      (module Repr with type value = tt),
      tf
    ) =>
    tt =
  ((module Repr_f), (module Repr_t), v) =>
    switch (eq_repr_uid(v, Repr_f.repr_uid, Repr_t.repr_uid)) {
    | Some(r) => r
    | None =>
      let rec conv = v =>
        switch (Repr_f.view(v)) {
        | (`Float(_) | `Bool(_) | `String(_) | `Null) as v => Repr_t.repr(v)
        | `A(values) => Repr_t.repr(`A(List.map(conv, values)))
        | `O(values) =>
          Repr_t.repr(`O(List.map(((k, v)) => (k, conv(v)), values)))
        };
      conv(v);
    };

let pp_string = (ppf, s) => {
  Format.fprintf(ppf, "\"");
  for (i in 0 to String.length(s) - 1) {
    switch (s.[i]) {
    | '"' => Format.fprintf(ppf, "\\\"")
    | '\n' => Format.fprintf(ppf, "\\n")
    | '\r' => Format.fprintf(ppf, "\\r")
    | '\b' => Format.fprintf(ppf, "\\b")
    | '\t' => Format.fprintf(ppf, "\\t")
    | '\\' => Format.fprintf(ppf, "\\\\")
    | '\000' .. '\031' as c => Format.fprintf(ppf, "\\u%04x", Char.code(c))
    | c => Format.fprintf(ppf, "%c", c)
    };
  };
  Format.fprintf(ppf, "\"");
};

let pp =
    (
      ~compact=false,
      ~pp_string=pp_string,
      type value,
      module Repr: Repr with type value = value,
      ppf,
      v: value,
    ) => {
  let rec pp_compact = (ppf, v) =>
    switch (Repr.view(v)) {
    | `O(l) =>
      let pp_sep = (ppf, ()) => Format.fprintf(ppf, ",");
      let pp_field = (ppf, (name, v)) =>
        Format.fprintf(ppf, "%a:%a", pp_string, name, pp_compact, v);
      Format.fprintf(
        ppf,
        "{%a}",
        Format.pp_print_list(~pp_sep, pp_field),
        l,
      );
    | `A(l) =>
      let pp_sep = (ppf, ()) => Format.fprintf(ppf, ",");
      Format.fprintf(
        ppf,
        "[%a]",
        Format.pp_print_list(~pp_sep, pp_compact),
        l,
      );
    | `Bool(true) => Format.fprintf(ppf, "true")
    | `Bool(false) => Format.fprintf(ppf, "false")
    | `Float(f) =>
      let (fract, intr) = modf(f);
      if (fract == 0.0) {
        Format.fprintf(ppf, "%.0f", intr);
      } else {
        Format.fprintf(ppf, "%g", f);
      };
    | `String(s) => pp_string(ppf, s)
    | `Null => Format.fprintf(ppf, "null")
    };
  let rec pp_box = (ppf, v) =>
    switch (Repr.view(v)) {
    | `O([]) => Format.fprintf(ppf, "{}")
    | `O(l) =>
      let pp_sep = (ppf, ()) => Format.fprintf(ppf, ",@ ");
      let pp_field = (ppf, (name, v)) =>
        Format.fprintf(ppf, "@[<hov 2>%a:@ %a@]", pp_string, name, pp_box, v);
      Format.fprintf(
        ppf,
        "@[<hov 2>{ %a }@]",
        Format.pp_print_list(~pp_sep, pp_field),
        l,
      );
    | `A([]) => Format.fprintf(ppf, "[]")
    | `A(l) =>
      let pp_sep = (ppf, ()) => Format.fprintf(ppf, ",@ ");
      Format.fprintf(
        ppf,
        "@[<hov 2>[ %a ]@]",
        Format.pp_print_list(~pp_sep, pp_box),
        l,
      );
    | _ => pp_compact(ppf, v)
    };
  if (compact) {
    pp_compact(ppf, v);
  } else {
    pp_box(ppf, v);
  };
};

let from_yojson = non_basic => {
  /* Delete `Variant, `Tuple and `Intlit */
  let rec to_basic = non_basic =>
    switch (non_basic) {
    | `Intlit(i) => `String(i)
    | `Tuple(l) => `List(List.map(to_basic, l))
    | `Variant(label, Some(x)) => `List([`String(label), to_basic(x)])
    | `Variant(label, None) => `String(label)
    | `Assoc(l) =>
      `Assoc(List.map(((key, value)) => (key, to_basic(value)), l))
    | `List(l) => `List(List.map(to_basic, l))
    | `Int(i) => `Int(i)
    | `Float(f) => `Float(f)
    | `String(s) => `String(s)
    | `Null => `Null
    | `Bool(b) => `Bool(b)
    };
  /* Rename `Assoc, `Int and `List */
  let rec to_value: 'a. _ => ([> ezjsonm] as 'a) =
    fun
    | `List(l) => `A(List.map(to_value, l))
    | `Assoc(l) =>
      `O(List.map(((key, value)) => (key, to_value(value)), l))
    | `Int(i) => `Float(float_of_int(i))
    | `Float(f) => `Float(f)
    | `Null => `Null
    | `String(s) => `String(s)
    | `Bool(b) => `Bool(b);
  to_basic((non_basic :> yojson)) |> to_value;
};

let to_yojson = json => {
  let rec aux: 'a. _ => ([> yojson] as 'a) =
    fun
    | `A(values) => `List(List.map(aux, values))
    | `O(values) => `Assoc(List.map(((k, v)) => (k, aux(v)), values))
    | `Float(f) => {
        let (fract, intr) = modf(f);
        let max_intf = float(0x3F_FF_FF_FF);
        let min_intf = -. max_intf -. 1.;
        if (fract == 0.0) {
          if (intr >= min_intf && intr <= max_intf) {
            `Int(int_of_float(intr));
          } else {
            `Intlit(Printf.sprintf("%.0f", intr));
          };
        } else {
          `Float(f);
        };
      }
    | `Bool(b) => `Bool(b)
    | `String(s) => `String(s)
    | `Null => `Null;
  aux((json :> ezjsonm));
};

type any =
  | Value_with_repr((module Repr with type value = 'a), 'a): any;

let pp_any =
    (
      ~compact=?,
      ~pp_string=?,
      (),
      ppf,
      [@implicit_arity] Value_with_repr(repr, v),
    ) =>
  pp(~compact?, ~pp_string?, repr, ppf, v);

let any_to_repr: type tt. ((module Repr with type value = tt), any) => tt =
  (repr_t, [@implicit_arity] Value_with_repr(repr_f, v)) =>
    convert(repr_f, repr_t, v);

let repr_to_any = (repr, v) => [@implicit_arity] Value_with_repr(repr, v);

let from_any: 'a. any => ([> ezjsonm] as 'a) =
  repr => {
    let res = any_to_repr((module Ezjsonm), repr);
    (res: ezjsonm :> [> ezjsonm]);
  };

let to_any = v =>
  [@implicit_arity] Value_with_repr((module Ezjsonm), (v :> ezjsonm));
