/* JSON structure description using dependently typed combinators. */

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

exception Unexpected(string, string);
exception No_case_matched(list(exn));
exception Bad_array_size(int, int);
exception Missing_field(string);
exception Unexpected_field(string);
exception Bad_schema(exn);
exception Cannot_destruct((Json_query.path, exn));

/*-- types and errors --------------------------------------------------------*/

let unexpected = (kind, expected) => {
  let kind =
    switch (kind) {
    | `O([]) => "empty object"
    | `A([]) => "empty array"
    | `O(_) => "object"
    | `A(_) => "array"
    | `Null => "null"
    | `String(_) => "string"
    | `Float(_) => "number"
    | `Bool(_) => "boolean"
    };
  [@implicit_arity]
  Cannot_destruct([], [@implicit_arity] Unexpected(kind, expected));
};

type repr_agnostic_custom('t) = {
  write: 'rt. ((module Json_repr.Repr with type value = 'rt), 't) => 'rt,
  read: 'rf. ((module Json_repr.Repr with type value = 'rf), 'rf) => 't,
};

/* The GADT definition for encodings. This type must be kept internal
   because it does not encode all invariants. Some properties are
   checked at encoding construction time by smart constructors, since
   checking them would either be impossible, or would make the type
   too complex. In a few corners that involve custom encodings using
   user defined functions, some properties cannot be checked until
   construction/destruction time. If such a run time check fails, is
   denotes a programmer error and an [Invalid_argument] exceptions is
   thus raised. */
type encoding(_) =
  | Null: encoding(unit)
  | Empty: encoding(unit)
  | Ignore: encoding(unit)
  | Option(encoding('a)): encoding(option('a))
  | Constant(string): encoding(unit)
  | Int(int_encoding('a)): encoding('a)
  | Bool: encoding(bool)
  | String: encoding(string)
  | Float(option(bounds)): encoding(float)
  | Array(encoding('a)): encoding(array('a))
  | Obj(field('a)): encoding('a)
  | Objs(encoding('a), encoding('b)): encoding(('a, 'b))
  | Tup(encoding('a)): encoding('a)
  | Tups(encoding('a), encoding('b)): encoding(('a, 'b))
  | Custom(repr_agnostic_custom('t), Json_schema.schema): encoding('t)
  | Conv('a => 'b, 'b => 'a, encoding('b), option(Json_schema.schema))
    : encoding('a)
  | Describe(describe('a)): encoding('a)
  | Mu(mu('a)): encoding('a)
  | Union(list(case('t))): encoding('t)

and describe('a) = {
  id: string,
  title: option(string),
  description: option(string),
  encoding: encoding('a),
}

and mu('a) = {
  id: string,
  title: option(string),
  description: option(string),
  self: encoding('a) => encoding('a),
}

and int_encoding('a) = {
  int_name: string,
  of_float: float => 'a,
  to_float: 'a => float,
  lower_bound: 'a,
  upper_bound: 'a,
}

and bounds = {
  float_name: string,
  minimum: float,
  maximum: float,
}

and field(_) =
  | Req(req('a)): field('a)
  | Opt(opt('a)): field(option('a))
  | Dft(dft('a)): field('a)

and req('a) = {
  name: string,
  encoding: encoding('a),
  title: option(string),
  description: option(string),
}

and opt('a) = {
  name: string,
  encoding: encoding('a),
  title: option(string),
  description: option(string),
}

and dft('a) = {
  name: string,
  encoding: encoding('a),
  title: option(string),
  description: option(string),
  default: 'a,
}

and case('t) =
  | Case(case_case('a, 't)): case('t)

and case_case('a, 't) = {
  encoding: encoding('a),
  proj: 't => option('a),
  inj: 'a => 't,
};

/*-- construct / destruct / schema over the main GADT forms ------------------*/

module Make = (Repr: Json_repr.Repr) => {
  let construct = (enc, v) => {
    let rec construct: type t. (encoding(t), t) => Repr.value =
      fun
      | Null => (() => Repr.repr(`Null))
      | Empty => (() => Repr.repr(`O([])))
      | Ignore => (() => Repr.repr(`O([])))
      | Option(t) => (
          fun
          | None => Repr.repr(`Null)
          | Some(v) => construct(t, v)
        )
      | Constant(str) => (() => Repr.repr(`String(str)))
      | Int({int_name, to_float, lower_bound, upper_bound}) => (
          (i: t) => {
            if (i < lower_bound || i > upper_bound) {
              invalid_arg(
                "Json_encoding.construct: " ++ int_name ++ " out of range",
              );
            };
            Repr.repr(`Float(to_float(i)));
          }
        )
      | Bool => ((b: t) => Repr.repr(`Bool(b)))
      | String => (s => Repr.repr(`String(s)))
      | Float(Some({minimum, maximum, float_name})) => {
          let err =
            "Json_encoding.construct: " ++ float_name ++ " out of range";
          (
            float => {
              if (float < minimum || float > maximum) {
                invalid_arg(err);
              };
              Repr.repr(`Float(float));
            }
          );
        }
      | Float(None) => (float => Repr.repr(`Float(float)))
      | Describe({encoding: t}) => construct(t)
      | [@implicit_arity] Custom({write}, _) => (
          (j: t) => write((module Repr), j)
        )
      | [@implicit_arity] Conv(ffrom, _, t, _) => (
          v => construct(t, ffrom(v))
        )
      | Mu({self}) as enc => construct(self(enc))
      | Array(t) => {
          let w = v => construct(t, v);
          (arr => Repr.repr(`A(Array.to_list(Array.map(w, arr)))));
        }
      | Obj(Req({name: n, encoding: t})) => {
          let w = v => construct(t, v);
          (v => Repr.repr(`O([(n, w(v))])));
        }
      | Obj(Dft({name: n, encoding: t, default: d})) => {
          let w = v => construct(t, v);
          (
            v =>
              Repr.repr(
                `O(
                  if (v != d) {
                    [(n, w(v))];
                  } else {
                    [];
                  },
                ),
              )
          );
        }
      | Obj(Opt({name: n, encoding: t})) => {
          let w = v => construct(t, v);
          (
            fun
            | None => Repr.repr(`O([]))
            | Some(v) => Repr.repr(`O([(n, w(v))]))
          );
        }
      | [@implicit_arity] Objs(o1, o2) => {
          let w1 = v => construct(o1, v);
          let w2 = v => construct(o2, v);
          (
            fun
            | (v1, v2) =>
              switch (Repr.view(w1(v1)), Repr.view(w2(v2))) {
              | (`O(l1), `O(l2)) => Repr.repr(`O(l1 @ l2))
              | (`Null, `Null)
              | _ =>
                invalid_arg(
                  "Json_encoding.construct: consequence of bad merge_objs",
                )
              }
          );
        }
      | Tup(t) => {
          let w = v => construct(t, v);
          (v => Repr.repr(`A([w(v)])));
        }
      | [@implicit_arity] Tups(o1, o2) => {
          let w1 = v => construct(o1, v);
          let w2 = v => construct(o2, v);
          (
            fun
            | (v1, v2) =>
              switch (Repr.view(w1(v1)), Repr.view(w2(v2))) {
              | (`A(l1), `A(l2)) => Repr.repr(`A(l1 @ l2))
              | _ =>
                invalid_arg(
                  "Json_encoding.construct: consequence of bad merge_tups",
                )
              }
          );
        }
      | Union(cases) => (
          v => {
            let rec do_cases =
              fun
              | [] =>
                invalid_arg(
                  "Json_encoding.construct: consequence of bad union",
                )
              | [Case({encoding, proj}), ...rest] =>
                switch (proj(v)) {
                | Some(v) => construct(encoding, v)
                | None => do_cases(rest)
                };
            do_cases(cases);
          }
        );
    construct(enc, v);
  };

  let rec destruct: type t. (encoding(t), Repr.value) => t =
    fun
    | Null => (
        v =>
          switch (Repr.view(v)) {
          | `Null => ()
          | k => raise(unexpected(k, "null"))
          }
      )
    | Empty => (
        v =>
          switch (Repr.view(v)) {
          | `O([]) => ()
          | `O([(f, _)]) =>
            raise(
              [@implicit_arity] Cannot_destruct([], Unexpected_field(f)),
            )
          | k => raise @@ unexpected(k, "an empty object")
          }
      )
    | Ignore => (
        v =>
          switch (Repr.view(v)) {
          | _ => ()
          }
      )
    | Option(t) => (
        v =>
          switch (Repr.view(v)) {
          | `Null => None
          | _ => Some(destruct(t, v))
          }
      )
    | Constant(str) => (
        v =>
          switch (Repr.view(v)) {
          | `String(s) when s == str => ()
          | x => raise @@ unexpected(x, str)
          }
      )
    | Int({int_name, of_float, to_float, lower_bound, upper_bound}) => {
        let lower_bound = to_float(lower_bound);
        let upper_bound = to_float(upper_bound);
        (
          v =>
            switch (Repr.view(v)) {
            | `Float(v) =>
              let (rest, v) = modf(v);
              if (rest != 0.) {
                let exn =
                  Failure(int_name ++ " cannot have a fractional part");
                raise([@implicit_arity] Cannot_destruct([], exn));
              };
              if (v < lower_bound || v > upper_bound) {
                let exn = Failure(int_name ++ " out of range");
                raise([@implicit_arity] Cannot_destruct([], exn));
              };
              of_float(v);
            | k => raise(unexpected(k, "number"))
            }
        );
      }
    | Bool => (
        v =>
          switch (Repr.view(v)) {
          | `Bool(b) => (b: t)
          | k => raise(unexpected(k, "boolean"))
          }
      )
    | String => (
        v =>
          switch (Repr.view(v)) {
          | `String(s) => s
          | k => raise(unexpected(k, "string"))
          }
      )
    | Float(None) => (
        v =>
          switch (Repr.view(v)) {
          | `Float(f) => f
          | k => raise(unexpected(k, "float"))
          }
      )
    | Float(Some({minimum, maximum, float_name})) => (
        v =>
          switch (Repr.view(v)) {
          | `Float(f) =>
            if (f < minimum || f > maximum) {
              let exn = Failure(float_name ++ " out of range");
              raise([@implicit_arity] Cannot_destruct([], exn));
            } else {
              f;
            }
          | k => raise(unexpected(k, "float"))
          }
      )
    | Describe({encoding: t}) => destruct(t)
    | [@implicit_arity] Custom({read}, _) => read((module Repr))
    | [@implicit_arity] Conv(_, fto, t, _) => (v => fto(destruct(t, v)))
    | Mu({self}) as enc => destruct(self(enc))
    | Array(t) => (
        v =>
          switch (Repr.view(v)) {
          | `O([]) =>
            /* Weak `Repr`s like BSON don't know the difference  */
            [||]
          | `A(cells) =>
            Array.mapi(
              (i, cell) =>
                try(destruct(t, cell)) {
                | [@implicit_arity] Cannot_destruct(path, err) =>
                  raise(
                    [@implicit_arity]
                    Cannot_destruct([`Index(i), ...path], err),
                  )
                },
              Array.of_list(cells),
            )
          | k => raise @@ unexpected(k, "array")
          }
      )
    | Obj(_) as t => {
        let d = destruct_obj(t);
        (
          v =>
            switch (Repr.view(v)) {
            | `O(fields) =>
              let (r, rest, ign) = d(fields);
              switch (rest) {
              | [(field, _), ..._] when !ign =>
                raise @@ Unexpected_field(field)
              | _ => r
              };
            | k => raise @@ unexpected(k, "object")
            }
        );
      }
    | Objs(_) as t => {
        let d = destruct_obj(t);
        (
          v =>
            switch (Repr.view(v)) {
            | `O(fields) =>
              let (r, rest, ign) = d(fields);
              switch (rest) {
              | [(field, _), ..._] when !ign =>
                raise @@ Unexpected_field(field)
              | _ => r
              };
            | k => raise @@ unexpected(k, "object")
            }
        );
      }
    | Tup(_) as t => {
        let (r, i) = destruct_tup(0, t);
        (
          v =>
            switch (Repr.view(v)) {
            | `A(cells) =>
              let cells = Array.of_list(cells);
              let len = Array.length(cells);
              if (i != Array.length(cells)) {
                raise(
                  [@implicit_arity]
                  Cannot_destruct(
                    [],
                    [@implicit_arity] Bad_array_size(len, i),
                  ),
                );
              } else {
                r(cells);
              };
            | k => raise @@ unexpected(k, "array")
            }
        );
      }
    | Tups(_) as t => {
        let (r, i) = destruct_tup(0, t);
        (
          v =>
            switch (Repr.view(v)) {
            | `A(cells) =>
              let cells = Array.of_list(cells);
              let len = Array.length(cells);
              if (i != Array.length(cells)) {
                raise(
                  [@implicit_arity]
                  Cannot_destruct(
                    [],
                    [@implicit_arity] Bad_array_size(len, i),
                  ),
                );
              } else {
                r(cells);
              };
            | k => raise @@ unexpected(k, "array")
            }
        );
      }
    | Union(cases) => (
        v => {
          let rec do_cases = errs =>
            fun
            | [] =>
              raise(
                [@implicit_arity]
                Cannot_destruct([], No_case_matched(List.rev(errs))),
              )
            | [Case({encoding, inj}), ...rest] =>
              try(inj(destruct(encoding, v))) {
              | err => do_cases([err, ...errs], rest)
              };
          do_cases([], cases);
        }
      )
  and destruct_tup:
    type t. (int, encoding(t)) => (array(Repr.value) => t, int) =
    (i, t) =>
      switch (t) {
      | Tup(t) => (
          (
            arr =>
              try(destruct(t, arr[i])) {
              | [@implicit_arity] Cannot_destruct(path, err) =>
                raise(
                  [@implicit_arity]
                  Cannot_destruct([`Index(i), ...path], err),
                )
              }
          ),
          succ(i),
        )
      | [@implicit_arity] Tups(t1, t2) =>
        let (r1, i) = destruct_tup(i, t1);
        let (r2, i) = destruct_tup(i, t2);
        ((arr => (r1(arr), r2(arr))), i);
      | [@implicit_arity] Conv(_, fto, t, _) =>
        let (r, i) = destruct_tup(i, t);
        ((arr => fto(r(arr))), i);
      | Mu({self}) as enc => destruct_tup(i, self(enc))
      | Describe({encoding}) => destruct_tup(i, encoding)
      | _ =>
        invalid_arg("Json_encoding.destruct: consequence of bad merge_tups")
      }
  and destruct_obj:
    type t.
      (encoding(t), list((string, Repr.value))) =>
      (t, list((string, Repr.value)), bool) =
    t => {
      let rec assoc = (acc, n) =>
        fun
        | [] => raise(Not_found)
        | [(f, v), ...rest] when n == f => (v, acc @ rest)
        | [oth, ...rest] => assoc([oth, ...acc], n, rest);
      switch (t) {
      | Empty => (fields => ((), fields, false))
      | Ignore => (fields => ((), fields, true))
      | Obj(Req({name: n, encoding: t})) => (
          fields =>
            try({
              let (v, rest) = assoc([], n, fields);
              (destruct(t, v), rest, false);
            }) {
            | Not_found =>
              raise([@implicit_arity] Cannot_destruct([], Missing_field(n)))
            | [@implicit_arity] Cannot_destruct(path, err) =>
              raise(
                [@implicit_arity]
                Cannot_destruct([`Field(n), ...path], err),
              )
            }
        )
      | Obj(Opt({name: n, encoding: t})) => (
          fields =>
            try({
              let (v, rest) = assoc([], n, fields);
              (Some(destruct(t, v)), rest, false);
            }) {
            | Not_found => (None, fields, false)
            | [@implicit_arity] Cannot_destruct(path, err) =>
              raise(
                [@implicit_arity]
                Cannot_destruct([`Field(n), ...path], err),
              )
            }
        )
      | Obj(Dft({name: n, encoding: t, default: d})) => (
          fields =>
            try({
              let (v, rest) = assoc([], n, fields);
              (destruct(t, v), rest, false);
            }) {
            | Not_found => (d, fields, false)
            | [@implicit_arity] Cannot_destruct(path, err) =>
              raise(
                [@implicit_arity]
                Cannot_destruct([`Field(n), ...path], err),
              )
            }
        )
      | [@implicit_arity] Objs(o1, o2) =>
        let d1 = destruct_obj(o1);
        let d2 = destruct_obj(o2);
        (
          fields => {
            let (r1, rest, ign1) = d1(fields);
            let (r2, rest, ign2) = d2(rest);
            ((r1, r2), rest, ign1 || ign2);
          }
        );
      | [@implicit_arity] Conv(_, fto, t, _) =>
        let d = destruct_obj(t);
        (
          fields => {
            let (r, rest, ign) = d(fields);
            (fto(r), rest, ign);
          }
        );
      | Mu({self}) as enc => destruct_obj(self(enc))
      | Describe({encoding}) => destruct_obj(encoding)
      | Union(cases) => (
          fields => {
            let rec do_cases = errs =>
              fun
              | [] =>
                raise(
                  [@implicit_arity]
                  Cannot_destruct([], No_case_matched(List.rev(errs))),
                )
              | [Case({encoding, inj}), ...rest] =>
                try({
                  let (r, rest, ign) = destruct_obj(encoding, fields);
                  (inj(r), rest, ign);
                }) {
                | err => do_cases([err, ...errs], rest)
                };
            do_cases([], cases);
          }
        )
      | _ =>
        invalid_arg("Json_encoding.destruct: consequence of bad merge_objs")
      };
    };

  let custom = (write, read, ~schema) => {
    let read: type tf. ((module Json_repr.Repr with type value = tf), tf) => 't =
      ((module Repr_f), repr) =>
        read(Json_repr.convert((module Repr_f), (module Repr), repr));
    let write:
      type tf. ((module Json_repr.Repr with type value = tf), 't) => tf =
      ((module Repr_f), v) =>
        Json_repr.convert((module Repr), (module Repr_f), write(v));
    [@implicit_arity] Custom({read, write}, schema);
  };
};

module Ezjsonm_encoding = Make(Json_repr.Ezjsonm);

let patch_description = (~title=?, ~description=?, elt: Json_schema.element) =>
  switch (title, description) {
  | (None, None) => elt
  | (Some(_), None) => {...elt, title}
  | (None, Some(_)) => {...elt, description}
  | (Some(_), Some(_)) => {...elt, title, description}
  };

let schema = (~definitions_path=?, encoding) => {
  open Json_schema;
  let sch = ref(any);
  let rec prod = (l1, l2) =>
    switch (l1) {
    | [] => []
    | [(l1, b1), ...es] =>
      List.map(((l2, b2)) => (l1 @ l2, b1 || b2), l2) @ prod(es, l2)
    };
  let rec object_schema:
    type t.
      encoding(t) =>
      list((list((string, element, bool, option(Json_repr.any))), bool)) =
    fun
    | [@implicit_arity] Conv(_, _, o, None) => object_schema(o)
    | Empty => [([], false)]
    | Ignore => [([], true)]
    | Obj(Req({name: n, encoding: t, title, description})) => [
        (
          [
            (
              n,
              patch_description(~title?, ~description?, schema(t)),
              true,
              None,
            ),
          ],
          false,
        ),
      ]
    | Obj(Opt({name: n, encoding: t, title, description})) => [
        (
          [
            (
              n,
              patch_description(~title?, ~description?, schema(t)),
              false,
              None,
            ),
          ],
          false,
        ),
      ]
    | Obj(Dft({name: n, encoding: t, title, description, default: d})) => {
        let d =
          Json_repr.repr_to_any(
            (module Json_repr.Ezjsonm),
            Ezjsonm_encoding.construct(t, d),
          );
        [
          (
            [
              (
                n,
                patch_description(~title?, ~description?, schema(t)),
                false,
                Some(d),
              ),
            ],
            false,
          ),
        ];
      }
    | [@implicit_arity] Objs(o1, o2) =>
      prod(object_schema(o1), object_schema(o2))
    | Union([]) => invalid_arg("Json_encoding.schema: empty union in object")
    | Union(cases) =>
      List.flatten(
        List.map((Case({encoding: o})) => object_schema(o), cases),
      )
    | Mu({self}) as enc => object_schema(self(enc))
    | Describe({encoding: t}) => object_schema(t)
    | [@implicit_arity] Conv(_, _, _, Some(_)) /* FIXME: We could do better */
    | _ => invalid_arg("Json_encoding.schema: consequence of bad merge_objs")
  and array_schema: type t. encoding(t) => list(element) =
    fun
    | [@implicit_arity] Conv(_, _, o, None) => array_schema(o)
    | Tup(t) => [schema(t)]
    | [@implicit_arity] Tups(t1, t2) => array_schema(t1) @ array_schema(t2)
    | Mu({self}) as enc => array_schema(self(enc))
    | Describe({encoding: t}) => array_schema(t)
    | [@implicit_arity] Conv(_, _, _, Some(_)) /* FIXME: We could do better */
    | _ => invalid_arg("Json_encoding.schema: consequence of bad merge_tups")
  and schema: type t. encoding(t) => element =
    fun
    | Null => element(Null)
    | Empty =>
      element(Object({...object_specs, additional_properties: None}))
    | Ignore => element(Any)
    | Option(t) =>
      element(
        [@implicit_arity] Combine(One_of, [schema(t), element(Null)]),
      )
    | Int({to_float, lower_bound, upper_bound}) => {
        let minimum = Some((to_float(lower_bound), `Inclusive));
        let maximum = Some((to_float(upper_bound), `Inclusive));
        element(Integer({multiple_of: None, minimum, maximum}));
      }
    | Bool => element(Boolean)
    | Constant(str) => {
        ...element(String(string_specs)),
        enum: Some([Json_repr.to_any(`String(str))]),
      }
    | String => element(String(string_specs))
    | Float(Some({minimum, maximum})) =>
      element(
        Number({
          multiple_of: None,
          minimum: Some((minimum, `Inclusive)),
          maximum: Some((maximum, `Inclusive)),
        }),
      )
    | Float(None) => element(Number(numeric_specs))
    | Describe({id: name, title, description, encoding}) => {
        let schema =
          patch_description(~title?, ~description?, schema(encoding));
        let (s, def) =
          add_definition(~definitions_path?, name, schema, sch^);
        sch := fst(merge_definitions((sch^, s)));
        def;
      }
    | [@implicit_arity] Custom(_, s) => {
        sch := fst(merge_definitions((sch^, s)));
        root(s);
      }
    | [@implicit_arity] Conv(_, _, _, Some(s)) => {
        sch := fst(merge_definitions((sch^, s)));
        root(s);
      }
    | [@implicit_arity] Conv(_, _, t, None) => schema(t)
    | Mu({id: name, title, description, self: f}) => {
        let fake_schema =
          if (definition_exists(~definitions_path?, name, sch^)) {
            update(definition_ref(~definitions_path?, name), sch^);
          } else {
            let (sch, elt) =
              add_definition(~definitions_path?, name, element(Dummy), sch^);
            update(elt, sch);
          };
        let fake_self =
          [@implicit_arity]
          Custom(
            {write: (_, _) => assert(false), read: _ => assert(false)},
            fake_schema,
          );
        let root =
          patch_description(~title?, ~description?, schema(f(fake_self)));
        let (nsch, def) =
          add_definition(~definitions_path?, name, root, sch^);
        sch := nsch;
        def;
      }
    | Array(t) =>
      element([@implicit_arity] Monomorphic_array(schema(t), array_specs))
    | Objs(_) as o =>
      switch (object_schema(o)) {
      | [(properties, ext)] =>
        let additional_properties =
          if (ext) {
            Some(element(Any));
          } else {
            None;
          };
        element(
          Object({...object_specs, properties, additional_properties}),
        );
      | more =>
        let elements =
          List.map(
            ((properties, ext)) => {
              let additional_properties =
                if (ext) {
                  Some(element(Any));
                } else {
                  None;
                };
              element(
                Object({...object_specs, properties, additional_properties}),
              );
            },
            more,
          );
        element([@implicit_arity] Combine(One_of, elements));
      }
    | Obj(_) as o =>
      switch (object_schema(o)) {
      | [(properties, ext)] =>
        let additional_properties =
          if (ext) {
            Some(element(Any));
          } else {
            None;
          };
        element(
          Object({...object_specs, properties, additional_properties}),
        );
      | more =>
        let elements =
          List.map(
            ((properties, ext)) => {
              let additional_properties =
                if (ext) {
                  Some(element(Any));
                } else {
                  None;
                };
              element(
                Object({...object_specs, properties, additional_properties}),
              );
            },
            more,
          );
        element([@implicit_arity] Combine(One_of, elements));
      }
    | Tup(_) as t =>
      element([@implicit_arity] Array(array_schema(t), array_specs))
    | Tups(_) as t =>
      element([@implicit_arity] Array(array_schema(t), array_specs))
    | Union(cases) => {
        /* FIXME: smarter merge */
        let elements =
          List.map((Case({encoding})) => schema(encoding), cases);
        element([@implicit_arity] Combine(One_of, elements));
      };
  let schema = schema(encoding);
  update(schema, sch^);
};

/*-- utility wrappers over the GADT ------------------------------------------*/

let req = (~title=?, ~description=?, n, t) =>
  Req({name: n, encoding: t, title, description});
let opt = (~title=?, ~description=?, n, t) =>
  Opt({name: n, encoding: t, title, description});
let dft = (~title=?, ~description=?, n, t, d) =>
  Dft({name: n, encoding: t, title, description, default: d});

let mu = (name, ~title=?, ~description=?, self) =>
  Mu({id: name, title, description, self});
let null = Null;
let int =
  Int({
    int_name: "int",
    of_float: int_of_float,
    to_float: float_of_int,
    /* cross-platform consistent OCaml ints */
    lower_bound: - (1 lsl 30),
    upper_bound: 1 lsl 30 - 1,
  });
let ranged_int = (~minimum as lower_bound, ~maximum as upper_bound, name) => {
  if (Sys.word_size == 64
      && (lower_bound < - (1 lsl 30) || upper_bound > 1 lsl 30 - 1)) {
    invalid_arg(
      "Json_encoding.ranged_int: bounds out of portable int31 range",
    );
  };
  Int({
    int_name: name,
    of_float: int_of_float,
    to_float: float_of_int,
    lower_bound,
    upper_bound,
  });
};

let int53 =
  Int({
    int_name: "int53",
    of_float: Int64.of_float,
    to_float: Int64.to_float,
    lower_bound: Int64.neg(Int64.shift_left(1L, 53)),
    upper_bound: Int64.shift_left(1L, 53),
  });
let ranged_int53 = (~minimum as lower_bound, ~maximum as upper_bound, name) => {
  if (lower_bound < Int64.neg(Int64.shift_left(1L, 53))
      || upper_bound > Int64.shift_left(1L, 53)) {
    invalid_arg(
      "Json_encoding.ranged_int53: bounds out of JSON-representable integers",
    );
  };
  Int({
    int_name: name,
    of_float: Int64.of_float,
    to_float: Int64.to_float,
    lower_bound,
    upper_bound,
  });
};

let int32 =
  Int({
    int_name: "int32",
    of_float: Int32.of_float,
    to_float: Int32.to_float,
    lower_bound: Int32.min_int,
    upper_bound: Int32.max_int,
  });
let ranged_int32 = (~minimum as lower_bound, ~maximum as upper_bound, name) =>
  Int({
    int_name: name,
    of_float: Int32.of_float,
    to_float: Int32.to_float,
    lower_bound,
    upper_bound,
  });

let ranged_float = (~minimum, ~maximum, float_name) =>
  Float(Some({minimum, maximum, float_name}));

let float = Float(None);
let string = String;
let conv = (ffrom, fto, ~schema=?, t) =>
  [@implicit_arity] Conv(ffrom, fto, t, schema);
let bytes =
  [@implicit_arity] Conv(Bytes.to_string, Bytes.of_string, string, None);
let bool = Bool;
let array = t => Array(t);
let obj1 = f1 => Obj(f1);
let obj2 = (f1, f2) => [@implicit_arity] Objs(Obj(f1), Obj(f2));
let obj3 = (f1, f2, f3) =>
  conv(
    ((a, b, c)) => (a, (b, c)),
    ((a, (b, c))) => (a, b, c),
    [@implicit_arity]
    Objs(Obj(f1), [@implicit_arity] Objs(Obj(f2), Obj(f3))),
  );
let obj4 = (f1, f2, f3, f4) =>
  conv(
    ((a, b, c, d)) => (a, (b, (c, d))),
    ((a, (b, (c, d)))) => (a, b, c, d),
    [@implicit_arity]
    Objs(
      Obj(f1),
      [@implicit_arity]
      Objs(Obj(f2), [@implicit_arity] Objs(Obj(f3), Obj(f4))),
    ),
  );
let obj5 = (f1, f2, f3, f4, f5) =>
  conv(
    ((a, b, c, d, e)) => (a, (b, (c, (d, e)))),
    ((a, (b, (c, (d, e))))) => (a, b, c, d, e),
    [@implicit_arity]
    Objs(
      Obj(f1),
      [@implicit_arity]
      Objs(
        Obj(f2),
        [@implicit_arity]
        Objs(Obj(f3), [@implicit_arity] Objs(Obj(f4), Obj(f5))),
      ),
    ),
  );
let obj6 = (f1, f2, f3, f4, f5, f6) =>
  conv(
    ((a, b, c, d, e, f)) => (a, (b, (c, (d, (e, f))))),
    ((a, (b, (c, (d, (e, f)))))) => (a, b, c, d, e, f),
    [@implicit_arity]
    Objs(
      Obj(f1),
      [@implicit_arity]
      Objs(
        Obj(f2),
        [@implicit_arity]
        Objs(
          Obj(f3),
          [@implicit_arity]
          Objs(Obj(f4), [@implicit_arity] Objs(Obj(f5), Obj(f6))),
        ),
      ),
    ),
  );
let obj7 = (f1, f2, f3, f4, f5, f6, f7) =>
  conv(
    ((a, b, c, d, e, f, g)) => (a, (b, (c, (d, (e, (f, g)))))),
    ((a, (b, (c, (d, (e, (f, g))))))) => (a, b, c, d, e, f, g),
    {
      let rest = [@implicit_arity] Objs(Obj(f6), Obj(f7));
      [@implicit_arity]
      Objs(
        Obj(f1),
        [@implicit_arity]
        Objs(
          Obj(f2),
          [@implicit_arity]
          Objs(
            Obj(f3),
            [@implicit_arity]
            Objs(Obj(f4), [@implicit_arity] Objs(Obj(f5), rest)),
          ),
        ),
      );
    },
  );
let obj8 = (f1, f2, f3, f4, f5, f6, f7, f8) =>
  conv(
    ((a, b, c, d, e, f, g, h)) =>
      (a, (b, (c, (d, (e, (f, (g, h))))))),
    ((a, (b, (c, (d, (e, (f, (g, h)))))))) =>
      (a, b, c, d, e, f, g, h),
    {
      let rest =
        [@implicit_arity]
        Objs(Obj(f6), [@implicit_arity] Objs(Obj(f7), Obj(f8)));
      [@implicit_arity]
      Objs(
        Obj(f1),
        [@implicit_arity]
        Objs(
          Obj(f2),
          [@implicit_arity]
          Objs(
            Obj(f3),
            [@implicit_arity]
            Objs(Obj(f4), [@implicit_arity] Objs(Obj(f5), rest)),
          ),
        ),
      );
    },
  );
let obj9 = (f1, f2, f3, f4, f5, f6, f7, f8, f9) =>
  conv(
    ((a, b, c, d, e, f, g, h, i)) =>
      (a, (b, (c, (d, (e, (f, (g, (h, i)))))))),
    ((a, (b, (c, (d, (e, (f, (g, (h, i))))))))) =>
      (a, b, c, d, e, f, g, h, i),
    {
      let rest =
        [@implicit_arity]
        Objs(
          Obj(f6),
          [@implicit_arity]
          Objs(Obj(f7), [@implicit_arity] Objs(Obj(f8), Obj(f9))),
        );
      [@implicit_arity]
      Objs(
        Obj(f1),
        [@implicit_arity]
        Objs(
          Obj(f2),
          [@implicit_arity]
          Objs(
            Obj(f3),
            [@implicit_arity]
            Objs(Obj(f4), [@implicit_arity] Objs(Obj(f5), rest)),
          ),
        ),
      );
    },
  );
let obj10 = (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) =>
  conv(
    ((a, b, c, d, e, f, g, h, i, j)) =>
      (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))),
    ((a, (b, (c, (d, (e, (f, (g, (h, (i, j)))))))))) =>
      (a, b, c, d, e, f, g, h, i, j),
    {
      let rest =
        [@implicit_arity]
        Objs(
          Obj(f6),
          [@implicit_arity]
          Objs(
            Obj(f7),
            [@implicit_arity]
            Objs(Obj(f8), [@implicit_arity] Objs(Obj(f9), Obj(f10))),
          ),
        );
      [@implicit_arity]
      Objs(
        Obj(f1),
        [@implicit_arity]
        Objs(
          Obj(f2),
          [@implicit_arity]
          Objs(
            Obj(f3),
            [@implicit_arity]
            Objs(Obj(f4), [@implicit_arity] Objs(Obj(f5), rest)),
          ),
        ),
      );
    },
  );
let tup1 = f1 => Tup(f1);
let tup2 = (f1, f2) => [@implicit_arity] Tups(Tup(f1), Tup(f2));
let tup3 = (f1, f2, f3) =>
  conv(
    ((a, b, c)) => (a, (b, c)),
    ((a, (b, c))) => (a, b, c),
    [@implicit_arity]
    Tups(Tup(f1), [@implicit_arity] Tups(Tup(f2), Tup(f3))),
  );
let tup4 = (f1, f2, f3, f4) =>
  conv(
    ((a, b, c, d)) => (a, (b, (c, d))),
    ((a, (b, (c, d)))) => (a, b, c, d),
    [@implicit_arity]
    Tups(
      Tup(f1),
      [@implicit_arity]
      Tups(Tup(f2), [@implicit_arity] Tups(Tup(f3), Tup(f4))),
    ),
  );
let tup5 = (f1, f2, f3, f4, f5) =>
  conv(
    ((a, b, c, d, e)) => (a, (b, (c, (d, e)))),
    ((a, (b, (c, (d, e))))) => (a, b, c, d, e),
    [@implicit_arity]
    Tups(
      Tup(f1),
      [@implicit_arity]
      Tups(
        Tup(f2),
        [@implicit_arity]
        Tups(Tup(f3), [@implicit_arity] Tups(Tup(f4), Tup(f5))),
      ),
    ),
  );
let tup6 = (f1, f2, f3, f4, f5, f6) =>
  conv(
    ((a, b, c, d, e, f)) => (a, (b, (c, (d, (e, f))))),
    ((a, (b, (c, (d, (e, f)))))) => (a, b, c, d, e, f),
    [@implicit_arity]
    Tups(
      Tup(f1),
      [@implicit_arity]
      Tups(
        Tup(f2),
        [@implicit_arity]
        Tups(
          Tup(f3),
          [@implicit_arity]
          Tups(Tup(f4), [@implicit_arity] Tups(Tup(f5), Tup(f6))),
        ),
      ),
    ),
  );
let tup7 = (f1, f2, f3, f4, f5, f6, f7) =>
  conv(
    ((a, b, c, d, e, f, g)) => (a, (b, (c, (d, (e, (f, g)))))),
    ((a, (b, (c, (d, (e, (f, g))))))) => (a, b, c, d, e, f, g),
    {
      let rest = [@implicit_arity] Tups(Tup(f6), Tup(f7));
      [@implicit_arity]
      Tups(
        Tup(f1),
        [@implicit_arity]
        Tups(
          Tup(f2),
          [@implicit_arity]
          Tups(
            Tup(f3),
            [@implicit_arity]
            Tups(Tup(f4), [@implicit_arity] Tups(Tup(f5), rest)),
          ),
        ),
      );
    },
  );
let tup8 = (f1, f2, f3, f4, f5, f6, f7, f8) =>
  conv(
    ((a, b, c, d, e, f, g, h)) =>
      (a, (b, (c, (d, (e, (f, (g, h))))))),
    ((a, (b, (c, (d, (e, (f, (g, h)))))))) =>
      (a, b, c, d, e, f, g, h),
    {
      let rest =
        [@implicit_arity]
        Tups(Tup(f6), [@implicit_arity] Tups(Tup(f7), Tup(f8)));
      [@implicit_arity]
      Tups(
        Tup(f1),
        [@implicit_arity]
        Tups(
          Tup(f2),
          [@implicit_arity]
          Tups(
            Tup(f3),
            [@implicit_arity]
            Tups(Tup(f4), [@implicit_arity] Tups(Tup(f5), rest)),
          ),
        ),
      );
    },
  );
let tup9 = (f1, f2, f3, f4, f5, f6, f7, f8, f9) =>
  conv(
    ((a, b, c, d, e, f, g, h, i)) =>
      (a, (b, (c, (d, (e, (f, (g, (h, i)))))))),
    ((a, (b, (c, (d, (e, (f, (g, (h, i))))))))) =>
      (a, b, c, d, e, f, g, h, i),
    {
      let rest =
        [@implicit_arity]
        Tups(
          Tup(f6),
          [@implicit_arity]
          Tups(Tup(f7), [@implicit_arity] Tups(Tup(f8), Tup(f9))),
        );
      [@implicit_arity]
      Tups(
        Tup(f1),
        [@implicit_arity]
        Tups(
          Tup(f2),
          [@implicit_arity]
          Tups(
            Tup(f3),
            [@implicit_arity]
            Tups(Tup(f4), [@implicit_arity] Tups(Tup(f5), rest)),
          ),
        ),
      );
    },
  );
let tup10 = (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) =>
  conv(
    ((a, b, c, d, e, f, g, h, i, j)) =>
      (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))),
    ((a, (b, (c, (d, (e, (f, (g, (h, (i, j)))))))))) =>
      (a, b, c, d, e, f, g, h, i, j),
    {
      let rest =
        [@implicit_arity]
        Tups(
          Tup(f6),
          [@implicit_arity]
          Tups(
            Tup(f7),
            [@implicit_arity]
            Tups(Tup(f8), [@implicit_arity] Tups(Tup(f9), Tup(f10))),
          ),
        );
      [@implicit_arity]
      Tups(
        Tup(f1),
        [@implicit_arity]
        Tups(
          Tup(f2),
          [@implicit_arity]
          Tups(
            Tup(f3),
            [@implicit_arity]
            Tups(Tup(f4), [@implicit_arity] Tups(Tup(f5), rest)),
          ),
        ),
      );
    },
  );

let repr_agnostic_custom = ({write, read}, ~schema) =>
  [@implicit_arity] Custom({write, read}, schema);

let constant = s => Constant(s);

let def = (id, ~title=?, ~description=?, encoding) =>
  Describe({id, title, description, encoding});

let rec is_nullable: type t. encoding(t) => bool =
  fun
  | Constant(_) => false
  | Int(_) => false
  | Float(_) => false
  | Array(_) => false
  | Empty => false
  | String => false
  | Bool => false
  | Obj(_) => false
  | Tup(_) => false
  | Objs(_) => false
  | Tups(_) => false
  | Null => true
  | Ignore => true
  | Option(_) => true
  | [@implicit_arity] Conv(_, _, t, _) => is_nullable(t)
  | Union(cases) =>
    List.exists((Case({encoding: t})) => is_nullable(t), cases)
  | Describe({encoding: t}) => is_nullable(t)
  | Mu({self}) as enc => is_nullable(self(enc))
  | [@implicit_arity] Custom(_, sch) => Json_schema.is_nullable(sch);

let option: type t. encoding(t) => encoding(option(t)) =
  t => {
    if (is_nullable(t)) {
      invalid_arg("Json_encoding.option: cannot nest nullable encodings");
    };
    Option(t);
  };

let any_value = {
  let read = (repr, v) => Json_repr.repr_to_any(repr, v);
  let write = (repr, v) => Json_repr.any_to_repr(repr, v);
  [@implicit_arity] Custom({read, write}, Json_schema.any);
};

let any_ezjson_value = {
  let read = (repr, v) =>
    Json_repr.convert(repr, (module Json_repr.Ezjsonm), v);
  let write = (repr, v) =>
    Json_repr.convert((module Json_repr.Ezjsonm), repr, v);
  [@implicit_arity] Custom({read, write}, Json_schema.any);
};

let any_document = {
  let read:
    type tt.
      ((module Json_repr.Repr with type value = tt), tt) => Json_repr.any =
    ((module Repr), v) =>
      switch (Repr.view(v)) {
      | `A(_)
      | `O(_) => Json_repr.repr_to_any((module Repr), v)
      | k => raise @@ unexpected(k, "array or object")
      };
  let write = (repr, v) => Json_repr.any_to_repr(repr, v);
  [@implicit_arity] Custom({read, write}, Json_schema.any);
};

let any_schema =
  Ezjsonm_encoding.custom(
    Json_schema.to_json,
    j =>
      try(Json_schema.of_json(j)) {
      | err => raise([@implicit_arity] Cannot_destruct([], Bad_schema(err)))
      },
    ~schema=Json_schema.self,
  );

let merge_tups = (t1, t2) => {
  let rec is_tup: type t. encoding(t) => bool =
    fun
    | Tup(_) => true
    | Tups(_) /* by construction */ => true
    | [@implicit_arity] Conv(_, _, t, None) => is_tup(t)
    | Mu({self}) as enc => is_tup(self(enc))
    | Describe({encoding: t}) => is_tup(t)
    | _ => false;
  if (is_tup(t1) && is_tup(t2)) {
    [@implicit_arity] Tups(t1, t2);
  } else {
    invalid_arg("Json_encoding.merge_tups");
  };
};

let list = t =>
  [@implicit_arity] Conv(Array.of_list, Array.to_list, Array(t), None);

let merge_objs = (o1, o2) => {
  /* FIXME: check fields unicity */
  let rec is_obj: type t. encoding(t) => bool =
    fun
    | Obj(_) => true
    | Objs(_) /* by construction */ => true
    | [@implicit_arity] Conv(_, _, t, None) => is_obj(t)
    | Empty => true
    | Ignore => true
    | Union(cases) =>
      List.for_all((Case({encoding: o})) => is_obj(o), cases)
    | Mu({self}) as enc => is_obj(self(enc))
    | Describe({encoding: t}) => is_obj(t)
    | _ => false;
  if (is_obj(o1) && is_obj(o2)) {
    [@implicit_arity] Objs(o1, o2);
  } else {
    invalid_arg("Json_encoding.merge_objs");
  };
};

let empty = Empty;

let unit = Ignore;

let case = (encoding, proj, inj) => Case({encoding, proj, inj});

let union =
  fun
  | [] => invalid_arg("Json_encoding.union")
  | cases =>
    /* FIXME: check mutual exclusion */
    Union(cases);

let rec print_error = (~print_unknown=?, ppf) =>
  fun
  | [@implicit_arity] Cannot_destruct([], exn) =>
    print_error(~print_unknown?, ppf, exn)
  | [@implicit_arity]
    Cannot_destruct(path, [@implicit_arity] Unexpected(unex, ex)) =>
    Format.fprintf(
      ppf,
      "At %a, unexpected %s instead of %s",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      unex,
      ex,
    )
  | [@implicit_arity] Cannot_destruct(path, No_case_matched(errs)) =>
    Format.fprintf(
      ppf,
      "@[<v 2>At %a, no case matched:@,%a@]",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      Format.pp_print_list(print_error(~print_unknown?)),
      errs,
    )
  | [@implicit_arity]
    Cannot_destruct(path, [@implicit_arity] Bad_array_size(unex, ex)) =>
    Format.fprintf(
      ppf,
      "At %a, unexpected array of size %d instead of %d",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      unex,
      ex,
    )
  | [@implicit_arity] Cannot_destruct(path, Missing_field(n)) =>
    Format.fprintf(
      ppf,
      "At %a, missing object field %s",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      n,
    )
  | [@implicit_arity] Cannot_destruct(path, Unexpected_field(n)) =>
    Format.fprintf(
      ppf,
      "At %a, unexpected object field %s",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      n,
    )
  | [@implicit_arity] Cannot_destruct(path, Bad_schema(exn)) =>
    Format.fprintf(
      ppf,
      "@[<v 2>At %a, bad custom schema:@,%a@]",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      print_error(~print_unknown?),
      exn,
    )
  | [@implicit_arity] Unexpected(unex, ex) =>
    Format.fprintf(ppf, "Unexpected %s instead of %s", unex, ex)
  | No_case_matched(errs) =>
    Format.fprintf(
      ppf,
      "@[<v 2>No case matched:@,%a@]",
      Format.pp_print_list(print_error(~print_unknown?)),
      errs,
    )
  | [@implicit_arity] Bad_array_size(unex, ex) =>
    Format.fprintf(ppf, "Unexpected array of size %d instead of %d", unex, ex)
  | Missing_field(n) => Format.fprintf(ppf, "Missing object field %s", n)
  | Unexpected_field(n) =>
    Format.fprintf(ppf, "Unexpected object field %s", n)
  | Bad_schema(exn) =>
    Format.fprintf(
      ppf,
      "@[<v 2>bad custom schema:@,%a@]",
      print_error(~print_unknown?),
      exn,
    )
  | [@implicit_arity] Cannot_destruct(path, exn) =>
    Format.fprintf(
      ppf,
      "@[<v 2>At %a:@,%a@]",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      print_error(~print_unknown?),
      exn,
    )
  | exn => Json_schema.print_error(~print_unknown?, ppf, exn);

include Ezjsonm_encoding;
