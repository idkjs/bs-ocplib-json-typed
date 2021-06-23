/* Abstract representation of JSON schemas. */

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

/* TODO: validator */

open Json_query;

/* The currently handled version */
let version = "http://json-schema.org/draft-04/schema#";

/*-- types -----------------------------------------------------------------*/

/* The root of a schema with the named definitions,
   a precomputed ID-element map and a cache for external documents. */
type schema = {
  root: element,
  source: string /* whose fragment should be empty */,
  definitions: list((path, element)),
  ids: list((string, element)),
  world: list(schema),
}

and element = {
  title: option(string),
  description: option(string),
  default: option(Json_repr.any),
  enum: option(list(Json_repr.any)),
  kind: element_kind,
  format: option(string),
  id: option(string),
}

and element_kind =
  | Object(object_specs)
  | Array(list(element), array_specs)
  | Monomorphic_array(element, array_specs)
  | Combine(combinator, list(element))
  | Def_ref(path)
  | Id_ref(string)
  | Ext_ref(string)
  | String(string_specs)
  | Integer(numeric_specs)
  | Number(numeric_specs)
  | Boolean
  | Null
  | Any
  | Dummy

and combinator =
  | Any_of
  | One_of
  | All_of
  | Not

and array_specs = {
  min_items: int,
  max_items: option(int),
  unique_items: bool,
  additional_items: option(element),
}

and numeric_specs = {
  multiple_of: option(float),
  minimum: option((float, [ | `Inclusive | `Exclusive])),
  maximum: option((float, [ | `Inclusive | `Exclusive])),
}

and object_specs = {
  properties: list((string, element, bool, option(Json_repr.any))),
  pattern_properties: list((string, element)),
  additional_properties: option(element),
  min_properties: int,
  max_properties: option(int),
  schema_dependencies: list((string, element)),
  property_dependencies: list((string, list(string))),
}

and string_specs = {
  pattern: option(string),
  min_length: int,
  max_length: option(int),
};

/* box an element kind without any optional field */
let element = kind => {
  title: None,
  description: None,
  default: None,
  kind,
  format: None,
  enum: None,
  id: None,
};

/*-- equality --------------------------------------------------------------*/

let option_map = f =>
  fun
  | None => None
  | Some(v) => Some(f(v));

let rec eq_element = (a, b) =>
  a.title == b.title
  && a.description == b.description
  && option_map(Json_repr.from_any, a.default)
  == option_map(Json_repr.from_any, b.default)
  && option_map(List.map(Json_repr.from_any), a.enum)
  == option_map(List.map(Json_repr.from_any), b.enum)
  && eq_kind(a.kind, b.kind)
  && a.format == b.format
  && a.id == b.id

and eq_kind = (a, b) =>
  switch (a, b) {
  | (Object(aa), Object(ab)) => eq_object_specs(aa, ab)
  | ([@implicit_arity] Array(esa, sa), [@implicit_arity] Array(esb, sb)) =>
    List.length(esa) == List.length(esb)
    && List.for_all2(eq_element, esa, esb)
    && eq_array_specs(sa, sb)
  | (
      [@implicit_arity] Monomorphic_array(ea, sa),
      [@implicit_arity] Monomorphic_array(eb, sb),
    ) =>
    eq_element(ea, eb) && eq_array_specs(sa, sb)
  | ([@implicit_arity] Combine(ca, esa), [@implicit_arity] Combine(cb, esb)) =>
    ca == cb
    && List.length(esa) == List.length(esb)
    && List.for_all2(eq_element, esa, esb)
  | (Def_ref(pa), Def_ref(pb)) => pa == pb
  | (Id_ref(ra), Id_ref(rb)) => ra == rb
  | (Ext_ref(ra), Ext_ref(rb)) => ra == rb
  | (String(sa), String(sb)) => sa == sb
  | (Integer(na), Integer(nb)) => na == nb
  | (Number(na), Number(nb)) => na == nb
  | (Boolean, Boolean) => true
  | (Null, Null) => true
  | (Any, Any) => true
  | (Dummy, Dummy) => true
  | _ => false
  }

and eq_object_specs = (a, b) =>
  a.min_properties == b.min_properties
  && a.max_properties == b.max_properties
  && List.sort(compare, a.property_dependencies)
  == List.sort(compare, b.property_dependencies)
  && (
    switch (a.additional_properties, b.additional_properties) {
    | (Some(a), Some(b)) => eq_element(a, b)
    | (None, None) => true
    | (_, _) => false
    }
  )
  && List.length(a.pattern_properties) == List.length(b.pattern_properties)
  && List.for_all2(
       ((na, ea), (nb, eb)) => na == nb && eq_element(ea, eb),
       List.sort(((x, _), (y, _)) => compare(x, y), a.pattern_properties),
       List.sort(((x, _), (y, _)) => compare(x, y), b.pattern_properties),
     )
  && List.length(a.schema_dependencies) == List.length(b.schema_dependencies)
  && List.for_all2(
       ((na, ea), (nb, eb)) => na == nb && eq_element(ea, eb),
       List.sort(
         ((x, _), (y, _)) => compare(x, y),
         a.schema_dependencies,
       ),
       List.sort(
         ((x, _), (y, _)) => compare(x, y),
         b.schema_dependencies,
       ),
     )
  && List.length(a.properties) == List.length(b.properties)
  && List.for_all2(
       ((na, ea, ra, da), (nb, eb, rb, db)) =>
         na == nb
         && eq_element(ea, eb)
         && ra == rb
         && option_map(Json_repr.from_any, da)
         == option_map(Json_repr.from_any, db),
       List.sort(
         ((x, _, _, _), (y, _, _, _)) => compare(x, y),
         a.properties,
       ),
       List.sort(
         ((x, _, _, _), (y, _, _, _)) => compare(x, y),
         b.properties,
       ),
     )

and eq_array_specs = (a, b) =>
  a.min_items == b.min_items
  && a.max_items == b.max_items
  && a.unique_items == b.unique_items
  && (
    switch (a.additional_items, b.additional_items) {
    | (Some(a), Some(b)) => eq_element(a, b)
    | (None, None) => true
    | (_, _) => false
    }
  );

/*-- human readable output -------------------------------------------------*/

let pp_string = (ppf, s) =>
  Json_repr.(pp((module Ezjsonm)))(ppf, `String(s));
let pp_num = (ppf, m) =>
  if (abs_float(m) < 1000.) {
    Format.fprintf(ppf, "%g", m);
  } else {
    let (pos, m) =
      if (m < 0.) {
        (false, -. m);
      } else {
        (true, m);
      };
    if (List.fold_left(
          (acc, d) =>
            if (acc) {
              acc;
            } else {
              let v = log(m +. d) /. log(2.);
              if (abs_float(ceil(v) -. v) < 0.00001) {
                Format.fprintf(ppf, "%s2^%g", if (pos) {""} else {"-"}, v);
                if (pos && d < 0. || !pos && d > 0.) {
                  Format.fprintf(ppf, "+%g", abs_float(d));
                };
                if (pos && d > 0. || !pos && d < 0.) {
                  Format.fprintf(ppf, "-%g", abs_float(d));
                };
                true;
              } else {
                false;
              };
            },
          false,
          [(-2.), (-1.), 0., 1., 2.],
        )) {
      ();
    } else {
      Format.fprintf(ppf, "%f", m);
    };
  };
let pp_numeric_specs = (ppf, {multiple_of, minimum, maximum}) =>
  Format.fprintf(
    ppf,
    "%a%a%a",
    ppf =>
      fun
      | None => ()
      | Some(v) => Format.fprintf(ppf, "multiple of %g", v),
    multiple_of,
    ppf =>
      fun
      | (None, _, _)
      | (_, None, None) => ()
      | _ => Format.fprintf(ppf, ", "),
    (multiple_of, minimum, maximum),
    ppf =>
      fun
      | (None, None) => ()
      | (minimum, maximum) =>
        Format.fprintf(
          ppf,
          "∈ %a, %a",
          ppf =>
            fun
            | None => Format.fprintf(ppf, "]∞")
            | Some((m, `Exclusive)) => Format.fprintf(ppf, "]%a", pp_num, m)
            | Some((m, `Inclusive)) => Format.fprintf(ppf, "[%a", pp_num, m),
          minimum,
          ppf =>
            fun
            | None => Format.fprintf(ppf, "∞[")
            | Some((m, `Exclusive)) => Format.fprintf(ppf, "%a[", pp_num, m)
            | Some((m, `Inclusive)) => Format.fprintf(ppf, "%a]", pp_num, m),
          maximum,
        ),
    (minimum, maximum),
  );
let pp_path = ppf =>
  fun
  | [`Field("definitions"), `Field(name)] =>
    Format.fprintf(ppf, "%s", name)
  | path => Json_query.(print_path_as_json_path(~wildcards=true))(ppf, path);
let pp_desc = element =>
  switch (element) {
  | {title: None, description: None} => None
  | {title: Some(text), description: None}
  | {title: None, description: Some(text)} =>
    Some(
      (ppf, ()) =>
        Format.fprintf(
          ppf,
          "/* @[<hov 0>%a@] */",
          Format.pp_print_text,
          text,
        ),
    )
  | {title: Some(title), description: Some(description)} =>
    Some(
      (ppf, ()) =>
        Format.fprintf(
          ppf,
          "/* @[<v 0>@[<hov 0>%a@]@,@[<hov 0>%a@]@] */",
          Format.pp_print_text,
          title,
          Format.pp_print_text,
          description,
        ),
    )
  };
let rec pp_element = (ppf, element) =>
  switch (element.id) {
  | Some(id) => Format.fprintf(ppf, "#%s", id)
  | None =>
    switch (element.format) {
    | Some(format) => Format.fprintf(ppf, "%s", format)
    | None =>
      switch (element.enum) {
      | Some(cases) =>
        let pp_sep = (ppf, ()) => Format.fprintf(ppf, "@ | ");
        Format.fprintf(
          ppf,
          "@[<hv 0>%a@]",
          Format.pp_print_list(
            ~pp_sep,
            Json_repr.pp_any(~compact=false, ()),
          ),
          cases,
        );
      | None =>
        switch (pp_desc(element)) {
        | Some(pp_desc) =>
          let stripped = {...element, title: None, description: None};
          switch (element.kind) {
          | Combine(_) =>
            Format.fprintf(ppf, "%a@,%a", pp_desc, (), pp_element, stripped)
          | Object(specs) =>
            Format.fprintf(
              ppf,
              "@[<v 2>{ %a@,%a }@]",
              pp_desc,
              (),
              pp_object_contents,
              specs,
            )
          | _ =>
            Format.fprintf(ppf, "%a@ %a", pp_element, stripped, pp_desc, ())
          };
        | None =>
          switch (element.kind) {
          | String({pattern: None, min_length: 0, max_length: None}) =>
            Format.fprintf(ppf, "string")
          | String({pattern: Some(pat), min_length: 0, max_length: None}) =>
            Format.fprintf(ppf, "/%s/", pat)
          | String({pattern, min_length, max_length}) =>
            Format.fprintf(
              ppf,
              "%a (%alength%a)",
              ppf =>
                fun
                | None => Format.fprintf(ppf, "string")
                | Some(pat) => Format.fprintf(ppf, "/%s/", pat),
              pattern,
              (ppf, n) =>
                if (n > 0) {
                  Format.fprintf(ppf, "%d <= ", n);
                },
              min_length,
              ppf =>
                fun
                | None => ()
                | Some(m) => Format.fprintf(ppf, "<= %d", m),
              max_length,
            )
          | Integer({multiple_of: None, minimum: None, maximum: None}) =>
            Format.fprintf(ppf, "integer")
          | Integer(specs) =>
            Format.fprintf(ppf, "integer %a", pp_numeric_specs, specs)
          | Number({multiple_of: None, minimum: None, maximum: None}) =>
            Format.fprintf(ppf, "number")
          | Number(specs) =>
            Format.fprintf(ppf, "number %a", pp_numeric_specs, specs)
          | Id_ref(id) => Format.fprintf(ppf, "#%s", id)
          | Def_ref(path) => Format.fprintf(ppf, "$%a", pp_path, path)
          | Ext_ref(uri) => Format.fprintf(ppf, "%s", uri)
          | Boolean => Format.fprintf(ppf, "boolean")
          | Null => Format.fprintf(ppf, "null")
          | Any => Format.fprintf(ppf, "any")
          | Dummy => assert(false)
          | [@implicit_arity] Combine(Not, [elt]) =>
            Format.fprintf(ppf, "! %a", pp_element, elt)
          | [@implicit_arity] Combine(c, elts) =>
            let pp_sep = (ppf, ()) =>
              switch (c) {
              | Any_of => Format.fprintf(ppf, "@ | ")
              | One_of => Format.fprintf(ppf, "@ || ")
              | All_of => Format.fprintf(ppf, "@ && ")
              | _ => assert(false)
              };
            Format.fprintf(
              ppf,
              "@[<hv 0>%a@]",
              Format.pp_print_list(~pp_sep, pp_element),
              elts,
            );
          | Object({
              properties: [],
              pattern_properties: [],
              additional_properties: None,
              min_properties: 0,
              max_properties: Some(0),
              schema_dependencies: [],
              property_dependencies: [],
            }) =>
            Format.fprintf(ppf, "{}")
          | Object(specs) =>
            Format.fprintf(ppf, "@[<v 2>{ %a }@]", pp_object_contents, specs)
          | [@implicit_arity] Array(_, {max_items: Some(0)})
          | [@implicit_arity] Monomorphic_array(_, {max_items: Some(0)}) =>
            Format.fprintf(ppf, "[]")
          | [@implicit_arity] Array(elements, {additional_items}) =>
            let pp_sep = {
              let first = ref(true);
              (
                (ppf, ()) =>
                  if (first^) {
                    first := false;
                  } else {
                    Format.fprintf(ppf, ",@ ");
                  }
              );
            };
            Format.fprintf(ppf, "@[<hv 2>[ ");
            List.iter(
              elt => Format.fprintf(ppf, "%a%a", pp_sep, (), pp_element, elt),
              elements,
            );
            switch (additional_items) {
            | None => ()
            | Some({kind: Any}) =>
              Format.fprintf(ppf, "%a,@ ...", pp_sep, ())
            | Some(elt) =>
              Format.fprintf(ppf, "%a,@ %a ...", pp_sep, (), pp_element, elt)
            };
            Format.fprintf(ppf, " ]@]");
          | [@implicit_arity]
            Monomorphic_array(elt, {additional_items: None}) =>
            Format.fprintf(ppf, "[ %a ... ]", pp_element, elt)
          | [@implicit_arity]
            Monomorphic_array(elt, {additional_items: Some({kind: Any})}) =>
            Format.fprintf(ppf, "@[<hv 2>[ %a ...,@ ... ]@]", pp_element, elt)
          | [@implicit_arity]
            Monomorphic_array(elt, {additional_items: Some(add_elt)}) =>
            /* TODO: find a good way to print length */
            Format.fprintf(
              ppf,
              "@[<hv 2>[ %a ...,@ %a ... ]@]",
              pp_element,
              elt,
              pp_element,
              add_elt,
            )
          }
        }
      }
    }
  }
and pp_object_contents =
    (ppf, {properties, pattern_properties, additional_properties}) => {
  /* TODO: find a good way to print length / dependencies */
  let pp_sep = {
    let first = ref(true);
    (ppf, ()) =>
      if (first^) {
        first := false;
      } else {
        Format.fprintf(ppf, ",@ ");
      };
  };
  List.iter(
    ((name, elt, req, _)) =>
      Format.fprintf(
        ppf,
        "%a@[<hv 2>%a%s:@ %a@]",
        pp_sep,
        (),
        pp_string,
        name,
        if (req) {""} else {"?"},
        pp_element,
        elt,
      ),
    properties,
  );
  List.iter(
    ((name, elt)) =>
      Format.fprintf(
        ppf,
        "%a@[<hv 2>/%s/:@ %a@]",
        pp_sep,
        (),
        name,
        pp_element,
        elt,
      ),
    pattern_properties,
  );
  switch (additional_properties) {
  | None => ()
  | Some({kind: Any}) => Format.fprintf(ppf, "%a...", pp_sep, ())
  | Some(elt) =>
    Format.fprintf(ppf, "%a@[<hv 2>*:@ %a@]", pp_sep, (), pp_element, elt)
  };
};
let pp = (ppf, schema) => {
  Format.fprintf(ppf, "@[<v 0>");
  pp_element(ppf, schema.root);
  List.iter(
    ((path, elt)) =>
      switch (pp_desc(elt)) {
      | None =>
        Format.fprintf(
          ppf,
          "@,@[<hv 2>$%a:@ %a@]",
          pp_path,
          path,
          pp_element,
          elt,
        )
      | Some(pp_desc) =>
        let stripped = {...elt, title: None, description: None};
        Format.fprintf(
          ppf,
          "@,@[<v 2>$%a:@,%a@,%a@]",
          pp_path,
          path,
          pp_desc,
          (),
          pp_element,
          stripped,
        );
      },
    schema.definitions,
  );
  List.iter(
    ((id, elt)) =>
      switch (pp_desc(elt)) {
      | None =>
        Format.fprintf(
          ppf,
          "@,@[<hv 2>#%s:@ %a@]",
          id,
          pp_element,
          {...elt, id: None},
        )
      | Some(pp_desc) =>
        let stripped = {...elt, title: None, description: None, id: None};
        Format.fprintf(
          ppf,
          "@,@[<v 2>#%s:@,%a@,%a@]",
          id,
          pp_desc,
          (),
          pp_element,
          stripped,
        );
      },
    schema.ids,
  );
  Format.fprintf(ppf, "@]");
};

/*-- errors ----------------------------------------------------------------*/

exception Cannot_parse(path, exn);
exception Dangling_reference(string);
exception Bad_reference(string);
exception Unexpected(string, string);
exception Duplicate_definition(path, element, element);

let rec print_error = (~print_unknown=?, ppf) =>
  fun
  | [@implicit_arity] Cannot_parse(path, exn) =>
    Format.fprintf(
      ppf,
      "@[<v 2>Schema parse error:@,At %a@,%a@]",
      Json_query.print_path_as_json_path(~wildcards=true),
      path,
      print_error(~print_unknown?),
      exn,
    )
  | Dangling_reference(uri) =>
    Format.fprintf(ppf, "Dangling reference %s", uri)
  | Bad_reference(str) =>
    Format.fprintf(ppf, "Illegal reference notation %s", str)
  | [@implicit_arity] Unexpected(unex, ex) =>
    Format.fprintf(ppf, "Unexpected %s instead of %s", unex, ex)
  | [@implicit_arity] Duplicate_definition(name, elt, defelt) =>
    Format.fprintf(
      ppf,
      "@[<v 2>Duplicate definition %a@,To be inserted:@,  @[<v 0>%a@]@,Already present:@,  @[<v 0>%a@]@]",
      Json_query.print_path_as_json_pointer(~wildcards=false),
      name,
      pp_element,
      elt,
      pp_element,
      defelt,
    )
  | exn => Json_query.print_error(~print_unknown?, ppf, exn);

/*-- internal definition table handling ------------------------------------*/

let find_definition = (name, defs) => List.assoc(name, defs);

let definition_exists = (name, defs) => List.mem_assoc(name, defs);

let insert_definition = (name, elt, defs) => {
  let rec insert =
    fun
    | [] => [(name, elt)]
    | [(defname, _) as def, ...rem] when defname != name => [
        def,
        ...insert(rem),
      ]
    | [(_, {kind: Dummy}), ...rem] => [(name, elt), ...rem]
    | [(_, defelt), ...rem] => {
        if (!eq_element(elt, defelt)) {
          raise([@implicit_arity] Duplicate_definition(name, elt, defelt));
        };
        [(name, elt), ...rem];
      };
  insert(defs);
};

module Make = (Repr: Json_repr.Repr) => {
  module Query = Json_query.Make(Repr);
  open Query;

  /*-- printer ---------------------------------------------------------------*/

  let to_json = schema => {
    /* functional JSON building combinators */
    let obj = l => Repr.repr(`O(l));
    let set_always = (f, v) => [(f, Repr.repr(v))];
    let set_if_some = (f, v, cb) =>
      switch (v) {
      | None => []
      | Some(v) => [(f, Repr.repr(cb(v)))]
      };
    let set_if_cons = (f, v, cb) =>
      switch (v) {
      | [] => []
      | v => [(f, Repr.repr(cb(v)))]
      };
    let set_if_neq = (f, v, v', cb) =>
      if (v != v') {
        [(f, Repr.repr(cb(v)))];
      } else {
        [];
      };
    /* recursive encoder */
    let rec format_element =
            ({title, description, default, enum, kind, format}) =>
      set_if_some("title", title, s => `String(s))
      @ set_if_some("description", description, s => `String(s))
      @ (
        switch (kind) {
        | Object(specs) =>
          let required =
            List.fold_left(
              (r, (n, _, p, _)) =>
                if (p) {
                  [Repr.repr(`String(n)), ...r];
                } else {
                  r;
                },
              [],
              specs.properties,
            );
          let properties =
            List.map(
              ((n, elt, _, _)) => (n, obj(format_element(elt))),
              specs.properties,
            );
          set_always("type", `String("object"))
          @ set_always("properties", `O(properties))
          @ set_if_cons("required", required, l => `A(l))
          @ set_if_cons("patternProperties", specs.pattern_properties, fs =>
              `O(
                List.map(
                  ((n, elt)) => (n, obj(format_element(elt))),
                  fs,
                ),
              )
            )
          @ set_if_neq(
              "additionalProperties",
              specs.additional_properties,
              Some(element(Any)),
              fun
              | None => `Bool(false)
              | Some(elt) => `O(format_element(elt)),
            )
          @ set_if_neq("minProperties", specs.min_properties, 0, i =>
              `Float(float(i))
            )
          @ set_if_some("maxProperties", specs.max_properties, i =>
              `Float(float(i))
            )
          @ set_if_cons("schemaDependencies", specs.schema_dependencies, fs =>
              `O(
                List.map(
                  ((n, elt)) => (n, obj(format_element(elt))),
                  fs,
                ),
              )
            )
          @ set_if_cons(
              "propertyDependencies",
              specs.property_dependencies,
              fs => {
                let property_dependencies = {
                  let strings = ls =>
                    List.map(s => Repr.repr(`String(s)), ls);
                  List.map(
                    ((n, ls)) => (n, Repr.repr(`A(strings(ls)))),
                    fs,
                  );
                };
                `O(property_dependencies);
              },
            );
        | [@implicit_arity] Array(elts, specs) =>
          set_always("type", `String("array"))
          @ set_always(
              "items",
              `A(List.map(elt => obj(format_element(elt)), elts)),
            )
          @ set_if_neq("minItems", specs.min_items, 0, i =>
              `Float(float(i))
            )
          @ set_if_some("maxItems", specs.max_items, i => `Float(float(i)))
          @ set_if_neq("uniqueItems", specs.unique_items, false, b =>
              `Bool(b)
            )
          @ set_if_neq(
              "additionalItems",
              specs.additional_items,
              Some(element(Any)),
              fun
              | None => `Bool(false)
              | Some(elt) => `O(format_element(elt)),
            )
        | [@implicit_arity]
          Monomorphic_array(elt, {min_items, max_items, unique_items}) =>
          set_always("type", `String("array"))
          @ set_always("items", `O(format_element(elt)))
          @ set_if_neq("minItems", min_items, 0, i => `Float(float(i)))
          @ set_if_some("maxItems", max_items, i => `Float(float(i)))
          @ set_if_neq("uniqueItems", unique_items, false, b => `Bool(b))
        | [@implicit_arity] Combine(c, elts) =>
          let combinator = (
            fun
            | Any_of => "anyOf"
            | One_of => "oneOf"
            | All_of => "allOf"
            | Not => "not"
          );
          set_always(
            combinator(c),
            `A(List.map(elt => obj(format_element(elt)), elts)),
          );
        | Def_ref(path) =>
          set_always("$ref", `String("#" ++ json_pointer_of_path(path)))
        | Id_ref(name) => set_always("$ref", `String("#" ++ name))
        | Ext_ref(uri) => set_always("$ref", `String(uri))
        | Integer(specs) =>
          set_always("type", `String("integer"))
          @ set_if_some("multipleOf", specs.multiple_of, v => `Float(v))
          @ (
            switch (specs.minimum) {
            | None => []
            | Some((v, `Inclusive)) => [("minimum", Repr.repr(`Float(v)))]
            | Some((v, `Exclusive)) => [
                ("minimum", Repr.repr(`Float(v))),
                ("exclusiveMinimum", Repr.repr(`Bool(true))),
              ]
            }
          )
          @ (
            switch (specs.maximum) {
            | None => []
            | Some((v, `Inclusive)) => [("maximum", Repr.repr(`Float(v)))]
            | Some((v, `Exclusive)) => [
                ("maximum", Repr.repr(`Float(v))),
                ("exclusiveMaximum", Repr.repr(`Bool(true))),
              ]
            }
          )
        | Number(specs) =>
          set_always("type", `String("number"))
          @ set_if_some("multipleOf", specs.multiple_of, v => `Float(v))
          @ (
            switch (specs.minimum) {
            | None => []
            | Some((v, `Inclusive)) => [("minimum", Repr.repr(`Float(v)))]
            | Some((v, `Exclusive)) => [
                ("minimum", Repr.repr(`Float(v))),
                ("exclusiveMinimum", Repr.repr(`Bool(true))),
              ]
            }
          )
          @ (
            switch (specs.maximum) {
            | None => []
            | Some((v, `Inclusive)) => [("maximum", Repr.repr(`Float(v)))]
            | Some((v, `Exclusive)) => [
                ("maximum", Repr.repr(`Float(v))),
                ("exclusiveMaximum", Repr.repr(`Bool(true))),
              ]
            }
          )
        | String({pattern, min_length, max_length}) =>
          set_always("type", `String("string"))
          @ set_if_neq("minLength", min_length, 0, i => `Float(float(i)))
          @ set_if_some("maxLength", max_length, i => `Float(float(i)))
          @ set_if_some("pattern", pattern, s => `String(s))
        | Boolean => set_always("type", `String("boolean"))
        | Null => set_always("type", `String("null"))
        | Dummy => invalid_arg("Json_schema.to_json: remaining dummy element")
        | Any => []
        }
      )
      @ set_if_some("default", default, j =>
          Repr.view(Json_repr.any_to_repr((module Repr), j))
        )
      @ set_if_some("enum", enum, js =>
          `A(List.map(Json_repr.any_to_repr((module Repr)), js))
        )
      @ set_if_some("format", format, s => `String(s));
    List.fold_left(
      (acc, (n, elt)) => insert(n, obj(format_element(elt)), acc),
      obj(
        set_always("$schema", `String(version))
        @ format_element(schema.root),
      ),
      schema.definitions,
    );
  };

  let unexpected = (kind, expected) => {
    let kind =
      switch (kind) {
      | `O([]) => "empty object"
      | `A([]) => "empty array"
      | `O(_) => "object"
      | `A(_) => "array"
      | `Null => "null"
      | `String("") => "empty string"
      | `String(_) => "string"
      | `Float(_) => "number"
      | `Bool(_) => "boolean"
      };
    [@implicit_arity]
    Cannot_parse([], [@implicit_arity] Unexpected(kind, expected));
  };

  /*-- parser ----------------------------------------------------------------*/

  let at_path = p =>
    fun
    | [@implicit_arity] Cannot_parse(l, err) =>
      [@implicit_arity] Cannot_parse(p @ l, err)
    | exn => exn;
  let at_field = n => at_path([`Field(n)]);
  let at_index = i => at_path([`Index(i)]);

  let of_json = json => {
    /* parser combinators */
    let opt_field = (obj, n) =>
      switch (Repr.view(obj)) {
      | `O(ls) =>
        try(Some(List.assoc(n, ls))) {
        | Not_found => None
        }
      | _ => None
      };
    let opt_field_view = (obj, n) =>
      switch (Repr.view(obj)) {
      | `O(ls) =>
        try(Some(Repr.view(List.assoc(n, ls)))) {
        | Not_found => None
        }
      | _ => None
      };
    let opt_string_field = (obj, n) =>
      switch (opt_field_view(obj, n)) {
      | Some(`String(s)) => Some(s)
      | Some(k) => raise(at_field(n) @@ unexpected(k, "string"))
      | None => None
      };
    let opt_bool_field = (def, obj, n) =>
      switch (opt_field_view(obj, n)) {
      | Some(`Bool(b)) => b
      | Some(k) => raise(at_field(n) @@ unexpected(k, "bool"))
      | None => def
      };
    let opt_int_field = (obj, n) =>
      switch (opt_field_view(obj, n)) {
      | Some(`Float(f))
          when fst(modf(f)) == 0. && f <= 2. ** 53. && f >= (-2.) ** 53. =>
        Some(f)
      | Some(k) => raise(at_field(n) @@ unexpected(k, "integer"))
      | None => None
      };
    let opt_length_field = (obj, n) =>
      switch (opt_field_view(obj, n)) {
      | Some(`Float(f)) when fst(modf(f)) == 0. && f <= 2. ** 30. && f >= 0. =>
        Some(int_of_float(f))
      | Some(k) => raise(at_field(n) @@ unexpected(k, "length"))
      | None => None
      };
    let opt_float_field = (obj, n) =>
      switch (opt_field_view(obj, n)) {
      | Some(`Float(f)) => Some(f)
      | Some(k) => raise(at_field(n) @@ unexpected(k, "number"))
      | None => None
      };
    let opt_array_field = (obj, n) =>
      switch (opt_field_view(obj, n)) {
      | Some(`A(s)) => Some(s)
      | Some(k) => raise(at_field(n) @@ unexpected(k, "array"))
      | None => None
      };
    let opt_uri_field = (obj, n) =>
      switch (opt_string_field(obj, n)) {
      | None => None
      | Some(uri) => Some(uri)
      };
    /* local resolution of definitions */
    let schema_source =
      switch (opt_uri_field(json, "id")) {
      | Some(uri) => "Uri.with_fragment uri None"
      | None => "Uri.empty"
      };
    let collected_definitions = ref([]);
    let collected_id_defs = ref([]);
    let collected_id_refs = ref([]);
    let rec collect_definition: string => element_kind = (
      uri =>
        switch (None, Some("lol")) {
        | (
            Some(_) /* Actually means: any of host, user or port is defined. */,
            _,
          ) =>
          Ext_ref(uri)
        | (None, None) =>
          raise(
            [@implicit_arity]
            Cannot_parse([], Bad_reference("uri has no fragment")),
          )
        | (None, Some(fragment)) when !String.contains(fragment, '/') =>
          collected_id_refs := [fragment, ...collected_id_refs^];
          Id_ref(fragment);
        | (None, Some(fragment)) =>
          let path =
            try(path_of_json_pointer(~wildcards=false, fragment)) {
            | err => raise([@implicit_arity] Cannot_parse([], err))
            };
          try({
            let raw = query(path, json);
            if (!definition_exists(path, collected_definitions^)) {
              collected_definitions :=
                insert_definition(
                  path,
                  element(Dummy),
                  collected_definitions^,
                );
              let elt =
                try(parse_element(schema_source, raw)) {
                | err => raise(at_path(path, err))
                };
              collected_definitions :=
                insert_definition(path, elt, collected_definitions^);
            };
            Def_ref(path);
          }) {
          | Not_found =>
            raise(
              [@implicit_arity] Cannot_parse([], Dangling_reference(uri)),
            )
          };
        }:
        string => element_kind
      /* dummy insertion so we don't recurse and we support cycles */
      /* actual insertion */
    )
    /* recursive parser */
    and parse_element: (string, Repr.value) => element = (
      (source, json) => {
        let id = opt_uri_field(json, "id");
        let (id, source) = (None, source);
        if (source != schema_source) {
          element(Ext_ref(""));
        } else {
          let id =
            switch (id) {
            | None => None
            | Some(id) when String.contains(id, '/') =>
              raise(
                at_field("id") @@
                [@implicit_arity]
                Cannot_parse([], Bad_reference(id ++ " is not a valid ID")),
              )
            | Some(id) => Some(id)
            };
          let as_kind =
            switch (opt_field_view(json, "type")) {
            | Some(`String(name)) =>
              Some(element(parse_element_kind(source, json, name)))
            | Some(`A([]) as k) =>
              raise(
                at_field("type") @@
                unexpected(k, "type, type array or operator"),
              )
            | Some(`A(l)) =>
              let rec items = (i, acc) => (
                fun
                | [] => {
                    let kind =
                      [@implicit_arity] Combine(Any_of, List.rev(acc));
                    Some(element(kind));
                  }
                | [`String(name), ...tl] => {
                    let kind = parse_element_kind(source, json, name);
                    let case = element(kind);
                    items(succ(i), [case, ...acc], tl);
                  }
                | [k, ..._] =>
                  raise(
                    at_field("type") @@ at_index(i) @@ unexpected(k, "type"),
                  )
              );
              items(0, [], List.map(Repr.view, l));
            | Some(k) =>
              raise(
                at_field("type") @@
                unexpected(k, "type, type array or operator"),
              )
            | None => None
            };
          let as_ref =
            switch (opt_uri_field(json, "$ref")) {
            | Some(uri) =>
              let path = collect_definition(uri);
              Some(element(path));
            | None => None
            };
          let as_nary = (name, combinator, others) => {
            let build =
              fun
              | [] => None
              | [case] => Some(case)
              | cases => {
                  let kind = [@implicit_arity] Combine(combinator, cases);
                  Some(element(kind));
                };
            switch (opt_field_view(json, name)) {
            | Some(`A([_, ..._] as cases)) /* list of schemas */ =>
              let rec items = (i, acc) => (
                fun
                | [elt, ...tl] => {
                    let elt =
                      try(parse_element(source, elt)) {
                      | err => raise(at_field(name) @@ at_index(i) @@ err)
                      };
                    items(succ(i), [elt, ...acc], tl);
                  }
                | [] => build(others @ List.rev(acc))
              );
              items(0, [], cases);
            | None => build(others)
            | Some(k) =>
              raise(at_field(name) @@ unexpected(k, "a list of elements"))
            };
          };
          let as_not =
            switch (opt_field_view(json, "not")) {
            | None => None
            | Some(elt) =>
              let elt =
                try(parse_element(source, Repr.repr(elt))) {
                | err => raise(at_field("not", err))
                };
              let kind = [@implicit_arity] Combine(Not, [elt]);
              Some(element(kind));
            };
          let title = opt_string_field(json, "title");
          let description = opt_string_field(json, "description");
          let default =
            switch (opt_field(json, "default")) {
            | Some(v) => Some(Json_repr.repr_to_any((module Repr), v))
            | None => None
            };
          let enum =
            switch (opt_array_field(json, "enum")) {
            | Some(v) =>
              Some(List.map(Json_repr.repr_to_any((module Repr)), v))
            | None => None
            };
          let format = opt_string_field(json, "format");
          let as_one_of = as_nary("oneOf", One_of, []);
          let as_any_of = as_nary("anyOf", Any_of, []);
          let all = [as_kind, as_ref, as_not, as_one_of, as_any_of];
          let cases =
            List.flatten(
              List.map(
                fun
                | None => []
                | Some(e) => [e],
                all,
              ),
            );
          let kind =
            switch (as_nary("allOf", All_of, cases)) {
            | None => Any
            | Some({kind}) => kind
            };
          {title, description, default, format, kind, enum, id};
        };
      }:
        (string, Repr.value) => element /* not found and no auxiliary case */ /* one case -> simplify */ /* several cases build the combination node with empty options */ /* TODO: check format ? */ /* no type, ref or logical combination found */
      /* We don't support inlined schemas, so we just drop elements with
         external sources and replace them with external references. */
      /* We parse the various element syntaxes and combine them afterwards. */
      /* 1. An element with a known type field and associated fields. */
      /* 2. A reference */
      /* 3. Combined schemas */
      /* 4. Negated schema */
      /* parse optional fields */
      /* combine all specifications under a big conjunction */
      /* add optional fields */
    )
    and parse_element_kind = (source, json, name) => {
      let integer_specs = json => {
        let multiple_of = opt_int_field(json, "multipleOf");
        let minimum =
          if (opt_bool_field(false, json, "exclusiveMinimum")) {
            switch (opt_int_field(json, "minimum")) {
            | None =>
              let err = "minimum field required when exclusiveMinimum is true";
              raise(Failure(err));
            | Some(v) => Some((v, `Inclusive))
            };
          } else {
            switch (opt_int_field(json, "minimum")) {
            | None => None
            | Some(v) => Some((v, `Exclusive))
            };
          };
        let maximum =
          if (opt_bool_field(false, json, "exclusiveMaximum")) {
            switch (opt_int_field(json, "maximum")) {
            | None =>
              let err = "maximum field required when exclusiveMaximum is true";
              raise(Failure(err));
            | Some(v) => Some((v, `Inclusive))
            };
          } else {
            switch (opt_int_field(json, "maximum")) {
            | None => None
            | Some(v) => Some((v, `Exclusive))
            };
          };
        {multiple_of, minimum, maximum};
      };
      let numeric_specs = json => {
        let multiple_of = opt_float_field(json, "multipleOf");
        let minimum =
          if (opt_bool_field(false, json, "exclusiveMinimum")) {
            switch (opt_float_field(json, "minimum")) {
            | None =>
              let err = "minimum field required when exclusiveMinimum is true";
              raise(Failure(err));
            | Some(v) => Some((v, `Inclusive))
            };
          } else {
            switch (opt_float_field(json, "minimum")) {
            | None => None
            | Some(v) => Some((v, `Exclusive))
            };
          };
        let maximum =
          if (opt_bool_field(false, json, "exclusiveMaximum")) {
            switch (opt_float_field(json, "maximum")) {
            | None =>
              let err = "maximum field required when exclusiveMaximum is true";
              raise(Failure(err));
            | Some(v) => Some((v, `Inclusive))
            };
          } else {
            switch (opt_float_field(json, "maximum")) {
            | None => None
            | Some(v) => Some((v, `Exclusive))
            };
          };
        {multiple_of, minimum, maximum};
      };
      switch (name) {
      | "integer" => Integer(integer_specs(json))
      | "number" => Number(numeric_specs(json))
      | "boolean" => Boolean
      | "null" => Null
      | "string" =>
        let specs = {
          let pattern = opt_string_field(json, "pattern");
          let min_length = opt_length_field(json, "minLength");
          let max_length = opt_length_field(json, "maxLength");
          let min_length =
            switch (min_length) {
            | None => 0
            | Some(l) => l
            };
          {pattern, min_length, max_length};
        };
        String(specs);
      | "array" =>
        let specs = {
          let unique_items = opt_bool_field(false, json, "uniqueItems");
          let min_items = opt_length_field(json, "minItems");
          let max_items = opt_length_field(json, "maxItems");
          let min_items =
            switch (min_items) {
            | None => 0
            | Some(l) => l
            };
          switch (opt_field_view(json, "additionalItems")) {
          | Some(`Bool(true)) => {
              min_items,
              max_items,
              unique_items,
              additional_items: Some(element(Any)),
            }
          | None
          | Some(`Bool(false)) => {
              min_items,
              max_items,
              unique_items,
              additional_items: None,
            }
          | Some(elt) =>
            let elt =
              try(parse_element(source, Repr.repr(elt))) {
              | err => raise(at_field("additionalItems", err))
              };
            {
              min_items,
              max_items,
              unique_items,
              additional_items: Some(elt),
            };
          };
        };
        switch (opt_field_view(json, "items")) {
        | Some(`A(elts)) =>
          let rec elements = (i, acc) => (
            fun
            | [] => [@implicit_arity] Array(List.rev(acc), specs)
            | [elt, ...tl] => {
                let elt =
                  try(parse_element(source, elt)) {
                  | err => raise(at_field("items") @@ at_index(i, err))
                  };
                elements(succ(i), [elt, ...acc], tl);
              }
          );
          elements(0, [], elts);
        | Some(elt) =>
          let elt =
            try(parse_element(source, Repr.repr(elt))) {
            | err => raise(at_field("items", err))
            };
          [@implicit_arity] Monomorphic_array(elt, specs);
        | None => [@implicit_arity] Monomorphic_array(element(Any), specs)
        };
      | "object" =>
        let required =
          switch (opt_array_field(json, "required")) {
          | None => []
          | Some(l) =>
            let rec items = (i, acc) => (
              fun
              | [`String(s), ...tl] => items(succ(i), [s, ...acc], tl)
              | [] => List.rev(acc)
              | [k, ..._] =>
                raise(
                  at_field("required") @@
                  at_index(i) @@
                  unexpected(k, "string"),
                )
            );
            items(0, [], List.map(Repr.view, l));
          };
        let properties =
          switch (opt_field_view(json, "properties")) {
          | Some(`O(props)) =>
            let rec items = acc => (
              fun
              | [] => List.rev(acc)
              | [(n, elt), ...tl] => {
                  let elt =
                    try(parse_element(source, elt)) {
                    | err =>
                      raise(at_field("properties") @@ at_field(n) @@ err)
                    };
                  let req = List.mem(n, required);
                  items([(n, elt, req, None), ...acc], tl);
                }
            ); /* XXX: fixme */
            items([], props);
          | None => []
          | Some(k) =>
            raise(at_field("properties") @@ unexpected(k, "object"))
          };
        let additional_properties =
          switch (opt_field_view(json, "additionalProperties")) {
          | Some(`Bool(false)) => None
          | None
          | Some(`Bool(true)) => Some(element(Any))
          | Some(elt) =>
            let elt =
              try(parse_element(source, Repr.repr(elt))) {
              | err => raise(at_field("additionalProperties", err))
              };
            Some(elt);
          };
        let property_dependencies =
          switch (opt_field_view(json, "propertyDependencies")) {
          | None => []
          | Some(`O(l)) =>
            let rec sets = sacc => (
              fun
              | [(n, `A(l)), ...tl] => {
                  let rec strings = (j, acc) => (
                    fun
                    | [] => sets([(n, List.rev(acc)), ...sacc], tl)
                    | [`String(s), ...tl] =>
                      strings(succ(j), [s, ...acc], tl)
                    | [k, ..._] =>
                      raise(
                        at_field("propertyDependencies") @@
                        at_field(n) @@
                        at_index(j) @@
                        unexpected(k, "string"),
                      )
                  );
                  strings(0, [], List.map(Repr.view, l));
                }
              | [(n, k), ..._] =>
                raise(
                  at_field("propertyDependencies") @@
                  at_field(n) @@
                  unexpected(k, "string array"),
                )
              | [] => List.rev(sacc)
            );
            sets([], List.map(((n, v)) => (n, Repr.view(v)), l));
          | Some(k) =>
            raise(
              at_field("propertyDependencies") @@ unexpected(k, "object"),
            )
          };
        let parse_element_assoc = field =>
          switch (opt_field_view(json, field)) {
          | None => []
          | Some(`O(props)) =>
            let rec items = acc => (
              fun
              | [] => List.rev(acc)
              | [(n, elt), ...tl] => {
                  let elt =
                    try(parse_element(source, elt)) {
                    | err => raise(at_field(field) @@ at_field(n, err))
                    };
                  items([(n, elt), ...acc], tl);
                }
            );
            items([], props);
          | Some(k) => raise(at_field(field) @@ unexpected(k, "object"))
          };
        let pattern_properties = parse_element_assoc("patternProperties");
        let schema_dependencies = parse_element_assoc("schemaDependencies");
        let min_properties =
          switch (opt_length_field(json, "minProperties")) {
          | None => 0
          | Some(l) => l
          };
        let max_properties = opt_length_field(json, "maxProperties");
        Object({
          properties,
          pattern_properties,
          additional_properties,
          min_properties,
          max_properties,
          schema_dependencies,
          property_dependencies,
        });
      | n =>
        raise(
          [@implicit_arity]
          Cannot_parse([], [@implicit_arity] Unexpected(n, "a known type")),
        )
      };
    };
    /* parse recursively from the root */
    let root = parse_element("Uri.empty", json);
    /* force the addition of everything inside /definitions */
    switch (Repr.view(query([`Field("definitions")], json))) {
    | `O(all) =>
      let all = List.map(((n, _)) => "#/definitions/" ++ n, all);
      List.iter(uri => collect_definition(uri) |> ignore, all);
    | _ => ()
    | exception Not_found => ()
    };
    /* check the domain of IDs */
    List.iter(
      id =>
        if (!List.mem_assoc(id, collected_id_defs^)) {
          raise(
            [@implicit_arity]
            Cannot_parse(
              [],
              Dangling_reference("(Uri.(with_fragment empty (Some id)))"),
            ),
          );
        },
      collected_id_refs^,
    );
    let ids = collected_id_defs^;
    let source = schema_source;
    let world = [];
    let definitions = collected_definitions^;
    {root, definitions, source, ids, world};
  };

  /*-- creation and update ---------------------------------------------------*/

  /* Checks that all local refs and ids are defined */
  let check_definitions = (root, definitions) => {
    let collected_id_defs = ref([]);
    let collected_id_refs = ref([]);
    let rec check = ({kind, id} as elt) => {
      switch (id) {
      | None => ()
      | Some(id) => collected_id_defs := [(id, elt), ...collected_id_defs^]
      };
      switch (kind) {
      | Object({
          properties,
          pattern_properties,
          additional_properties,
          schema_dependencies,
        }) =>
        List.iter(((_, e, _, _)) => check(e), properties);
        List.iter(((_, e)) => check(e), pattern_properties);
        List.iter(((_, e)) => check(e), schema_dependencies);
        switch (additional_properties) {
        | Some(e) => check(e)
        | None => ()
        };
      | [@implicit_arity] Array(es, {additional_items}) =>
        List.iter(check, es);
        switch (additional_items) {
        | Some(e) => check(e)
        | None => ()
        };
      | [@implicit_arity] Monomorphic_array(e, {additional_items}) =>
        check(e);
        switch (additional_items) {
        | Some(e) => check(e)
        | None => ()
        };
      | [@implicit_arity] Combine(_, es) => List.iter(check, es)
      | Def_ref(path) =>
        if (!definition_exists(path, definitions)) {
          let path = json_pointer_of_path(path);
          raise(
            Dangling_reference("(Uri.(with_fragment empty) (Some path))"),
          );
        }
      | Id_ref(id) => collected_id_refs := [id, ...collected_id_refs^]
      | Ext_ref(_)
      | String(_)
      | Integer(_)
      | Number(_)
      | Boolean
      | Null
      | Any
      | Dummy => ()
      };
    };
    /* check the root and definitions */
    check(root);
    List.iter(((_, root)) => check(root), definitions);
    /* check the domain of IDs */
    List.iter(
      id =>
        if (!List.mem_assoc(id, collected_id_defs^)) {
          raise(Dangling_reference("(Uri.(with_fragment empty (Some id)))"));
        },
      collected_id_refs^,
    );
    collected_id_defs^;
  };

  let create = root => {
    let ids = check_definitions(root, []);
    {root, definitions: [], world: [], ids, source: "Uri.empty"};
  };

  let root = ({root}) => root;

  let update = (root, sch) => {
    let ids = check_definitions(root, sch.definitions);
    {...sch, root, ids};
  };

  let any = create(element(Any));

  let self = {
    root: element(Ext_ref(version)),
    definitions: [],
    ids: [],
    world: [],
    source: "Uri.empty",
  };

  /* remove unused definitions from the schema */
  let simplify = schema => {
    let res = ref([]) /* collected definitions */;
    let rec collect = ({kind}) =>
      switch (kind) {
      | Object({
          properties,
          pattern_properties,
          additional_properties,
          schema_dependencies,
        }) =>
        List.iter(((_, e, _, _)) => collect(e), properties);
        List.iter(((_, e)) => collect(e), pattern_properties);
        List.iter(((_, e)) => collect(e), schema_dependencies);
        switch (additional_properties) {
        | Some(e) => collect(e)
        | None => ()
        };
      | [@implicit_arity] Array(es, {additional_items}) =>
        List.iter(collect, es);
        switch (additional_items) {
        | Some(e) => collect(e)
        | None => ()
        };
      | [@implicit_arity] Monomorphic_array(e, {additional_items}) =>
        collect(e);
        switch (additional_items) {
        | Some(e) => collect(e)
        | None => ()
        };
      | [@implicit_arity] Combine(_, es) => List.iter(collect, es)
      | Def_ref(path) =>
        let def = find_definition(path, schema.definitions);
        res := insert_definition(path, def, res^);
      | Ext_ref(_)
      | Id_ref(_)
      | String(_)
      | Integer(_)
      | Number(_)
      | Boolean
      | Null
      | Any
      | Dummy => ()
      };

    collect(schema.root);
    {...schema, definitions: res^};
  };

  let definition_path_of_name = (~definitions_path="/definitions/", name) =>
    path_of_json_pointer(~wildcards=false) @@
    (
      switch (name.[0]) {
      | exception _ => raise(Bad_reference(name))
      | '/' => name
      | _ => definitions_path ++ name
      }
    );

  let find_definition = (~definitions_path=?, name, schema) => {
    let path = definition_path_of_name(~definitions_path?, name);
    find_definition(path, schema.definitions);
  };

  let definition_ref = (~definitions_path=?, name) => {
    let path = definition_path_of_name(~definitions_path?, name);
    element(Def_ref(path));
  };

  let definition_exists = (~definitions_path=?, name, schema) => {
    let path = definition_path_of_name(~definitions_path?, name);
    definition_exists(path, schema.definitions);
  };

  let add_definition = (~definitions_path=?, name, elt, schema) => {
    let path = definition_path_of_name(~definitions_path?, name);
    /* check inside def */
    let definitions = insert_definition(path, elt, schema.definitions);
    ({...schema, definitions}, element(Def_ref(path)));
  };

  let merge_definitions = ((sa, sb)) => {
    let rec sorted_merge =
      fun
      | [(na, da) as a, (nb, db) as b, ...tl] =>
        if (na == nb) {
          if (da.kind == Dummy || db.kind == Dummy || eq_element(da, db)) {
            [(na, da), ...sorted_merge(tl)];
          } else {
            raise([@implicit_arity] Duplicate_definition(na, da, db));
          };
        } else {
          [a, ...sorted_merge([b, ...tl])];
        }
      | ([] | [_]) as rem => rem;

    let definitions =
      sorted_merge(List.sort(compare, sa.definitions @ sb.definitions));
    ({...sa, definitions}, {...sb, definitions});
  };

  let combine = (op, schemas) => {
    let rec combine = (sacc, eacc) =>
      fun
      | [] => update(element([@implicit_arity] Combine(op, eacc)), sacc)
      | [s, ...ss] => {
          let (sacc, s) = merge_definitions((sacc, s));
          combine(sacc, [s.root, ...eacc], ss);
        };
    combine(any, [], schemas);
  };

  let is_nullable = ({ids, definitions, root}) => {
    let rec nullable = ({kind}) =>
      switch (kind) {
      | Null
      | Any => true
      | Object(_)
      | Array(_)
      | Monomorphic_array(_)
      | Ext_ref(_)
      | String(_)
      | Integer(_)
      | Number(_)
      | Boolean => false
      | [@implicit_arity] Combine(Not, [elt]) => !nullable(elt)
      | [@implicit_arity] Combine(All_of, elts) =>
        List.for_all(nullable, elts)
      | [@implicit_arity] Combine(Any_of | One_of, elts) =>
        List.exists(nullable, elts)
      | Def_ref(path) => nullable(List.assoc(path, definitions))
      | Id_ref(id) => nullable(List.assoc(id, ids))
      | [@implicit_arity] Combine(Not, _)
      | Dummy => assert(false)
      };
    nullable(root);
  };

  /*-- default specs ---------------------------------------------------------*/

  let array_specs = {
    min_items: 0,
    max_items: None,
    unique_items: false,
    additional_items: None,
  };
  let object_specs = {
    properties: [],
    pattern_properties: [],
    additional_properties: Some(element(Any)),
    min_properties: 0,
    max_properties: None,
    schema_dependencies: [],
    property_dependencies: [],
  };
  let string_specs = {pattern: None, min_length: 0, max_length: None};
  let numeric_specs = {multiple_of: None, minimum: None, maximum: None};
};

include Make(Json_repr.Ezjsonm);
