/** Abstract representation of JSON schemas as of version
    [http://json-schema.org/draft-04/schema#]. */;

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

/** {2 Abstract representation of schemas} */; /******************************/

/** A JSON schema root. */

type schema

/** A node in the schema, embeds all type-agnostic specs. */

and element = {
  /** An optional short description. */
  title: option(string),
  /** An optional long description. */
  description: option(string),
  /** A default constant to be substituted in case of a missing value. */
  default: option(Json_repr.any),
  /** A valid value must equal one of these constants. */
  enum: option(list(Json_repr.any)),
  /** The type-specific part. */
  kind: element_kind,
  /** predefined formats such as [date-time], [email], [ipv4], [ipv6], [uri]. */
  format: option(string),
  /** An optional ID. */
  id: option(string),
}

/** The type-specific part of schema nodes. */

and element_kind =
  | /** The type of an object. */
    Object(object_specs)
  | /** An fixed-length array with the types of its elements (a tuple). */
    Array(
      list(element),
      array_specs,
    )
  | /** A variable-length array with the type of its children. */
    Monomorphic_array(
      element,
      array_specs,
    )
  | /** A mix of schemas using logical combinators. */
    Combine(
      combinator,
      list(element),
    )
  | /** A ref to an element from its path in the JSON representation. */
    Def_ref(
      Json_query.path,
    )
  | /** A ref to an element from its ID. */
    Id_ref(string)
  | /** A ref to an external element. */
    Ext_ref(string)
  | /** A string (with optional characteristics). */
    String(string_specs)
  | /** An int (with optional characteristics). */
    Integer(numeric_specs)
  | /** A float (with optional characteristics). */
    Number(numeric_specs)
  | /** Any boolean. */
    Boolean
  | /** The null value. */
    Null
  | /** Any JSON value. */
    Any
  | /** For building cyclic definitions, a definition bound to a dummy
      will be considered absent for {!add_definition} but present
      for {!update}. The idea is to insert a dummy definition, build a
      cyclic structure using it for recursion, and finally update the
      definition with the structure. */
    Dummy

/** Grammar combinators. */

and combinator =
  | /** Logical OR n-ary combinator. */
    Any_of
  | /** Logical XOR n-ary combinator. */
    One_of
  | /** Logical AND n-ary combinator. */
    All_of
  | /** Logical NOT unary combinator. */
    Not

/** Parameters of the [Array] and [MonomorphicArray] type specifiers. */

and array_specs = {
  /** The minimum number of elements. */
  min_items: int,
  /** The maximum number of elements. */
  max_items: option(int),
  /** Teels if all elements must be different. */
  unique_items: bool,
  /** The type of additional items, if allowed. */
  additional_items: option(element),
}

/** Parameters of the [Integer] and [Number] type specifiers. */

and numeric_specs = {
  /** An optional divisor of valid values */
  multiple_of: option(float),
  /** The optional lower bound of the numeric range */
  minimum: option((float, [ | `Inclusive | `Exclusive])),
  /** The optional upper bound of the numeric range */
  maximum: option((float, [ | `Inclusive | `Exclusive])),
}

/** Parameters of the [Object] type specifier. */

and object_specs = {
  /** The names and types of properties, with a flag to indicate if
        they are required ([true]) or optional. */
  properties: list((string, element, bool, option(Json_repr.any))),
  /** Alternative definition of properties, matching field names
        using regexps instead of constant strings. */
  pattern_properties: list((string, element)),
  /** The type of additional properties, if allowed. */
  additional_properties: option(element),
  /** The minimum number of properties. */
  min_properties: int,
  /** The maximum number of properties. */
  max_properties: option(int),
  /** Additional schemas the value must verify if a property is
        present (property, additional schema). */
  schema_dependencies: list((string, element)),
  /** Additional properties required whenever some property is
        present (property, additional properties). */
  property_dependencies: list((string, list(string))),
}

/** Parameters of the [String] type specifier. */

and string_specs = {
  /** A regexp the string must conform to. */
  pattern: option(string),
  /** The minimum string length. */
  min_length: int,
  /** The maximum string length. */
  max_length: option(int),
};

/** {2 Combinators to build schemas and elements} */; /*************************/

/** Construct a naked element (all optional properties to None). */

let element: element_kind => element;

/** Construct a schema from its root, without any definition ; the
    element is checked not to contain any [Def] element. */

let create: element => schema;

/** Extract the root element from an existing schema. */

let root: schema => element;

/** Update a schema from its root, using the definitions from an
    existing schema ; the element is checked to contain only valid
    [Def] elements ; unused definitions are kept, see {!simplify}. */

let update: (element, schema) => schema;

/** Describes the implemented schema specification as a schema. */

let self: schema;

/** A completely generic schema, without any definition. */

let any: schema;

/** Combines several schemas. */

let combine: (combinator, list(schema)) => schema;

/** Tells is a schema accepts null. */

let is_nullable: schema => bool;

/** {2 Named definitions} */; /***********************************************/

/** Merges the definitions of two schemas if possible and returns the
    updated schemas, so that their elements can be mixed without
    introducing dangling references ; if two different definitions are
    bound to the same path, {!Duplicate_definition} will be raised. */

let merge_definitions: ((schema, schema)) => (schema, schema);

/** Remove the definitions that are not present in the schema. */

let simplify: schema => schema;

/** Adds a definition by its path. If the path is absolute (starting
    with a ['/']), it is untouched. Otherwise, it is considered
    relative to ["#/definitions"] as recommended by the standard. May
    raise {!Duplicate_definition} if this path is already used or any
    error raised by {!Json_repr.path_of_json_pointer} with
    [~wildcards:false]. Returns the modified schema and the [Def_ref]
    node that references this definition to be used in the schema. */

let add_definition:
  (~definitions_path: string=?, string, element, schema) => (schema, element);

/** Finds a definition by its path, may raise [Not_found].
    See {!add_definition} for the name format.*/

let find_definition: (~definitions_path: string=?, string, schema) => element;

/** Tells if a path leads to a definition.
    See {!add_definition} for the name format. */

let definition_exists: (~definitions_path: string=?, string, schema) => bool;

/** Build a reference to a definition.
    See {!add_definition} for the name format. */

let definition_ref: (~definitions_path: string=?, string) => element;

/** {2 Predefined values} */; /***********************************************/

/** Default Parameters of the [Array] and [MonomorphicArray] type specifiers. */

let array_specs: array_specs;

/** Default parameters of the [Object] type specifier. */

let object_specs: object_specs;

/** Default parameters of the [String] type specifier. */

let string_specs: string_specs;

/** Default parameters of the [Integer] and [Number] type specifiers. */

let numeric_specs: numeric_specs;

/** {2 JSON Serialization} */; /*********************************************/

/** Formats a JSON schema as its JSON representation.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. */

let to_json: schema => Json_repr.ezjsonm;

/** Parse a JSON structure as a JSON schema, if possible.
    May throw {!Cannot_parse}.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. */

let of_json: Json_repr.ezjsonm => schema;

/** Formats a JSON schema in human readable format. */

let pp: (Format.formatter, schema) => unit;

/** {2 Errors} */; /**********************************************************/

/** An error happened during parsing.
    May box one of the following exceptions, among others.. */

exception Cannot_parse(Json_query.path, exn);

/** A reference to a non-existent location was detected. */

exception Dangling_reference(string);

/** A reference litteral could not be understood. */

exception Bad_reference(string);

/** An unexpected kind of JSON value was encountered. */

exception Unexpected(string, string);

/** A non-[Dummy] definition appeared twice on insertion or merge. */

exception Duplicate_definition(Json_query.path, element, element);

/** Produces a human readable version of an error. */

let print_error:
  (~print_unknown: (Format.formatter, exn) => unit=?, Format.formatter, exn) =>
  unit;

/** {2 Advanced interface for using a custom JSON representation} */; /**********/

module Make:
  (Repr: Json_repr.Repr) =>
   {
    /** Same as {!to_json} for a custom JSON representation. */

    let to_json: schema => Repr.value;

    /** Same as {!of_json} for a custom JSON representation. */

    let of_json: Repr.value => schema;
  };
