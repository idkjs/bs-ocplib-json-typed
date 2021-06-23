/** Representations of JSON documents */;

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

/** {2 Abstraction over JSON representations} */; /*****************************/

/** The internal format used by the library. A common format to view
    JSON structures from different representations. It only shows the
    head of structures, hiding the contents of fields, so that the
    conversion from another format or a stream can be done lazily. */

type view('a) = [
  | /** An associative table (object). */
    `O(list((string, 'a)))
  | /** An (integer indexed) array. */
    `A(list('a))
  | /** A JS boolean [true] or [false]. */
    `Bool(bool)
  | /** A floating point number (double precision). */
    `Float(float)
  | /** An UTF-8 encoded string. */
    `String(string)
  | /** The [null] constant. */
    `Null
];

/** Each representation must provide a unique identifier, obtained via
    the {!repr_uid} function. This identifier is used when converting
    between representations, to optimize out a copy when converting
    from a representation to itself. Beware that this optimization
    relies only on this [uid] token. Converting between values of the
    same type using two different representation modules with
    different [uid]s will perform a copy. A practical way to ensure
    that the optimization is made is to write your representations as
    toplevel modules, and not inside functors. */

type repr_uid('a);

/** See {!type:repr_uid}. */

let repr_uid: unit => repr_uid('a);

/** A view over a given implementation. */

module type Repr = {
  /** The implementation type. */

  type value;

  /** View a value in the common format. */

  let view: value => view(value);

  /** Builds a value from a view */

  let repr: view(value) => value;

  /** See {!type:repr_uid}. */

  let repr_uid: repr_uid(value);
};

/** Convert a JSON value from one representation to another. */

let convert:
  (
    (module Repr with type value = 'tf),
    (module Repr with type value = 'tt),
    'tf
  ) =>
  'tt;

/** Generic pretty-printer. If [compact] is set (by default), then the
    output is not really pretty (no space is output). Ascii-compatible
    string encoding is expected, as printing only escapes double
    quotes and control characters. Use [pp_string] for more advanced
    escaping. This function does not claim to be the best JSON pretty
    printer, it is mostly a small utility. */

let pp:
  (
    ~compact: bool=?,
    ~pp_string: (Format.formatter, string) => unit=?,
    (module Repr with type value = 'tf),
    Format.formatter,
    'tf
  ) =>
  unit;

/** {2 Third party in-memory JSON document representations} */; /****************/

/** A JSON value compatible with {!Ezjsonm.value}. */

type ezjsonm = [
  | /** An associative table (object). */
    `O(list((string, ezjsonm)))
  | /** An (integer indexed) array. */
    `A(list(ezjsonm))
  | /** A JS boolean [true] or [false]. */
    `Bool(bool)
  | /** A floating point number (double precision). */
    `Float(float)
  | /** An UTF-8 encoded string. */
    `String(string)
  | /** The [null] constant. */
    `Null
];

/** A view over the {!type:ezjsonm} representation.*/

module Ezjsonm: Repr with type value = ezjsonm;

/** A JSON value compatible with {!Yojson.Safe.json}. */

type yojson = [
  | /** A JS boolean [true] of [false]. */
    `Bool(bool)
  | /** JSON object. */
    `Assoc(list((string, yojson)))
  | /** A floating point number (double precision). */
    `Float(float)
  | /** A number without decimal point or exponent. */
    `Int(int)
  | /** A number without decimal point or exponent, preserved as string. */
    `Intlit(
      string,
    )
  | /** A JS array. */
    `List(list(yojson))
  | /** The [null] constant. */
    `Null
  | /** An UTF-8 encoded string. */
    `String(string)
  | /** A tuple (non-standard). Syntax: ("abc", 123). */
    `Tuple(list(yojson))
  | /** A variant (non-standard). Syntax: <"Foo"> or <"Bar": 123>. */
    `Variant(
      string,
      option(yojson),
    )
];

/** A view over the {!yojson} representation.*/

module Yojson: Repr with type value = yojson;

/** {2 Representation-agnostic JSON format} */; /********************************/

/** A meta-representation for JSON values that can unify values of
    different representations by boxing them with their corresponding
    {!Repr} modules. */

type any = pri | Value_with_repr((module Repr with type value = 'a), 'a): any;

/** Converts a boxed value from its intrinsic representation to the
    one of the given {!Repr} module. Optimized if the internal
    representation of the value actually is the requested one. */

let any_to_repr: ((module Repr with type value = 'a), any) => 'a;

/** Boxes a value with a compatible {!Repr} module. */

let repr_to_any: ((module Repr with type value = 'a), 'a) => any;

/** Pretty-printer for values of type {!any}. See {!pp} for details. */

let pp_any:
  (
    ~compact: bool=?,
    ~pp_string: (Format.formatter, string) => unit=?,
    unit,
    Format.formatter,
    any
  ) =>
  unit;

/** {2 Predefined converters for {!type:ezjsonm}} */; /********************************/

/** Conversion helper. */

let from_yojson: [< yojson] => [> ezjsonm];

/** Conversion helper. */

let to_yojson: [< ezjsonm] => [> yojson];

/** Converts a boxed value from its representation to {!ezjsonm}. */

let from_any: any => [> ezjsonm];

/** Boxes as {!ezjsonm} value. */

let to_any: [< ezjsonm] => any;
