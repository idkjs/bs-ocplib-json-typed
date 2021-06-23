/* Queries in JSON documents */

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

type path = list(path_item)

and path_item = [ | `Field(string) | `Index(int) | `Star | `Next];

exception Illegal_pointer_notation(string, int, string);
exception Unsupported_path_item(path_item, string);
exception Cannot_merge(path);

/*-- path operations -------------------------------------------------------*/

let print_path_as_json_path = (~wildcards=true, ppf) =>
  fun
  | [] => Format.fprintf(ppf, "/")
  | nonempty => {
      let rec print = ppf => (
        fun
        | [] => ()
        | [`Field(n), ...rem] => Format.fprintf(ppf, "/%s%a", n, print, rem)
        | [`Index(n), ...rem] => Format.fprintf(ppf, "[%d]%a", n, print, rem)
        | [`Next, ...rem] when wildcards =>
          Format.fprintf(ppf, "-%a", print, rem)
        | [`Star, ...rem] when wildcards =>
          Format.fprintf(ppf, "*%a", print, rem)
        | [`Next | `Star, ..._] =>
          raise(
            [@implicit_arity]
            Unsupported_path_item(`Star, "JSON path w/o wildcards"),
          )
      );
      print(ppf, nonempty);
    };

let print_path_as_json_pointer = (~wildcards=true, ppf) =>
  fun
  | [] => Format.fprintf(ppf, "/")
  | nonempty => {
      let rec print = ppf => (
        fun
        | [] => ()
        | [`Field(n), ...rem] => Format.fprintf(ppf, "/%s%a", n, print, rem)
        | [`Index(n), ...rem] => Format.fprintf(ppf, "/%d%a", n, print, rem)
        | [`Next, ...rem] when wildcards =>
          Format.fprintf(ppf, "/-%a", print, rem)
        | [`Next, ..._] =>
          raise(
            [@implicit_arity]
            Unsupported_path_item(`Star, "JSON pointer w/o wildcards"),
          )
        | [`Star, ..._] =>
          raise(
            [@implicit_arity] Unsupported_path_item(`Star, "JSON pointer"),
          )
      );
      print(ppf, nonempty);
    };

let json_pointer_of_path = (~wildcards=?, path) =>
  Format.asprintf("%a", print_path_as_json_pointer(~wildcards?), path);

let path_of_json_pointer = (~wildcards=true, str) => {
  let buf = Buffer.create(100);
  let len = String.length(str);
  let rec slashes = (acc, i) =>
    if (i >= len) {
      List.rev(acc);
    } else if (str.[i] == '/') {
      slashes(acc, i + 1);
    } else {
      item(acc, i);
    }
  and item = (acc, i) =>
    if (i >= len) {
      List.rev([interp(), ...acc]);
    } else {
      switch (str.[i]) {
      | '/' => slashes([interp(), ...acc], i)
      | '~' =>
        if (i + 1 >= len) {
          raise(
            [@implicit_arity]
            Illegal_pointer_notation(str, i, "Unterminated escape sequence"),
          );
        };
        switch (str.[i]) {
        | '0' => Buffer.add_char(buf, '~')
        | '1' => Buffer.add_char(buf, '/')
        | _illegal =>
          raise(
            [@implicit_arity]
            Illegal_pointer_notation(str, i + 1, "Illegal escape character"),
          )
        };
        item(acc, i + 1);
      | unescaped =>
        Buffer.add_char(buf, unescaped);
        item(acc, i + 1);
      };
    }
  and interp = () => {
    let field = Buffer.contents(buf);
    Buffer.clear(buf);
    if (field == "-") {
      if (wildcards) {
        `Next;
      } else {
        raise(
          [@implicit_arity]
          Unsupported_path_item(`Next, "JSON pointer w/o wildcards"),
        );
      };
    } else {
      try(`Index(int_of_string(field))) {
      | _ => `Field(field)
      };
    };
  };
  if (len == 0) {
    [];
  } else if (str.[0] != '/') {
    raise(
      [@implicit_arity]
      Illegal_pointer_notation(str, 0, "Missing initial slash"),
    );
  } else {
    slashes([], 1);
  };
};

/*-- queries ---------------------------------------------------------------*/

module Make = (Repr: Json_repr.Repr) => {
  let rec query = (path, json) =>
    switch (path, Repr.view(json)) {
    | ([], _) => json
    | ([`Field(n), ...rempath], `O([(n', v), ...rem])) =>
      if (n == n') {
        query(rempath, v);
      } else {
        query(path, Repr.repr(`O(rem)));
      }
    | ([`Index(i), ...rempath], `A(cells)) =>
      let i =
        if (i < 0) {
          List.length(cells) - i;
        } else {
          i;
        };
      query(rempath, List.nth(cells, i));
    | ([`Star, ...rempath], `O([(_, v), ...rem])) =>
      try(query(rempath, v)) {
      | Not_found => query(path, Repr.repr(`O(rem)))
      }
    | ([`Star, ...rempath], `A([v, ...rem])) =>
      try(query(rempath, v)) {
      | Not_found => query(path, Repr.repr(`A(rem)))
      }
    | (_, _) => raise(Not_found)
    };

  let query_all = (path, json) => {
    let res = ref([]);
    let rec query = (path, json) =>
      switch (path, Repr.view(json)) {
      | ([], _) => res := [json, ...res^]
      | ([`Field(n), ...rempath], `O([(n', v), ...rem])) =>
        if (n == n') {
          query(rempath, v);
        } else {
          query(path, Repr.repr(`O(rem)));
        }
      | ([`Index(i), ...rempath], `A(cells)) =>
        let i =
          if (i < 0) {
            List.length(cells) - i;
          } else {
            i;
          };
        query(rempath, List.nth(cells, i));
      | ([`Star, ...rempath], `O(fields)) =>
        List.iter(((_, v)) => query(rempath, v), fields)
      | ([`Star, ...rempath], `A(cells)) =>
        List.iter(query(rempath), cells)
      | (_, _) => ()
      };
    query(path, json);
    res^;
  };

  /*-- updates ---------------------------------------------------------------*/

  let sort_fields = List.sort(((l, _), (r, _)) => compare(l, r));

  let equals = (l, r) => {
    let rec canon = v =>
      switch (Repr.view(v)) {
      | `O(l) =>
        Repr.repr(
          `O(List.map(((n, o)) => (n, canon(o)), l) |> sort_fields),
        )
      | `A(l) => Repr.repr(`A(List.map(canon, l)))
      | _ => v
      };
    canon(l) == canon(r);
  };

  let merge = (l, r) => {
    let rec merge = (path, l, r) =>
      switch (Repr.view(l), Repr.view(r)) {
      | (`O(l), `O(r)) =>
        Repr.repr(`O(merge_fields(path, [], sort_fields(l @ r))))
      | (`Null, v)
      | (v, `Null) => Repr.repr(v)
      | (`A(l), `A(r)) => Repr.repr(`A(merge_cells(path, 0, [], l, r)))
      | _ =>
        if (equals(l, r)) {
          l;
        } else {
          raise(Cannot_merge(List.rev(path)));
        }
      }
    and merge_cells = (path, i, acc, l, r) =>
      switch (l, r) {
      | ([], rem)
      | (rem, []) => List.rev_append(acc, rem)
      | ([l, ...ls], [r, ...rs]) =>
        let item = merge([`Index(i), ...path], l, r);
        merge_cells(path, succ(i), [item, ...acc], ls, rs);
      }
    and merge_fields = (path, acc) =>
      fun
      | [(lf, lv), ...[(rf, rv), ...rem] as rrem] =>
        if (lf == rf) {
          let item = merge([`Field(lf), ...path], lv, rv);
          merge_fields(path, [(lf, item), ...acc], rem);
        } else {
          merge_fields(path, [(lf, lv), ...acc], rrem);
        }
      | ([_] | []) as last => last;
    merge([], l, r);
  };

  let insert = (~merge=merge, path, value, root) => {
    let revpath = sub => {
      let rec loop = acc =>
        fun
        | l when l === sub => List.rev(acc)
        | [item, ...items] => loop([item, ...acc], items)
        | [] => /* absurd */ assert(false);
      loop([], path);
    };
    let merge = (path, l, r) =>
      try(merge(l, r)) {
      | Cannot_merge(sub) => raise(Cannot_merge(revpath(path) @ sub))
      };
    let rec nulls = (acc, n, last) =>
      if (n <= 0) {
        List.rev([last, ...acc]);
      } else {
        nulls([Repr.repr(`Null), ...acc], pred(n), last);
      };
    let rec insert = (~root=?, path) => {
      let root =
        switch (root) {
        | None => None
        | Some(repr) => Some(Repr.view(repr))
        };
      switch (path, root) {
      /* create objects */
      | ([`Field(n), ...rempath], None) =>
        Repr.repr(`O([(n, insert(rempath))]))
      | ([`Index(0) | `Star | `Next, ...rempath], None) =>
        Repr.repr(`A([insert(rempath)]))
      | ([`Index(i), ...rempath], None) =>
        if (i < 0) {
          raise(Cannot_merge(revpath(path)));
        };
        Repr.repr(`A(nulls([], max(0, pred(i)), insert(rempath))));
      | ([], None) => value
      /* insert in existing */
      | ([], Some(value')) => merge(path, value, Repr.repr(value'))
      | ([`Field(n), ...rempath], Some(`O(fields))) =>
        Repr.repr(`O(insert_fields([], n, rempath, fields)))
      | ([`Index(i), ...rempath], Some(`A(cells))) =>
        let i =
          if (i < 0) {
            List.length(cells) - i;
          } else {
            i;
          };
        if (i < 0) {
          raise(Cannot_merge(revpath(path)));
        };
        Repr.repr(`A(insert_cells([], i, rempath, cells)));
      | ([`Next, ...rempath], Some(`A(cells))) =>
        Repr.repr(
          `A(List.rev_append(List.rev(cells), [insert(rempath)])),
        )
      /* multiple insertions */
      | ([`Star, ...rempath], Some(`A(cells))) =>
        Repr.repr(`A(List.map(root => insert(~root, rempath), cells)))
      | ([`Star, ...rempath], Some(`O(fields))) =>
        Repr.repr(
          `O(
            List.map(((n, root)) => (n, insert(~root, rempath)), fields),
          ),
        )
      | ([`Star], Some(root)) => merge(path, value, Repr.repr(root))
      /* FIXME: make explicit unhandled cases */
      | (_, Some(_)) => raise(Cannot_merge(revpath(path)))
      };
    }
    and insert_fields = (acc, n, rempath, fields) =>
      switch (fields) {
      | [] => List.rev([(n, insert(rempath)), ...acc])
      | [(n', root), ...rem] when n == n' =>
        List.rev_append([(n, insert(~root, rempath)), ...acc], rem)
      | [other, ...rem] => insert_fields([other, ...acc], n, rempath, rem)
      }
    and insert_cells = (acc, n, rempath, cells) =>
      switch (cells, n) {
      | ([], n) => nulls(acc, n, insert(rempath))
      | ([root, ...rem], 0) =>
        List.rev_append([insert(~root, rempath), ...acc], rem)
      | ([other, ...rem], n) =>
        insert_cells([other, ...acc], n - 1, rempath, rem)
      };
    insert(~root, path);
  };

  let replace = (path, value, root) =>
    insert(~merge=(value, _prev) => value, path, value, root);

  let insert = (path, value, root) => insert(path, value, root);
};

let path_operator_name =
  fun
  | `Field(_) => "field access"
  | `Index(_) => "array access"
  | `Star => "wildcard"
  | `Next => "array append";

let print_error = (~print_unknown=?, ppf, err) =>
  switch (err) {
  | [@implicit_arity] Illegal_pointer_notation(notation, pos, msg) =>
    Format.fprintf(
      ppf,
      "@[<v 2>Illegal pointer notation@,At character %d of %S@,%s@]",
      pos,
      notation,
      msg,
    )
  | [@implicit_arity] Unsupported_path_item(item, msg) =>
    Format.fprintf(
      ppf,
      "Path operator %s unsupported by %s",
      path_operator_name(item),
      msg,
    )
  | Cannot_merge([]) => Format.fprintf(ppf, "Unmergeable objects")
  | Cannot_merge(path) =>
    Format.fprintf(
      ppf,
      "Unmergeable objects, incompatibility at %a",
      print_path_as_json_path(~wildcards=true),
      path,
    )
  | exn =>
    switch (print_unknown) {
    | Some(print_unknown) => print_unknown(ppf, exn)
    | None =>
      Format.fprintf(ppf, "Unhandled error %s", Printexc.to_string(exn))
    }
  };

include Make(Json_repr.Ezjsonm);
