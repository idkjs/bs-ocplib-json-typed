// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Format = require("bs-platform/lib/js/format.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_float = require("bs-platform/lib/js/caml_float.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");

function repr_uid(param) {
  return {
          contents: undefined
        };
}

function eq_repr_uid(a, ta, tb) {
  tb.contents = undefined;
  ta.contents = Caml_option.some(a);
  return tb.contents;
}

function view(v) {
  return v;
}

function repr(v) {
  return v;
}

var repr_uid$1 = {
  contents: undefined
};

var Ezjsonm = {
  view: view,
  repr: repr,
  repr_uid: repr_uid$1
};

function view$1(param) {
  if (typeof param === "string") {
    return "Null";
  }
  var variant = param.NAME;
  if (variant === "Int") {
    return {
            NAME: "Float",
            VAL: param.VAL
          };
  }
  if (variant === "Float") {
    return {
            NAME: "Float",
            VAL: param.VAL
          };
  }
  if (variant !== "Variant") {
    if (variant === "List" || variant === "Tuple") {
      return {
              NAME: "A",
              VAL: param.VAL
            };
    } else if (variant === "Bool") {
      return {
              NAME: "Bool",
              VAL: param.VAL
            };
    } else if (variant === "Assoc") {
      return {
              NAME: "O",
              VAL: param.VAL
            };
    } else {
      return {
              NAME: "String",
              VAL: param.VAL
            };
    }
  }
  var match = param.VAL;
  var x = match[1];
  var label = match[0];
  if (x !== undefined) {
    return {
            NAME: "A",
            VAL: {
              hd: {
                NAME: "String",
                VAL: label
              },
              tl: {
                hd: x,
                tl: /* [] */0
              }
            }
          };
  } else {
    return {
            NAME: "String",
            VAL: label
          };
  }
}

function repr$1(param) {
  if (typeof param === "string") {
    return "Null";
  }
  var variant = param.NAME;
  if (variant === "A") {
    return {
            NAME: "List",
            VAL: param.VAL
          };
  } else if (variant === "O") {
    return {
            NAME: "Assoc",
            VAL: param.VAL
          };
  } else if (variant === "Float") {
    return {
            NAME: "Float",
            VAL: param.VAL
          };
  } else if (variant === "Bool") {
    return {
            NAME: "Bool",
            VAL: param.VAL
          };
  } else {
    return {
            NAME: "String",
            VAL: param.VAL
          };
  }
}

var repr_uid$2 = {
  contents: undefined
};

var Yojson = {
  view: view$1,
  repr: repr$1,
  repr_uid: repr_uid$2
};

function convert(Repr_f, Repr_t, v) {
  var r = eq_repr_uid(v, Repr_f.repr_uid, Repr_t.repr_uid);
  if (r !== undefined) {
    return Caml_option.valFromOption(r);
  }
  var conv = function (v) {
    var v$1 = Curry._1(Repr_f.view, v);
    if (typeof v$1 === "string") {
      return Curry._1(Repr_t.repr, v$1);
    }
    var variant = v$1.NAME;
    if (variant === "A") {
      return Curry._1(Repr_t.repr, {
                  NAME: "A",
                  VAL: List.map(conv, v$1.VAL)
                });
    } else if (variant === "O") {
      return Curry._1(Repr_t.repr, {
                  NAME: "O",
                  VAL: List.map((function (param) {
                          return [
                                  param[0],
                                  conv(param[1])
                                ];
                        }), v$1.VAL)
                });
    } else {
      return Curry._1(Repr_t.repr, v$1);
    }
  };
  return conv(v);
}

function pp_string(ppf, s) {
  Format.fprintf(ppf, /* Format */{
        _0: {
          TAG: /* Char_literal */12,
          _0: /* '"' */34,
          _1: /* End_of_format */0
        },
        _1: "\""
      });
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    var c = Caml_string.get(s, i);
    var exit = 0;
    if (c !== 34) {
      if (c >= 32) {
        if (c !== 92) {
          Curry._1(Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* Char */0,
                      _0: /* End_of_format */0
                    },
                    _1: "%c"
                  }), c);
        } else {
          Format.fprintf(ppf, /* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "\\\\",
                  _1: /* End_of_format */0
                },
                _1: "\\\\"
              });
        }
      } else if (c >= 14) {
        exit = 1;
      } else {
        switch (c) {
          case 8 :
              Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "\\b",
                      _1: /* End_of_format */0
                    },
                    _1: "\\b"
                  });
              break;
          case 9 :
              Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "\\t",
                      _1: /* End_of_format */0
                    },
                    _1: "\\t"
                  });
              break;
          case 10 :
              Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "\\n",
                      _1: /* End_of_format */0
                    },
                    _1: "\\n"
                  });
              break;
          case 0 :
          case 1 :
          case 2 :
          case 3 :
          case 4 :
          case 5 :
          case 6 :
          case 7 :
          case 11 :
          case 12 :
              exit = 1;
              break;
          case 13 :
              Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "\\r",
                      _1: /* End_of_format */0
                    },
                    _1: "\\r"
                  });
              break;
          
        }
      }
    } else {
      Format.fprintf(ppf, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "\\\"",
              _1: /* End_of_format */0
            },
            _1: "\\\""
          });
    }
    if (exit === 1) {
      Curry._1(Format.fprintf(ppf, /* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "\\u",
                  _1: {
                    TAG: /* Int */4,
                    _0: /* Int_x */6,
                    _1: {
                      TAG: /* Lit_padding */0,
                      _0: /* Zeros */2,
                      _1: 4
                    },
                    _2: /* No_precision */0,
                    _3: /* End_of_format */0
                  }
                },
                _1: "\\u%04x"
              }), c);
    }
    
  }
  return Format.fprintf(ppf, /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* '"' */34,
                _1: /* End_of_format */0
              },
              _1: "\""
            });
}

function pp(compactOpt, pp_stringOpt, Repr, ppf, v) {
  var compact = compactOpt !== undefined ? compactOpt : false;
  var pp_string$1 = pp_stringOpt !== undefined ? pp_stringOpt : pp_string;
  var pp_compact = function (ppf, v) {
    var match = Curry._1(Repr.view, v);
    if (typeof match === "string") {
      return Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "null",
                    _1: /* End_of_format */0
                  },
                  _1: "null"
                });
    }
    var variant = match.NAME;
    if (variant === "A") {
      var pp_sep = function (ppf, param) {
        return Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* ',' */44,
                      _1: /* End_of_format */0
                    },
                    _1: ","
                  });
      };
      var partial_arg = pp_sep;
      return Curry._2(Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* '[' */91,
                        _1: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* ']' */93,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "[%a]"
                    }), (function (param, param$1) {
                    return Format.pp_print_list(partial_arg, pp_compact, param, param$1);
                  }), match.VAL);
    }
    if (variant === "O") {
      var pp_sep$1 = function (ppf, param) {
        return Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* ',' */44,
                      _1: /* End_of_format */0
                    },
                    _1: ","
                  });
      };
      var pp_field = function (ppf, param) {
        return Curry._4(Format.fprintf(ppf, /* Format */{
                        _0: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* ':' */58,
                            _1: {
                              TAG: /* Alpha */15,
                              _0: /* End_of_format */0
                            }
                          }
                        },
                        _1: "%a:%a"
                      }), pp_string$1, param[0], pp_compact, param[1]);
      };
      var partial_arg$1 = pp_sep$1;
      return Curry._2(Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* '{' */123,
                        _1: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* '}' */125,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "{%a}"
                    }), (function (param, param$1) {
                    return Format.pp_print_list(partial_arg$1, pp_field, param, param$1);
                  }), match.VAL);
    }
    if (variant !== "Float") {
      if (variant === "Bool") {
        if (match.VAL) {
          return Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "true",
                        _1: /* End_of_format */0
                      },
                      _1: "true"
                    });
        } else {
          return Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "false",
                        _1: /* End_of_format */0
                      },
                      _1: "false"
                    });
        }
      } else {
        return Curry._2(pp_string$1, ppf, match.VAL);
      }
    }
    var f = match.VAL;
    var match$1 = Caml_float.caml_modf_float(f);
    if (match$1[0] === 0.0) {
      return Curry._1(Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* Float */8,
                        _0: /* Float_f */0,
                        _1: /* No_padding */0,
                        _2: /* Lit_precision */{
                          _0: 0
                        },
                        _3: /* End_of_format */0
                      },
                      _1: "%.0f"
                    }), match$1[1]);
    } else {
      return Curry._1(Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* Float */8,
                        _0: /* Float_g */9,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      },
                      _1: "%g"
                    }), f);
    }
  };
  var pp_box = function (ppf, v) {
    var match = Curry._1(Repr.view, v);
    if (typeof match === "string") {
      return pp_compact(ppf, v);
    }
    var variant = match.NAME;
    if (variant === "A") {
      var l = match.VAL;
      if (!l) {
        return Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "[]",
                      _1: /* End_of_format */0
                    },
                    _1: "[]"
                  });
      }
      var pp_sep = function (ppf, param) {
        return Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* ',' */44,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: {
                          TAG: /* Break */0,
                          _0: "@ ",
                          _1: 1,
                          _2: 0
                        },
                        _1: /* End_of_format */0
                      }
                    },
                    _1: ",@ "
                  });
      };
      var partial_arg = pp_sep;
      return Curry._2(Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* Formatting_gen */18,
                        _0: {
                          TAG: /* Open_box */1,
                          _0: /* Format */{
                            _0: {
                              TAG: /* String_literal */11,
                              _0: "<hov 2>",
                              _1: /* End_of_format */0
                            },
                            _1: "<hov 2>"
                          }
                        },
                        _1: {
                          TAG: /* String_literal */11,
                          _0: "[ ",
                          _1: {
                            TAG: /* Alpha */15,
                            _0: {
                              TAG: /* String_literal */11,
                              _0: " ]",
                              _1: {
                                TAG: /* Formatting_lit */17,
                                _0: /* Close_box */0,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      },
                      _1: "@[<hov 2>[ %a ]@]"
                    }), (function (param, param$1) {
                    return Format.pp_print_list(partial_arg, pp_box, param, param$1);
                  }), l);
    }
    if (variant !== "O") {
      return pp_compact(ppf, v);
    }
    var l$1 = match.VAL;
    if (!l$1) {
      return Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "{}",
                    _1: /* End_of_format */0
                  },
                  _1: "{}"
                });
    }
    var pp_sep$1 = function (ppf, param) {
      return Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* ',' */44,
                    _1: {
                      TAG: /* Formatting_lit */17,
                      _0: {
                        TAG: /* Break */0,
                        _0: "@ ",
                        _1: 1,
                        _2: 0
                      },
                      _1: /* End_of_format */0
                    }
                  },
                  _1: ",@ "
                });
    };
    var pp_field = function (ppf, param) {
      return Curry._4(Format.fprintf(ppf, /* Format */{
                      _0: {
                        TAG: /* Formatting_gen */18,
                        _0: {
                          TAG: /* Open_box */1,
                          _0: /* Format */{
                            _0: {
                              TAG: /* String_literal */11,
                              _0: "<hov 2>",
                              _1: /* End_of_format */0
                            },
                            _1: "<hov 2>"
                          }
                        },
                        _1: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* ':' */58,
                            _1: {
                              TAG: /* Formatting_lit */17,
                              _0: {
                                TAG: /* Break */0,
                                _0: "@ ",
                                _1: 1,
                                _2: 0
                              },
                              _1: {
                                TAG: /* Alpha */15,
                                _0: {
                                  TAG: /* Formatting_lit */17,
                                  _0: /* Close_box */0,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        }
                      },
                      _1: "@[<hov 2>%a:@ %a@]"
                    }), pp_string$1, param[0], pp_box, param[1]);
    };
    var partial_arg$1 = pp_sep$1;
    return Curry._2(Format.fprintf(ppf, /* Format */{
                    _0: {
                      TAG: /* Formatting_gen */18,
                      _0: {
                        TAG: /* Open_box */1,
                        _0: /* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "<hov 2>",
                            _1: /* End_of_format */0
                          },
                          _1: "<hov 2>"
                        }
                      },
                      _1: {
                        TAG: /* String_literal */11,
                        _0: "{ ",
                        _1: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* String_literal */11,
                            _0: " }",
                            _1: {
                              TAG: /* Formatting_lit */17,
                              _0: /* Close_box */0,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    },
                    _1: "@[<hov 2>{ %a }@]"
                  }), (function (param, param$1) {
                  return Format.pp_print_list(partial_arg$1, pp_field, param, param$1);
                }), l$1);
  };
  if (compact) {
    return pp_compact(ppf, v);
  } else {
    return pp_box(ppf, v);
  }
}

function from_yojson(non_basic) {
  var to_basic = function (non_basic) {
    if (typeof non_basic === "string") {
      return "Null";
    }
    var variant = non_basic.NAME;
    if (variant === "Int") {
      return {
              NAME: "Int",
              VAL: non_basic.VAL
            };
    }
    if (variant === "Float") {
      return {
              NAME: "Float",
              VAL: non_basic.VAL
            };
    }
    if (variant !== "Variant") {
      if (variant === "List" || variant === "Tuple") {
        return {
                NAME: "List",
                VAL: List.map(to_basic, non_basic.VAL)
              };
      } else if (variant === "Bool") {
        return {
                NAME: "Bool",
                VAL: non_basic.VAL
              };
      } else if (variant === "Assoc") {
        return {
                NAME: "Assoc",
                VAL: List.map((function (param) {
                        return [
                                param[0],
                                to_basic(param[1])
                              ];
                      }), non_basic.VAL)
              };
      } else {
        return {
                NAME: "String",
                VAL: non_basic.VAL
              };
      }
    }
    var match = non_basic.VAL;
    var x = match[1];
    var label = match[0];
    if (x !== undefined) {
      return {
              NAME: "List",
              VAL: {
                hd: {
                  NAME: "String",
                  VAL: label
                },
                tl: {
                  hd: to_basic(x),
                  tl: /* [] */0
                }
              }
            };
    } else {
      return {
              NAME: "String",
              VAL: label
            };
    }
  };
  var to_value = function (param) {
    if (typeof param === "string") {
      return "Null";
    }
    var variant = param.NAME;
    if (variant === "Int" || variant === "Float") {
      return {
              NAME: "Float",
              VAL: param.VAL
            };
    } else if (variant === "Bool") {
      return {
              NAME: "Bool",
              VAL: param.VAL
            };
    } else if (variant === "List") {
      return {
              NAME: "A",
              VAL: List.map(to_value, param.VAL)
            };
    } else if (variant === "Assoc") {
      return {
              NAME: "O",
              VAL: List.map((function (param) {
                      return [
                              param[0],
                              to_value(param[1])
                            ];
                    }), param.VAL)
            };
    } else {
      return {
              NAME: "String",
              VAL: param.VAL
            };
    }
  };
  return to_value(to_basic(non_basic));
}

function to_yojson(json) {
  var aux = function (param) {
    if (typeof param === "string") {
      return "Null";
    }
    var variant = param.NAME;
    if (variant === "A") {
      return {
              NAME: "List",
              VAL: List.map(aux, param.VAL)
            };
    }
    if (variant === "O") {
      return {
              NAME: "Assoc",
              VAL: List.map((function (param) {
                      return [
                              param[0],
                              aux(param[1])
                            ];
                    }), param.VAL)
            };
    }
    if (variant !== "Float") {
      if (variant === "Bool") {
        return {
                NAME: "Bool",
                VAL: param.VAL
              };
      } else {
        return {
                NAME: "String",
                VAL: param.VAL
              };
      }
    }
    var f = param.VAL;
    var match = Caml_float.caml_modf_float(f);
    var intr = match[1];
    var max_intf = 1073741823;
    var min_intf = -max_intf - 1;
    if (match[0] === 0.0) {
      if (intr >= min_intf && intr <= max_intf) {
        return {
                NAME: "Int",
                VAL: intr | 0
              };
      } else {
        return {
                NAME: "Intlit",
                VAL: Curry._1(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* Float */8,
                            _0: /* Float_f */0,
                            _1: /* No_padding */0,
                            _2: /* Lit_precision */{
                              _0: 0
                            },
                            _3: /* End_of_format */0
                          },
                          _1: "%.0f"
                        }), intr)
              };
      }
    } else {
      return {
              NAME: "Float",
              VAL: f
            };
    }
  };
  return aux(json);
}

function pp_any(compact, pp_string, param, ppf, param$1) {
  return pp(compact, pp_string, param$1._0, ppf, param$1._1);
}

function any_to_repr(repr_t, param) {
  return convert(param._0, repr_t, param._1);
}

function repr_to_any(repr, v) {
  return /* Value_with_repr */{
          _0: repr,
          _1: v
        };
}

function from_any(repr) {
  return any_to_repr(Ezjsonm, repr);
}

function to_any(v) {
  return /* Value_with_repr */{
          _0: Ezjsonm,
          _1: v
        };
}

exports.repr_uid = repr_uid;
exports.convert = convert;
exports.pp = pp;
exports.Ezjsonm = Ezjsonm;
exports.Yojson = Yojson;
exports.any_to_repr = any_to_repr;
exports.repr_to_any = repr_to_any;
exports.pp_any = pp_any;
exports.from_yojson = from_yojson;
exports.to_yojson = to_yojson;
exports.from_any = from_any;
exports.to_any = to_any;
/* Format Not a pure module */
