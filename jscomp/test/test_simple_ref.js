// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

var v = [
  0,
  0
];

function gen() {
  ++ v[1];
  return v[1];
}

var h = [
  0,
  0
];

var a = 0;

var c = [
  0,
  0
];

var not_real_escape = a;

function real_escape(f, _) {
  return Caml_curry.app1(f, c);
}

var u = h;

exports.u               = u;
exports.gen             = gen;
exports.not_real_escape = not_real_escape;
exports.real_escape     = real_escape;
/* No side effect */