// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Caml_exceptions  = require("../runtime/caml_exceptions");
var Caml_curry       = require("../runtime/caml_curry");

var Undefined = [
  248,
  "CamlinternalLazy.Undefined",
  ++ Caml_exceptions.caml_oo_last_id
];

function raise_undefined() {
  throw Undefined;
}

function force_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  try {
    var result = Caml_curry.app1(closure, /* () */0);
    blk[0] = result;
    Caml_obj_runtime.caml_obj_set_tag(blk, 250);
    return result;
  }
  catch (e){
    blk[0] = function () {
      throw e;
    };
    throw e;
  }
}

function force_val_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  var result = Caml_curry.app1(closure, /* () */0);
  blk[0] = result;
  Caml_obj_runtime.caml_obj_set_tag(blk, 250);
  return result;
}

function force(lzv) {
  var t = Caml_obj_runtime.caml_obj_tag(lzv);
  if (t === 250) {
    return lzv[0];
  }
  else if (t !== 246) {
    return lzv;
  }
  else {
    return force_lazy_block(lzv);
  }
}

function force_val(lzv) {
  var t = Caml_obj_runtime.caml_obj_tag(lzv);
  if (t === 250) {
    return lzv[0];
  }
  else if (t !== 246) {
    return lzv;
  }
  else {
    return force_val_lazy_block(lzv);
  }
}

exports.Undefined            = Undefined;
exports.force_lazy_block     = force_lazy_block;
exports.force_val_lazy_block = force_val_lazy_block;
exports.force                = force;
exports.force_val            = force_val;
/* No side effect */