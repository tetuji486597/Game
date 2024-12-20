// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function printGreen(s) {
  return Pervasives.print_string("\x1b[32m" + (s + "\x1b[0m\n"));
}

function printRed(s) {
  return Pervasives.print_string("\x1b[31m" + (s + "\x1b[0m\n"));
}

function checkExpect(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("checkExpectSuccess: " + message);
  } else {
    printRed("checkExpectFail: " + message);
    printRed("expected output: ");
    console.log(expected);
    printRed("actual output: ");
    console.log(actual);
    return ;
  }
}

function checkError(input, expect) {
  try {
    Curry._1(input, undefined);
    return Pervasives.failwith("Error did not occur");
  }
  catch (raw_err){
    var err = Caml_js_exceptions.internalToOCamlException(raw_err);
    if (err.RE_EXN_ID === "Failure") {
      var err$1 = err._1;
      if (err$1 === expect) {
        return printGreen("checkErrorSuccess");
      } else if (err$1 === "Error did not occur") {
        return printRed("Error did not occur");
      } else {
        return printRed("checkErrorFail. Expected error: " + (expect + ("; Actual error: " + err$1)));
      }
    }
    throw err;
  }
}

function parseBoardDims(t) {
  var v = $$String.trim(t);
  if (v === "") {
    return /* [] */0;
  }
  var s = v + " ";
  var len = s.length;
  var firstSpace = $$String.index(s, /* ' ' */32);
  var num = $$String.sub(s, 0, firstSpace);
  var remainder = $$String.sub(s, firstSpace, len - firstSpace | 0);
  return {
          hd: Caml_format.caml_int_of_string(num),
          tl: parseBoardDims(remainder)
        };
}

function getBoardWidth(dims) {
  if (!dims) {
    return Pervasives.failwith("invalid dimensions");
  }
  var match = dims.tl;
  if (match && !match.tl) {
    return match.hd;
  } else {
    return Pervasives.failwith("invalid dimensions");
  }
}

function getBoardHeight(dims) {
  if (!dims) {
    return Pervasives.failwith("invalid dimensions");
  }
  var match = dims.tl;
  if (match && !match.tl) {
    return dims.hd;
  } else {
    return Pervasives.failwith("invalid dimensions");
  }
}

exports.printGreen = printGreen;
exports.printRed = printRed;
exports.checkExpect = checkExpect;
exports.checkError = checkError;
exports.parseBoardDims = parseBoardDims;
exports.getBoardWidth = getBoardWidth;
exports.getBoardHeight = getBoardHeight;
/* No side effect */
