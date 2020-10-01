// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Runner V8 script for testing dart2wasm, takes ".wasm" file as argument.
// Run as follows:
//
// $> d8 --experimental-wasm-gc run.js -- <file_name>.wasm
//
// For debugging, you'll need a debug build of V8 and adding the additional flag
// --trace-wasm-decoder.

// Load binary wasm file (e.g. created by running wat2wasm on a ".wat" file).
var bytes = readbuffer(arguments[0]);
console.log("Read binary file!");

// Create a Wasm module from the arraybuffer bytes.
var module = new WebAssembly.Module(bytes);
console.log("Created module!");

// Create an instance of this Wasm module, passing in a properly crafted imports object
// to allow for custom printing.
var buff = ""
var importObject = {
    console: {
        log: arg => {
            // In general, our print function will just print an integer, except for two
            // arbitrary values (in this case -10000 and -10001), for which it will
            // either print a space or print a newline and flush the output. It would be
            // better to change these to be the smallest two integers, but this will do
            // for the time being.
            if (arg == -10000) {
                console.log(buff);
                buff = "";
            } else if (arg == -10001) {
                buff += " ";
            } else {
                buff += arg;
            }
        }
    }
};
var inst = new WebAssembly.Instance(module, importObject);

console.log("Instantiated module!");

