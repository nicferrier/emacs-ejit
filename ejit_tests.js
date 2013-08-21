// ejit-tests.el -- tests for ejit core js

var assert = require ("assert");
var ejit = require ("./ejit.js");

assert.ok((ejit.CONS(1, 2).car == 1), "CONS has bad car");
assert.ok( (ejit.CONS(1, 2).cdr == 2), "CONS has bad cdr");
assert.ok( (ejit.CONS(1, ejit.CONS(2, null)).cdr.car == 2), "CONS has bad cadr");

(function  (myfunc) { 
     (function  (a,b) { 
          MULT (PLUS (a, car (b)), 2) ;
      })(); 
})();
