var WrongNumberOfArguments = function(msg) {
    this.message = msg;
    this.name = "WrongNumberOfArguments";
};

// The two emacs-lisp namespaces
exports.vars = {};
exports.functions = {};

exports.JSREQUIRE = function (symbol) {
    // Loads symbol (a string) into the current scope.
    var path = process.cwd() + "/" + symbol;
    try {
        var loadthing = require (path);
        for (var key in loadthing) {
            exports[key] = loadthing[key];
        }
    }
    catch (e) {
        throw new Exception("failed to require with " + path);
    }
};

exports.require = function (symbol) {
    // We need the ejit namespace to have been established.
    child_process.spawn(
        ejit.emacs_process,
        ["-batch", ""]
    );
};

exports.cons = function (car, cdr) {
    return {
        car: car,
        cdr: cdr
    };
};
 
exports.car = function (cons) {
    return cons.car;
};
    
exports.cdr = function (cons) {
    return cons.cdr;
};

exports.cadr = function (cons) {
    return cons.cdr.car;
}

exports.caddr = function (cons) {
    return cons.cdr.cdr.car;
}

exports.cadddr = function (cons) {
    return cons.cdr.cdr.cdr.car;
}

exports.PLUS = function ()  {
    var a=0; 
    for (var i=0; i < arguments.length; i++) { 
        a = a + arguments[i];
    }
    return a;
};

exports.MULT = function ()  {
    var a=1; 
    for (var i=0; i < arguments.length; i++) { 
        a = a * arguments[i];
    }
    return a;
};

exports.MINUS = function ()  {
    if (arguments.length == 1) {
        return 0 - arguments[0];
    }
    else {
        var a = arguments[0]; 
        for (var i=1; i < arguments.length; i++) { 
            a = a - arguments[i];
        }
        return a;
    }
};

exports.DIVIDE = function ()  {
    if (arguments.length < 2) {
        throw new WrongNumberOfArguments("wrong number of arguments");
    }
    else {
        var a = arguments[0]; 
        for (var i=1; i < arguments.length; i++) { 
            a = a - arguments[i];
        }
        return a;
    }
};


// And the std library

var utils = require("util");

exports.functions.message = function () {
    if (arguments.length < 2) {
        console.log(arguments);
    }
    else {
      console.log(utils.format(arguments));
    }
};
