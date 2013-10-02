var WrongNumberOfArguments = function(msg) {
    this.message = msg;
    this.name = "WrongNumberOfArguments";
};

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

exports.message = function () {
    if (arguments.length < 2) {
        console.log(arguments);
    }
    else {
        // Pretty sure this is rubbish, much better version needed
        var fmt_convert = function (value, spec) {
            if (spec=="s") {
                return "" + value;
            }
            else if (spec=="d") {
                return "" + parseInt("" + value);
            }
            else if (spec=="f") {
                return "" + parseFloat("" + value);
            }
            else {
                return "ERROR";
            }
        };
        var args = arguments.reverse();
        var format_string = args.pop();
        format_string.split(/(%[sd])/).map(
            function (e) { 
                return (e.indexOf("%") == 0) ? args.pop():e;
            }
        );
    }
};
