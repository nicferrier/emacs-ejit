var WrongNumberOfArguments = function(msg) {
    this.message = msg;
    this.name = "WrongNumberOfArguments";
};

exports.CONS = function (car, cdr) {
    return {
        car: car,
        cdr: cdr
    };
};
 
exports.CAR = function (cons) {
    return cons.car;
};
    
exports.CDR = function (cons) {
    return cons.cdr;
};

exports.CADR = function (cons) {
    return cons.cdr.car;
}

exports.CADDR = function (cons) {
    return cons.cdr.cdr.car;
}

exports.CADDDR = function (cons) {
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
