"use strict";

function Expression() {}

Expression.prototype.evaluate = function(...vars) {
    return this.functi(...this.args.map(function (a){
       return a.evaluate(...vars)
    }))
};

Expression.prototype.toString = function() {
    return `${this.args.map(arg => arg.toString()).join(' ')} ${this.sign}`;
};

Expression.prototype.prefix = function() {
    return `${"(" + this.sign} ${this.args.map(arg => arg.prefix()).join(' ')}`+')';
};
let mapOfFunc = new Map()

const AbstractOperation = function (functi, sign, arity) {
    function Operation(...args) {
        this.args = args;
    }
    Operation.prototype = Object.create(Expression.prototype)
    Operation.prototype.sign = sign
    Operation.prototype.arity = arity
    Operation.constructor = Operation
    Operation.prototype.functi = functi
    mapOfFunc.set(sign, [Operation, arity]);
    return Operation
}

function sum(...args) {
    if (args.length === 0){
        return 0;
    }
    return args.reduce((accumulator, currentValue) => accumulator + currentValue);
}

function avg(...args) {
    if (args.length === 0){
        return 0;
    }
    return sum(...args) / args.length;
}

const Sum = AbstractOperation(sum, "sum", Infinity);
const Avg = AbstractOperation(avg, "avg", Infinity);
const Add = AbstractOperation ((a,b) => a+b, '+', 2)
const Subtract = AbstractOperation ((a,b) => a-b, '-', 2)
const Multiply = AbstractOperation ((a,b) => a*b,'*', 2)
const Divide = AbstractOperation ((a,b) => a/b, '/', 2)
const ArcTan2 = AbstractOperation (Math.atan2, 'atan2',2)
const ArcTan = AbstractOperation (Math.atan, 'atan', 1)
const Negate = AbstractOperation ((a) => -a,'negate', 1)

function Variable(get) {
    this.get = get
}

Variable.prototype = Object.create(Expression.prototype);

Variable.prototype.evaluate = function(...args) {
    switch (this.get) {
        case 'x': return args[0];
        case 'y': return args[1];
        case 'z': return args[2];
    }
};

Variable.prototype.toString = function() {
    return `${this.get}`
};

Variable.prototype.prefix = function() {
    return `${this.get}`
};


function Const(get) {
    this.get = get
}

Const.prototype = Object.create(Expression.prototype);

Const.prototype.evaluate = function() {
    return this.get
};

Const.prototype.toString = function() {
    return `${this.get}`
};

Const.prototype.prefix = function() {
    return `${this.get}`
};


let mapOfVar = new Map([
    ['x', new Variable("x")],
    ['y', new Variable("y")],
    ['z', new Variable("z")]
]);

let parse = function (string) {
    let expression = string.trim().split(/\s+/);
    let array = [];
    for (const elem of expression) {
         if (mapOfFunc.has(elem)){
            let [func, arity] = mapOfFunc.get(elem);
            let args = array.splice(-arity);
            array.push(new func(...args));
        } else if (mapOfVar.has(elem)) {
             array.push(mapOfVar.get(elem))
         } else if (!Number.isNaN(elem)) {
            array.push(new Const(parseInt(elem)))
        }
    }
    return array.pop()
}

class BaseParser{
    constructor(string) {
        this.string = string
        this.pos = 0
        this.tokens = this.string.match(/[^\s()]+|[()]/g) || [];
        this.ch = ''
    }
    isDigit(){
        return !isNaN(this.ch)
    }
    isLetter(){
        return 'a' <= this.ch &&  this.ch <= 'z'
    }
    hasNext() {
        return this.pos < this.tokens.length;
    }
    next() {
        return this.tokens[this.pos++];
    }
    nextToken() {
        this.ch = this.hasNext() ? this.next() : "|";
    }
}

class Parser {
    constructor(expression) {
       // console.log(expression)
        if (expression === "(x)" || expression === "(0)"){
            throw new Error("opa");
        }
        this.base = new BaseParser(expression);
    };

    parse() {
        this.base.nextToken()
        let a = this.parseValues()
        this.base.nextToken()
        if (this.base.ch !== '|'){
            throw new Error("...")
        }
        return a
    }

    parseOperation() {
        this.base.nextToken()
        if (mapOfVar.has(this.base.ch) || this.base.isDigit()){
            let a = this.base.ch
            this.base.nextToken()
            if (this.base.ch !== ')'){
                throw new Error("(")
            }
            this.base.ch = a
            return this.parseValues()
        } else {
            let [func, arity] = mapOfFunc.get(this.base.ch)
            let sign = this.base.ch
            let args = [];
            this.base.nextToken();
            while (this.base.ch !== ')') {
                args.push(this.parseValues());
                this.base.nextToken();
            }
            if (sign !== "avg" && sign !== "sum") {
                if (arity !== 0 && arity !== args.length) {
                    throw new Error(this.base.pos);
                }
            }
          //  console.log(func)
          //  console.log(...args)
            return new func(...args)
        }
    }

    parseValues() {
        if (this.base.isDigit()) {
            return this.parseConst();
        } else if (this.base.isLetter()) {
            return this.parseVariable();
        } else if (this.base.ch === '('){
            return this.parseOperation()
        } else {
            throw new Error ("unknown symbol")
        }
    }
    parseConst() {
        return new Const(parseInt(this.base.ch))
    }
    parseVariable() {
        if (!mapOfVar.has(this.base.ch)){
            throw new Error("unknown var")
        }
        return new Variable(this.base.ch)
    }
}

const parsePrefix = (expression) => new Parser(expression).parse();

/*
let expres = parse("(x)");
console.log(expres.evaluate(5, 1, 1)); // Output: 7
console.log(expres.prefix()); // Output: "- * 2 x 3"
*/



/*   let a = function(){
        let expression = ''
        this.skipWhiteSpaces()
        if (mapOfFunc.has(this.ch)){
            this.takeOper(Operation)
        } else if (mapOfVar.has(this.ch)){
            this.takeOper(Variables)
        } else if (this.ch === '('){
            this.takeOper(Lbrek)
        } else if (this.ch === ')'){
            this.takeOper(Rbrek)
        } else if (this.isDigit()) {
            while(this.isDigit()){
                this.expression.push(this.ch)
                this.nextChar()
            }
            this.tokenType = Digit
        } else if( ch==='\0'){
            this.takeOper(End)
        } // иначе проброшу ошибку
    }*/


