"use strict";

let abstractFunc = f => (...args) => (x, y, z) => f(...args.map(func => func(x, y, z)))


let add = abstractFunc((a, b) => (a + b));
let negate = abstractFunc((a) => (-a));
let subtract = abstractFunc((a, b) => (a - b));
let floor = abstractFunc((a) => (Math.floor(a)));
let ceil = abstractFunc((a) => (Math.ceil(a)));
let madd = abstractFunc((a, b, c) => (a * b + c));
let multiply = abstractFunc((a, b) => (a * b));
let divide = abstractFunc((a, b) => (a / b));
let cnst = val => () => val
let pi = () => Math.PI;
let e = () => Math.E;
let cosh = abstractFunc(a => Math.cosh(a));
let sinh = abstractFunc(a => Math.sinh(a));

let variable = variab => (x, y, z) => {
    switch (variab) {
        case "x" :
            return x;
        case "y" :
            return y;
        case "z" :
            return z;
    }
};

let mapOfBinFunc = new Map([
    ['+', add],
    ['-', subtract],
    ['*', multiply],
    ['/', divide],
    ['*+', madd]
]);

let mapOfThirdFunc = new Map([
    ["*+", madd],
])

let mapOfUnarFunc = new Map([
    ['negate', negate],
    ['_', floor],
    ['^', ceil]
]);

const one = cnst(1);
const two = cnst(2);

let mapOfConst = new Map([
    ['one', cnst(1)],
    ['two', cnst(2)],
]);

let mapOfVar = new Map([
    ['x', variable("x")],
    ['y', variable("y")],
    ['z', variable("z")]
]);
let parse = function (string) {
    let expression = string.split(/\s+/);
    let array = [];
    for (const elem of expression) {
        if (elem === "") {
            continue;
        } else if (mapOfConst.has(elem)) {
            array.push(mapOfConst.get(elem))
        } else if (mapOfVar.has(elem)) {
            array.push(mapOfVar.get(elem))
        } else if (mapOfUnarFunc.has(elem)) {
            array.push(mapOfUnarFunc.get(elem)(array.pop()))
        } else if (mapOfThirdFunc.has(elem)) {
            const first = array.pop();
            const second = array.pop();
            array.push(mapOfThirdFunc.get(elem)(array.pop(), second, first))
        } else if (mapOfBinFunc.has(elem)) {
            const first = array.pop();
            array.push(mapOfBinFunc.get(elem)(array.pop(), first))
        } else {
            array.push(cnst(parseInt(elem)))
        }

    }
    return array.pop()
}