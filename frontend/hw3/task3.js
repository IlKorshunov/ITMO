// eslint-disable-next-line no-unused-vars
function calc(n) {
    if (typeof n !== 'number') {
        throw new Error("the argument is not a number");
    }
    let res = n;
    const operations = new Map([
        ['+', (a, b) => a + b],
        ['-', (a, b) => a - b],
        ['*', (a, b) => a * b],
        ['/', (a, b) => {
            if (b === 0) {
                throw new Error("Division by zero");
            }
            return a / b;
        }],
        ['%', (a, b) => a % b],
        ['**', (a, b) => a ** b]
    ]);

    function calculate(oper, num) {
        if (!operations.has(oper)) {
            throw new Error("unsupported sign");
        }
        if (typeof num !== 'number') {
            throw new Error("the argument is not a number");
        }
        res = operations.get(oper)(res, num);
        return calculate;
    }

    calculate.valueOf = function () {
        return res;
    };
    return calculate;
}

module.exports = calc;

// function main() {
//     console.log(calc(10)('+', 5).valueOf()); 
//     console.log(calc(15)('*', 2)('-', 5).valueOf());
// }

// main();
