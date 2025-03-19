#!/home/korshunovi/.nvm/versions/node/v20.18.0/bin/node


// eslint-disable-next-line no-unused-vars
// 1
const getNewObjWithPrototype = (obj) => {
    return Object.create(obj);
}

// 2
const getEmptyObj = () => {
    return Object.create(null);
}

// 3
const setPrototypeChain = ({ programmer, student, teacher, person }) => {
    Object.setPrototypeOf(teacher, person);
    Object.setPrototypeOf(student, teacher)
    Object.setPrototypeOf(programmer, student)
    return Object.create(programmer);
}

// 4
const getObjWithEnumerableProperty = () => {
    return Object.create(null, {
        name : {value : 'Alex'},
        age : {value : 18, enumerable: true},
        work : {value : 'empty'}
    })
}

// 5
const getWelcomeObject = (person) => {
    let out = Object.create(person)
    out.voice = function() {
        return `Hello, my name is ${Object.getPrototypeOf(out).name}. I am ${Object.getPrototypeOf(out).age}.`;
    };
    return out
}

// 6
class Singleton {
    static num = null
    static instance = null;
    constructor(id) {
        if (Singleton.num == null) {
            Singleton.instance = this
            Singleton.num = id
        }
        return Singleton.instance
    }

    toString() {
        return Singleton.num;
    }
}

// 7
const defineTimes = () => {
    Number.prototype.times = function(callback) {
        let array = [];
        for (let i = 0; i < this.valueOf(); i++) {  
            array.push(callback(i, this.valueOf()));  
        }
        return array;
    };
};

// 8
const defineUniq = () => {
    Object.defineProperty(Array.prototype, 'uniq', {
        get() {
            let localArray = [];
            this.forEach((curElement) => {
                if (!localArray.includes(curElement)) {
                    localArray.push(curElement); 
                }
            });
            return localArray;
        },
        enumerable: false
    });
};

// 9
const defineUniqSelf = () => {
    defineUniq()
    Object.defineProperty(Array.prototype, 'uniqSelf', {
        get() {
            let uniq_array = this.uniq
            this.length = 0
            uniq_array.forEach(val => this.push(val));  
            return this
        },
        enumerable: false
    });
}

module.exports = {
    getNewObjWithPrototype,
    getEmptyObj,
    setPrototypeChain,
    getObjWithEnumerableProperty,
    getWelcomeObject,
    Singleton,
    defineTimes,
    defineUniq,
    defineUniqSelf,
}



// function main() {
//     defineUniqSelf();
//     const arr = [1,2,2];
    
//     console.log(arr.uniqSelf); // [1,2];
//     console.log(arr); // [1,2];
// }


// main()