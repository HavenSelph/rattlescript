![Tests](https://github.com/HavenSelph/rattlescript/actions/workflows/build.yml/badge.svg)

# RattleScript is...
An interpreted, dynamic, and expressive programming language written in Rust! It started as a small development project with a friend of mine, but has quickly turned into a passion! I hope to continue working on this project and one day host a whole standard library of functions and modules for people to use. Every week the feature list grows, and I'm excited to see where this project ends up!

RattleScript is licensed under the [MIT](https://github.com/HavenSelph/rattlescript/blob/main/LICENSE.md) license, learn more about what it permits [here](https://choosealicense.com/licenses/mit/).

# Contributing
Want to contribute? That's awesome! Go ahead and fork the repository and make a pull request! I'll be sure to review it as soon as I can. However, please make sure to make sure your contribution...
- is NOT a formatting change. I will not accept changes that are unsubstantial.
- either fixes, improves, or adds a feature to the language.
- does not break any existing features. GitHub Actions will automatically run tests on your code, but please make sure to test your code yourself before submitting a pull request.
- does not add any new dependencies. I understand that you may want to add a feature, but don't want to spend time reinventing the wheel. However, I want to keep this project independent of any external crates.
- is not changing more than 1 feature at once. I want to keep pull requests on the small side and easy to review.

If your change is large or requires a rewrite of many parts, please make sure to open an issue first, so we can discuss it. I would hate to waste your time for me to not merge it in because of too many conflicts. Furthermore, please be sure to either document your code, or describe what it does in the pull request. If your change is large enough to permit it, please consider adding tests for common issues you had to prevent regression. I hate fixing things I've already fixed before, or that someone else has fixed before. The testing framework is very simple, and you can find examples of it in the `./tests` directory. The script that handles testing is `./test.py`.

# Building
Since there are no dependencies, you need nothing more than the Rust compiler itself. Just run the following command:
`cargo build --release`

# Using
RattleScript has two main ways of being run. You can either open a REPL or run a single file. To open a REPL, simply pass no arguments to the executable. To run a file, pass the path to the file as an argument. For example, to run the file `./test.rat` you would pass the argument `./test.rat` to the interpreter. For more information, run the interpreter with the `--help` or `-h` flag.

# Planned Language Features
- Import system
- Sets
- Match statements
- Switch statements (like match, but don't stop on first match)
- Loop returns
- Enums
- Structs (maybe)
- Better error messages (hints, possible solutions, pointing to the exact location of the problem instead of the statement)

# Planned Backend Features
- More file related functions
- Better system for builtins

# Features
### Variables and expressions
```javascript
let x = 10
let y = 2
let z = x + y // Will evaluate to 12
```
### Functions and Lambdas
```javascript
def add(a, b) {  
    return a + b
}

let multiply = |a, b| => a * b
```
### Python-like Variadics and Defaults
```javascript
def add(a, *b, c: 10, **d) => (a, b, c, d)

add(1,2,3,c:10,k:12) // Will evaluate to (1, [2,3], 10, {k:12})
```
### Comprensions
```javascript
let a = [1, 2, 3, 4, 5]
[print(x) for x in a]  // Prints each element in the list
[print(x) for x in a if x % 2 == 0]  // Prints each even element in the list
```
### Closure Scoping and Decorators
```js
def deco(msg) {
    def inner(func) {
        def wrapper(*args, **kwargs) {
            print(msg, "input: ", args, kwargs)
            return func(*args, **kwargs)        
        }
    }
}

@deco("addition")
def add(a, b) {  // We can decorate functions with python's @ syntax
    return a + b
}

def sub(a, b) {
    return a - b
}

sub = deco("subtraction")(sub) // Or we can do it the old fashioned way


add(3, 4) // Will print "addition input 3 4"
sub(5, 6) // Will print "subtraction input 5 6"
```
### Classes and Inheritance
```javascript
class ClassA {
    def new(self, name) {
        self.name = name
    }
    
    def print_name(self) {
        print(self.name)
    }
}

class ClassB(ClassA) {}  // ClassB inherits from ClassA
// This means that ClassB will have all of ClassA's methods and (static) variables
// You can also inherit from more than one class at a time. 

let a = ClassB
class ClassC(a) // This is valid too!
```
### Namespaces
```javascript
namespace Math {
    def add(a, b) => a+b
    
    def sub(a, b) => a-b
    
    let pi = 3.141592653589793
}

Math.add(1, 2)
Math.pi 
```

### Datatypes
```javascript
let a = 10  // Integers
let b = 10.0  // Floats
let c = 0b101  // Binary
let d = 0o67  // Octal
let e = 0x22B  // Hexadecimal
let f = "Hello World!"  // Strings
let g = true  // Booleans
let h = [1, 2, 3]  // Lists
let i = {a: 1, b: 2, c: 3}  // Dictionaries
let j = (1, 2, 3)  // Tuples
let k = nothing  // Nothing (equivalent to null or None)

// Since rattlescript is dynamic, everything is an object! You can
// call methods on most datatypes!

[1,2,3,4].iter().map(print)  // This will print each element in the list
"123".int()  // This will convert the string to an integer
123.str()  // This will convert the integer to a string


def add(a, b) {
    return a + b
}

let add_var = add  // Functions are objects too!

// Even ranges are objects!
let range = 0..10
for x in range {
    print(x)
}
```
### Control Flow
```javascript
let boo = true
let two = false

if boo {
    // Do something
} else if two {
    // Do something
}

while boo {
    // Do something
    if two {
        break
    } else {
        continue
    }
}
```
