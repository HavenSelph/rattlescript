# v0.3.0 - Errors, Try/Catch, and Built-In Classes
___
New error handling system will use control flow to propagate errors, this will allow me to create a try/except AST node which stores a block of instructions. When an instruction results in an error, it will propagate this error upwards, allowing the first try/except block to catch it. If no try/except block catches the error, the program will handle errors as it does now, by printing the error and exiting. 
```js
try {
    print(true + 1)
} catch (Error.InvalidType) as e {
    print(e)
}
```

## Added
- Enums
