## Added
- wildcard imports `from module import *`
- dump function `dump()`
- in operator `x in y`
- short-circuiting `and` and `or` operators
- if statement bodies can now be an expression or block
- example folder and Caluc example program
- default argument for `dict.get()` function
- array std lib module
    - `any` `all` `max` `sum` `min`
- collection std lib module
    - `Counter` `OrderedDict`
- string std lib module
    - `is_alpha` `is_digit` `is_space` `is_alnum`
## Changed
- class instances now hold a reference to their main parent class
- class instances can no longer create new fields outside of `new()`
- `==` operator can now compare a class to a class instance and will return `true` if the class instance is an instance of the class
    - It is worth noting that this is likely temporary and will be changed to a more explicit method in the future. 

## Removed
- json stdlib module
