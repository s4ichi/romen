# Memory safety language

Specific implementation of memory safety language that is temporarily called 'Romen'.
Romen is lightweight object oriented language, and has static memory inference module and tiny garbage collector.

## Review ideas

- Region based memory management
- smart pointer
- Substructual type system
- and more

## Syntax[WIP]

Syntax is still under consideration.

```
fn pow(a, b) {
    // a, b -> allocate stack
    // z -> allocate heap
    let z = a ** b
    return z

    // free a, b
}

{
    // x, y -> allocate stack
    let x = 10
    let y = 5

    {
        // pow(x, y) -> in heap
        let r = pow(x, y)
    }

    // garbage collect `pow(x, y)` later
    // free x, y
}
```

## Build

Clone repository and run make to build:

```
make
```

## LICENSE
MIT

## Referrences
wip
