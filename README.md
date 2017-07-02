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
{
    let x = 10 # allocate in stack
    let z = null # allocate in stack

    {
        let y = 5 # allocate in heap ( collect by GC )
        z = y

        x = x + y # valid
    }

    x = x + y # invalid
    x = x + z # valid
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
