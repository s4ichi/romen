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
`obj(a)` represents object that aliased to variable `a`.


```
{
    // obj(g) = NULL
    let g;

    fn pow(a, b) {
        // obj(a), obj(b) -> allocate stack
        // obj(z) -> allocate heap
        let z = a ** b
        return z

        // free obj(a), obj(b)
    }

    {
        // obj(x) -> allocate stack
        // obj(y) -> allocate heap
        let x = 10
        let y = 5

        // copy alias from y to g
        g = &y

        {
            // obj(r) -> in heap
            let r = pow(x, y)
        }

        // free obj(x)
    }
}

// garbage collect obj(y) = obj(g), obj(r) at later
```

## Build

Clone repository and run make to build:

```
make
```

## Run

Current version able to traverse AST and binding check.

```sh
./bin/romen -v -e 'let x = 3; { fn hoge(a){ let k = a; }; let y = 2; let z = x + y; let g = 3; }'
```

## LICENSE
MIT

## Referrences
wip
