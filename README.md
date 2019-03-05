# Romen

Example implementation of lightweight object oriented language that determines object lifetime statically.

## Review ideas (for memory management)

- Region based memory management
- Region analysis
- Garbage collection (when the region of object can't be analyzed)

## Syntax (sample)

```
{
      fn collatz(a) {
          if a % 2 == 0
          then a / 2
          else a * 3 + 1
      }

      n = 100

      while(n != 1) {
          n = collatz(n)
      }

      n
}
```

## Inference module

The inference algorithm implemented by OCaml is in the `/infer/main.ml`.
You can test the inference algorithm as interpreter.

```
cd infer && make && ./infer
```

## Getting started

Note: Current version able to only traverse AST and binding check.

```sh
make && ./bin/romen -v -e 'let x = 3; { fn hoge(a){ let k = a; }; let y = 2; let z = x + y; let g = 3; }'
```

## LICENSE
MIT
