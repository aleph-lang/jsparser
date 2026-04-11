# js_parser

Parses JavaScript source code into an [`AlephTree`](https://github.com/aleph-lang/aleph-syntax-tree).
Built on top of [rslint_parser](https://github.com/rslint/rslint).

## Installation

```toml
[dependencies]
js_parser = "0.1"
```

## Usage

```rust
let ast = js_parser::parse(source_code);
```

## Example

Input:

```js
function add(a, b) {
    return a + b;
}
```

Produces an `AlephTree::LetRec` with two arguments and an `Add` body.

## Related

- [`aleph-syntax-tree`](https://github.com/aleph-lang/aleph-syntax-tree) — AST definition
- [`alephc`](https://github.com/aleph-lang/aleph) — uses this parser with `--features js_parse`
