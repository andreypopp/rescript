# rescript

Rescript is a scripting runtime for ReasonML.

Rescript allows to write executable programs in ReasonML, specify external
dependencies, then have it seamlessly compiled to efficient native code.

## Installation


## Usage

Create file `lib.ml`:

```
let hello name = "Hello, " ^ name ^ "!"
```

And another file `hello.ml`:

```
module Lib = [%import "./lib.ml"]

let () = print_endline (Lib.hello "World")
```

Now you can run it with:

```
% rescript ./hello.ml
```

Which will compile dependencies and run it.

### Development

```
% esy build
% esy x rescript ./example/hello.ml
```

## TODO

- Cache compiled executables / object files
- Process `*.re` with `refmt`
- Figure out how to integrate with Merlin
- Release as esy release
- Support running via shebang line
- Fetch remote URLs `[%import "https://gist..."]`
- Fetch esy packages from npm / opam
