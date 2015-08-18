A port of the Scrap Your Boilerplate library to OCaml.

### Installation

```
opam switch 4.02.0+modular-implicits
opam pin add syb https://github.com/yallop/ocaml-syb.git
```

### Usage

The following examples assume that you have loaded the package and brought the instances into scope:

```ocaml
# #require "syb";;
# open Syb.Instances;;
```

Apply `succ` to every `int` within a larger structure:

```ocaml
# Syb.(everywhere (mkT succ)) [1;2;3];;
- : int list = [2; 3; 4]
```

Apply `not` to every `bool` within a larger structure:

```ocaml
# Syb.(everywhere (mkT not)) [[true], 1; [], 2; [true; false], 3];;
- : (bool list * int) list = [([false], 1); ([], 2); ([false; true], 3)]
```

Collect all `int` values less than `3`: 

```ocaml
# Syb.listify (fun x -> x < 3) [[(4, true)]; [(-2, false); (1, false)]; []; [0, true]];;
- : int list = [-2; 1; 0]
```
