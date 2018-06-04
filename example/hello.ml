module Lib = [%import "./lib.ml"]

let () =
  let msg = Lib.hello "World" in
  print_endline msg
