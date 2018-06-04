open Ppxlib

let name = "import"

let makeId spec =
  let len = String.length spec in

  let rec aux acc idx =
    if idx = len
    then acc |> List.rev |> String.concat ""
    else
      let chunk = match String.get spec idx with
      | '.' -> "S_DT_"
      | '/' -> "S_SL_"
      | '@' -> "S_AT_"
      | c -> String.make 1 c
      in
      aux (chunk::acc) (idx + 1)
  in
  aux [] 0

let expand ~onImport ~loc ~path:_ (spec : string) =
  let id, ident =
    let id = makeId spec in
    let ident =
      id
      |> Longident.parse
      |> Loc.make ~loc
    in id, ident
  in
  let () = onImport spec id in
  Ast_builder.Default.pmod_ident ~loc ident

let register ~onImport () =
  let ext =
    Extension.declare
      name
      Extension.Context.module_expr
      Ast_pattern.(single_expr_payload (estring __))
      (expand ~onImport)
  in
  Ppxlib.Driver.register_transformation name ~extensions:[ext]
