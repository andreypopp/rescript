let inputLines ic =
  let rec aux lines =
    try
      let line = input_line ic in
      aux (line::lines)
    with End_of_file -> lines
  in
  aux []
  |> List.rev
  |> String.concat "\n"

let readFile filename =
  let ic = open_in filename in
  let data = inputLines ic in
  close_in ic;
  data

let parseScript filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexer.skip_hash_bang lexbuf;
  Ppxlib.Parse.implementation lexbuf

let makeId = Rescript_ppx_import.makeId


module Process = struct
  let buildCmd args =
    args
    |> List.map Filename.quote
    |> String.concat " "

  let run args =
    let cmd = buildCmd args in
    let oc = Unix.open_process_out cmd in
    match Unix.close_process_out oc with
    | Unix.WEXITED 0 -> ()
    | _ -> exit 1

  let runOut args =
    let cmd = buildCmd args in
    let ic = Unix.open_process_in cmd in
    let r = inputLines ic in
    close_in ic;
    String.trim r
end

module Fs = struct

  let copyFile ~src ~dst () =
    let open Unix in
    let buffer_size = 8192 in
    let buffer = Bytes.create buffer_size in

    let fdIn = openfile src [O_RDONLY] 0 in
    let fdOut = openfile dst [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
    let rec loop () = match read fdIn buffer 0 buffer_size with
      |  0 -> ()
      | r -> ignore (write fdOut buffer 0 r); loop ()
    in
    loop ();
    close fdIn;
    close fdOut

  let mkdirp path =
    Process.run ["mkdir"; "-p"; path]

  let realpath path =
    Process.runOut ["realpath"; path]

  let homedir () =
    let uid = Unix.getuid () in
    let home = (Unix.getpwuid uid).Unix.pw_dir in
    home

end

module Module = struct

  type t = {
    id : string;
    src : src;
  }

  and src =
    | Remote of string
    | Local of string


  let imports = ref []
  let () =
    let onImport src id =
      let src = match String.get src 0 with
      | '.' -> Local src
      | '/' -> Local src
      | _ -> Remote src
      in
      imports := {id; src}::!imports
    in
    Rescript_ppx_import.register ~onImport ()

  let ofFile filename =
    imports := [];
    let structure = parseScript filename in
    let _ = Ppxlib.Driver.map_structure structure in
    let imports =
      let f imp =
        match imp.src with
        | Local path ->
          let dirname = Filename.dirname filename in
          let path = Fs.realpath (dirname ^ "/" ^ path) in
          {imp with src = Local path}
        | Remote _ -> imp
      in
      !imports
      |> List.map f
    in imports

end

module Ocamlopt = struct

  let ocamlopt = "ocamlopt"
  let ppx = "rescript-ppx"

  let withCwd ~cwd f =
    let prevCwd = Sys.getcwd () in
    Sys.chdir cwd;
    let r = f () in
    Sys.chdir prevCwd;
    r

  let makeObj ~buildDir ~filename () =
    withCwd ~cwd:buildDir (fun () ->
      Process.run [ocamlopt; "-ppx"; ppx ^ " -as-ppx"; "-c"; filename]
    )

  let makeExecutable ~buildDir ~objects ~out () =
    withCwd ~cwd:buildDir (fun () ->
      let cmd = [ocamlopt] @ objects @ ["-o"; out] in
      Process.run cmd
    )

end

module Build = struct

  module StringSet = Set.Make(String)

  let libRoot =
    let path = (Fs.homedir ()) ^ "/.rescript/lib" in
    (match Unix.stat path with
    | exception _ -> Fs.mkdirp path
    | _ -> ());
    path

  let libPath filename ext = libRoot ^ "/" ^ filename ^ "." ^ ext

  let needRebuild ~src ~out () =
    let srcStat = Unix.stat src in
    match Unix.stat out with
    | exception _ -> true
    | outStat -> outStat.Unix.st_mtime < srcStat.Unix.st_mtime

  let build filename =
    let rec aux ~seen ~objects queue =
      match queue with
      | Module.{id; src}::queue ->
        if StringSet.mem id seen
        then aux ~seen ~objects queue
        else
          begin match src with
          | Module.Local filename ->

            let libFilename = libPath id "ml" in
            let objFilename = libPath id "cmx" in

            let imports = Module.ofFile filename in
            let queue = imports @ queue in
            let seen = StringSet.add id seen in
            let objects = objFilename::objects in

            let objects = aux ~seen ~objects queue in

            let () =
              if needRebuild ~src:filename ~out:objFilename ()
              then (
                Fs.copyFile ~src:filename ~dst:libFilename ();
                Ocamlopt.makeObj ~buildDir:libRoot ~filename:libFilename ()
              ) else ()
            in

            objects
          | Module.Remote _url ->
            failwith "not implemented"
          end
      | [] -> objects
    in
    let root = Module.{
      id = makeId filename;
      src = Local filename
    } in

    let execFilename = libPath root.id "exe" in

    if needRebuild ~src:filename ~out:execFilename ()
    then
      let objects = aux ~seen:StringSet.empty ~objects:[] [root] in
      Ocamlopt.makeExecutable
        ~buildDir:libRoot
        ~objects
        ~out:(Filename.basename execFilename)
        ()
    else
      ();

    execFilename

end

let () =
  let filename = Sys.argv.(1) in
  let execFilename = Build.build filename in
  Unix.execv execFilename [||]
