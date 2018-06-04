let () =
  let () =
    Rescript_ppx_import.register
      ~onImport:(fun _ _ -> ())
      ()
  in
  Ppxlib.Driver.standalone ()
