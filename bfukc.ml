open Core.Std

let () =
  let () = Compiler.read stdin
    |> Compiler.optimize
    |> Generator.run 131072 in
  Out_channel.flush stdout
