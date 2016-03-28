open Core.Std

let () =
  let () = Compiler.read stdin
    |> Compiler.optimize
    |> Bytecode.compile
    |> Bytecode.run (Array.create ~len:131072 0) in
  Out_channel.flush stdout
