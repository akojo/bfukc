open Core.Std

let run file () =
  let () = In_channel.with_file file ~f:Compiler.read
    |> Compiler.optimize
    |> Generator.run 131072 in
  Out_channel.flush stdout

let command =
  Command.basic
    ~summary:"Optimizing BrainFuck compiler"
    Command.Spec.(empty +> anon ("filename" %: string))
    run

let () =
  Command.run ~version:"1.0" ~build_info:"Atte Kojo <atte.kojo@gmail.com>" command
