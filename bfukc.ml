open Core.Std

let tape_size = 131072

let run interpret file () =
  let runner = if interpret then Interpreter.run else Generator.run in
  let () = In_channel.with_file file ~f:Compiler.read
    |> Compiler.optimize
    |> runner tape_size in
  Out_channel.flush stdout

let command =
  Command.basic
    ~summary:"Optimizing BrainFuck compiler"
    Command.Spec.(
      empty
      +> flag "-b" no_arg ~doc:" compile and run immediately using a bytecode interpreter"
      +> anon ("filename" %: string)
      )
    run

let () =
  Command.run ~version:"1.0" ~build_info:"Atte Kojo <atte.kojo@gmail.com>" command
