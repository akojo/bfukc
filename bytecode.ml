open Core.Std

type t =
  | Halt | Add of int | Move of int | In | Out | Jeqz of int | Jnez of int
  | Clear | Mul of int * int

let compile program =
  let code = Array.create ~len:(List.length program + 1) Halt in
  let pop = function
    | it :: rest -> it, rest
    | [] -> failwith "Unmatched ']'"
  in
  let compile_ins i stack = function
    | Compiler.Add n -> code.(i) <- Add n; stack
    | Compiler.Move n -> code.(i) <- Move n; stack
    | Compiler.In -> code.(i) <- In; stack
    | Compiler.Out -> code.(i) <- Out; stack
    | Compiler.Open -> code.(i) <- Jeqz 0; i :: stack
    | Compiler.Close ->
      let idx, stack = pop stack in
      code.(idx) <- Jeqz (i + 1); code.(i) <- Jnez idx; stack
    | Compiler.Clear -> code.(i) <- Clear; stack
    | Compiler.Mul (x, y) -> code.(i) <- Mul (x, y); stack
  in
  let rec loop i stack instrs =
    match instrs with
    | [] ->
      if List.length stack = 0 then code
      else failwith "Unmatched '['"
    | ins :: rest -> loop (i + 1) (compile_ins i stack ins) rest
  in
  loop 0 [] program

let run buf program =
  let rec loop pc i =
    match program.(pc) with
    | Halt -> ()
    | Add n -> buf.(i) <- (buf.(i) + n) land 0xff; loop (pc + 1) i
    | Move n -> loop (pc + 1) (i + n)
    | In -> read pc i
    | Out -> Out_channel.output_char stdout (Char.of_int_exn buf.(i)); loop (pc + 1) i
    | Jeqz n -> if buf.(i) = 0 then loop n i else loop (pc + 1) i
    | Jnez n -> if buf.(i) <> 0 then loop n i else loop (pc + 1) i
    | Clear -> buf.(i) <- 0; loop (pc + 1) i
    | Mul (x, y) -> buf.(i + x) <- (buf.(i + x) + buf.(i) * y) land 0xff; loop (pc + 1) i
  and read pc i =
    match In_channel.input_char stdin with
    | None -> ()
    | Some ch -> buf.(i) <- Char.to_int ch; loop (pc + 1) i
  in
  loop 0 0

