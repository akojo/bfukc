open Core.Std

module Program = struct
  type op = Add of int | Move of int | In | Out | Open | Close
end

module Bytecode = struct
  type t = Halt | Add of int | Move of int | In | Out | Jeqz of int | Jnez of int
end

let read_program chan =
  let instructions = Program.(Char.Map.of_alist_exn [
    ('+', Add 1); ('-', Add (-1)); ('>', Move 1); ('<', Move (-1));
    (',', In); ('.', Out); ('[', Open); (']', Close)])
  in
  let rec read_loop is =
    match In_channel.input_char chan with
    | None -> List.rev is
    | Some c -> translate c is
  and translate ch is =
    match Map.find instructions ch with
    | None -> read_loop is
    | Some i -> read_loop (i :: is)
  in
  read_loop []

let compile program =
  let open Bytecode in
  let code = Array.create ~len:(List.length program + 1) Halt in
  let pop = function
    | it :: rest -> it, rest
    | [] -> failwith "Unmatched ']'"
  in
  let compile_ins i stack = function
    | Program.Add n -> code.(i) <- Add n; stack
    | Program.Move n -> code.(i) <- Move n; stack
    | Program.In -> code.(i) <- In; stack
    | Program.Out -> code.(i) <- Out; stack
    | Program.Open -> code.(i) <- Jeqz 0; i :: stack
    | Program.Close ->
      let idx, stack = pop stack in
      code.(idx) <- Jeqz (i + 1); code.(i) <- Jnez idx; stack
  in
  let rec loop i stack instrs =
    match instrs with
    | [] ->
      if List.length stack = 0 then code
      else failwith "Unmatched '['"
    | ins :: rest -> loop (i + 1) (compile_ins i stack ins) rest
  in
  loop 0 [] program

let optimize program =
  let open Program in
  let rec loop program optimized =
    match program with
    | Add x :: Add y :: rest -> loop (Add (x + y) :: rest) optimized
    | Move x :: Move y :: rest -> loop (Move (x + y) :: rest) optimized
    | ins :: rest -> loop rest (ins :: optimized)
    | [] -> List.rev optimized
  in
  loop program []

let run buf program =
  let add x y =
    let sum = (x + y) mod 256 in
    if sum < 0 then 256 + sum
    else sum
  in
  let rec loop pc i =
    let open Bytecode in
    match program.(pc) with
    | Halt -> ()
    | Add n -> buf.(i) <- add buf.(i) n; loop (pc + 1) i
    | Move n -> loop (pc + 1) (i + n)
    | In -> read pc i
    | Out -> Out_channel.output_char stdout (Char.of_int_exn buf.(i)); loop (pc + 1) i
    | Jeqz n -> if buf.(i) = 0 then loop n i  else loop (pc + 1) i
    | Jnez n -> if buf.(i) <> 0 then loop n i  else loop (pc + 1) i
  and read pc i =
    match In_channel.input_char stdin with
    | None -> ()
    | Some ch -> buf.(i) <- Char.to_int ch; loop (pc + 1) i
  in
  loop 0 0

let () =
  let () = read_program stdin |> optimize |> compile |> run (Array.create ~len:131072 0) in
  Out_channel.flush stdout
