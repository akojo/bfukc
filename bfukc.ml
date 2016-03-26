open Core.Std

module Program = struct
  type op =
    | Add of int | Move of int | In | Out | Open | Close
    | Clear | Mul of int * int
end

module Bytecode = struct
  type t =
    | Halt | Add of int | Move of int | In | Out | Jeqz of int | Jnez of int
    | Clear | Mul of int * int
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
    | Program.Clear -> code.(i) <- Clear; stack
    | Program.Mul (x, y) -> code.(i) <- Mul (x, y); stack
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
  let rec contract optimized program =
    match program with
    | Add x :: Add y :: rest -> contract optimized (Add (x + y) :: rest)
    | Move x :: Move y :: rest -> contract optimized (Move (x + y) :: rest)
    | ins :: rest -> contract (ins :: optimized) rest
    | [] -> List.rev optimized
  in
  let rec compact_loops optimized program =
    match program with
    | Open :: Add x :: Close :: rest when x = -1 || x = 1 ->
        compact_loops (Clear :: optimized) rest
    | Open :: Add n0 :: Move p1 :: Add n1 :: Move p2 :: Close :: rest when (n0 = 1 || n0 = -1) && p1 = -p2 ->
        compact_loops (Clear :: Mul (p1, n1) :: optimized) rest
    | Open :: Add n0 :: Move p1 :: Add n1 :: Move p2 :: Add n2 :: Move p3 :: Close :: rest when
      (n0 = 1 || n0 = -1) && p1 + p2 = -p3 ->
        compact_loops (Clear :: Mul (p1 + p2, n2) :: Mul (p1, n1) :: optimized) rest
    | ins :: rest -> compact_loops (ins :: optimized) rest
    | [] -> List.rev optimized
  in
  program |> contract [] |> compact_loops []

let run buf program =
  let rec loop pc i =
    let open Bytecode in
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

let () =
  let () = read_program stdin |> optimize |> compile |> run (Array.create ~len:131072 0) in
  Out_channel.flush stdout
