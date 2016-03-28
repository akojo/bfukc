open Core.Std

type op =
  | Add of int | Move of int | In | Out | Open | Close
  | Clear | Mul of int * int

let read chan =
  let instructions = Char.Map.of_alist_exn [
    ('+', Add 1); ('-', Add (-1)); ('>', Move 1); ('<', Move (-1));
    (',', In); ('.', Out); ('[', Open); (']', Close)]
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

let optimize program =
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

