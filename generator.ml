open Core.Std

let l = ref 0

let alloc_label () =
  let label = "L" ^ (Int.to_string !l) in
  l := !l + 1; label

let r = ref 0

let alloc_reg () =
  let register = "%r" ^ (Int.to_string !r) in
  r := !r + 1; register

let emit_header tape_size =
  printf "@tape = internal global [%d x i8] zeroinitializer\n\n" tape_size;
  print_endline "define i32 @main() #0 {";
  print_endline "  %zero = alloca i8";
  print_endline "  store i8 0, i8* %zero";
  print_endline "  %p = alloca i8*";
  printf "  store i8* getelementptr inbounds ([%d x i8], [%d x i8]* @tape, i32 0, i32 0), i8** %%p" tape_size tape_size;
  print_endline ""

let emit_add n =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  let r3 = alloc_reg () in
  printf "  %s = load i8*, i8** %%p\n" r1;
  printf "  %s = load i8, i8* %s\n" r2 r1;
  printf "  %s = add i8 %s, %d\n" r3 r2 n;
  printf "  store i8 %s, i8* %s\n\n" r3 r1

let emit_move n =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  printf "  %s = load i8*, i8** %%p\n" r1;
  printf "  %s = getelementptr inbounds i8, i8* %s, i64 %d\n" r2 r1 n;
  printf "  store i8* %s, i8** %%p\n\n" r2

let emit_in () = ()

let emit_out () =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  let r3 = alloc_reg () in
  let r4 = alloc_reg () in
  printf "  %s = load i8*, i8** %%p\n" r1;
  printf "  %s = load i8, i8* %s\n" r2 r1;
  printf "  %s = zext i8 %s to i32\n" r3 r2;
  printf "  %s = call i32 @putchar(i32 %s)\n\n" r4 r3

let emit_open l_open l_close =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  let r3 = alloc_reg () in
  let r4 = alloc_reg () in
  let l_true = alloc_label () in
  printf "  br label %%%s\n" l_open;
  printf "%s:\n" l_open;
  printf "  %s = load i8*, i8** %%p\n" r1;
  printf "  %s = load i8, i8* %s\n" r2 r1;
  printf "  %s = zext i8 %s to i32\n" r3 r2;
  printf "  %s = icmp eq i32 %s, 0\n" r4 r3;
  printf "  br i1 %s, label %%%s, label %%%s\n" r4 l_close l_true;
  printf "%s:\n\n" l_true

let emit_close l_open l_close =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  let r3 = alloc_reg () in
  let r4 = alloc_reg () in
  printf "  %s = load i8*, i8** %%p\n" r1;
  printf "  %s = load i8, i8* %s\n" r2 r1;
  printf "  %s = zext i8 %s to i32\n" r3 r2;
  printf "  %s = icmp ne i32 %s, 0\n" r4 r3;
  printf "  br i1 %s, label %%%s, label %%%s\n" r4 l_open l_close;
  printf "%s:\n\n" l_close

let emit_clear () =
  let r = alloc_reg () in
  printf "  %s = load i8*, i8** %%p\n" r;
  printf "  store i8 0, i8* %s\n\n" r

let emit_mul x y =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  let r3 = alloc_reg () in
  let r4 = alloc_reg () in
  let r5 = alloc_reg () in
  let r6 = alloc_reg () in
  let r7 = alloc_reg () in
  printf "  %s = load i8*, i8** %%p\n" r1;
  printf "  %s = load i8, i8* %s\n" r2 r1;
  printf "  %s = mul i8 %s, %d\n" r3 r2 y;
  printf "  %s = load i8*, i8** %%p\n" r4;
  printf "  %s = getelementptr inbounds i8, i8* %s, i64 %d\n" r5 r4 x;
  printf "  %s = load i8, i8* %s\n" r6 r5;
  printf "  %s = add i8 %s, %s\n" r7 r6 r3;
  printf "  store i8 %s, i8* %s\n" r7 r5

let emit_footer () =
  let r1 = alloc_reg () in
  let r2 = alloc_reg () in
  printf "  %s = load i8, i8* %%zero\n" r1;
  printf "  %s = zext i8 %s to i32\n" r2 r1;
  printf "  ret i32 %s\n" r2;
  print_endline "}";
  print_endline "";
  print_endline "declare i32 @getchar() #1";
  print_endline "declare i32 @putchar(i32) #2"

let compile tape_size program =
  let pop = function
    | top :: rest -> top, rest
    | [] -> failwith "Unmatched ']'"
  in
  let compile_ins stack = function
    | Compiler.Add n -> emit_add n; stack
    | Compiler.Move n -> emit_move n; stack
    | Compiler.In -> emit_in (); stack
    | Compiler.Out -> emit_out (); stack
    | Compiler.Open ->
        let l_open = alloc_label () in
        let l_close = alloc_label () in
        emit_open l_open l_close; (l_open, l_close) :: stack
    | Compiler.Close ->
        let (l_open, l_close), stack = pop stack in
        emit_close l_open l_close; stack
    | Compiler.Clear -> emit_clear (); stack
    | Compiler.Mul (x, y) -> emit_mul x y; stack
  in
  let rec loop stack instrs =
    match instrs with
    | [] ->
      if List.length stack = 0 then ()
      else failwith "Unmatched '['"
    | ins :: rest -> loop (compile_ins stack ins) rest
  in
  emit_header tape_size; loop [] program; emit_footer ()

let run tape_size program = compile tape_size program
