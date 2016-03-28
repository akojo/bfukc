open Core.Std
open Core_extended.Std

let l = ref 0

let alloc_label () =
  let label = "L" ^ (Int.to_string !l) in
  l := !l + 1; label

let r = ref 0

let alloc_reg () =
  let register = "%r" ^ (Int.to_string !r) in
  r := !r + 1; register

let compile ch tape_size program =
  let emit_header tape_size =
    fprintf ch "@tape = internal global [%d x i8] zeroinitializer\n\n" tape_size;
    fprintf ch "define i32 @main() #0 {\n";
    fprintf ch "  %%p = alloca i8*\n";
    fprintf ch "  store i8* getelementptr inbounds ([%d x i8], [%d x i8]* @tape, i32 0, i32 0), i8** %%p\n\n" tape_size tape_size;
  in
  let emit_add n =
    let r1 = alloc_reg () in
    let r2 = alloc_reg () in
    let r3 = alloc_reg () in
    fprintf ch "  %s = load i8*, i8** %%p\n" r1;
    fprintf ch "  %s = load i8, i8* %s\n" r2 r1;
    fprintf ch "  %s = add i8 %s, %d\n" r3 r2 n;
    fprintf ch "  store i8 %s, i8* %s\n\n" r3 r1
  in
  let emit_move n =
    let r1 = alloc_reg () in
    let r2 = alloc_reg () in
    fprintf ch "  %s = load i8*, i8** %%p\n" r1;
    fprintf ch "  %s = getelementptr inbounds i8, i8* %s, i64 %d\n" r2 r1 n;
    fprintf ch "  store i8* %s, i8** %%p\n\n" r2
  in
  let emit_in () = ()
  in
  let emit_out () =
    let r1 = alloc_reg () in
    let r2 = alloc_reg () in
    let r3 = alloc_reg () in
    let r4 = alloc_reg () in
    fprintf ch "  %s = load i8*, i8** %%p\n" r1;
    fprintf ch "  %s = load i8, i8* %s\n" r2 r1;
    fprintf ch "  %s = zext i8 %s to i32\n" r3 r2;
    fprintf ch "  %s = call i32 @putchar(i32 %s)\n\n" r4 r3
  in
  let emit_open l_open l_close =
    let r1 = alloc_reg () in
    let r2 = alloc_reg () in
    let r3 = alloc_reg () in
    let l_false = alloc_label () in
    fprintf ch "  br label %%%s\n" l_open;
    fprintf ch "%s:\n" l_open;
    fprintf ch "  %s = load i8*, i8** %%p\n" r1;
    fprintf ch "  %s = load i8, i8* %s\n" r2 r1;
    fprintf ch "  %s = icmp eq i8 %s, 0\n" r3 r2;
    fprintf ch "  br i1 %s, label %%%s, label %%%s\n" r3 l_close l_false;
    fprintf ch "%s:\n\n" l_false
  in
  let emit_close l_open l_close =
    fprintf ch "  br label %%%s\n" l_open;
    fprintf ch "%s:\n\n" l_close
  in
  let emit_clear () =
    let r = alloc_reg () in
    fprintf ch "  %s = load i8*, i8** %%p\n" r;
    fprintf ch "  store i8 0, i8* %s\n\n" r
  in
  let emit_mul x y =
    let r1 = alloc_reg () in
    let r2 = alloc_reg () in
    let r3 = alloc_reg () in
    let r4 = alloc_reg () in
    let r5 = alloc_reg () in
    let r6 = alloc_reg () in
    let r7 = alloc_reg () in
    fprintf ch "  %s = load i8*, i8** %%p\n" r1;
    fprintf ch "  %s = load i8, i8* %s\n" r2 r1;
    fprintf ch "  %s = mul i8 %s, %d\n" r3 r2 y;
    fprintf ch "  %s = load i8*, i8** %%p\n" r4;
    fprintf ch "  %s = getelementptr inbounds i8, i8* %s, i64 %d\n" r5 r4 x;
    fprintf ch "  %s = load i8, i8* %s\n" r6 r5;
    fprintf ch "  %s = add i8 %s, %s\n" r7 r6 r3;
    fprintf ch "  store i8 %s, i8* %s\n\n" r7 r5
  in
  let emit_footer () =
    fprintf ch "  ret i32 0\n";
    fprintf ch "}\n\n";
    fprintf ch "declare i32 @getchar() #1\n";
    fprintf ch "declare i32 @putchar(i32) #2\n"
  in
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

let run tape_size program =
  Filename.with_open_temp_file ~write:(fun ch ->
    compile ch tape_size program
  ) ~f:(fun filename ->
    let output = Shell.run_full "clang" ["-Wno-override-module"; "-Os"; filename] in
    printf "%s" output
  ) "bfukc" ".ll"
