exception LexicalError of string
exception SemanticError of string

open ToyC_riscv_lib.Ast
open ToyC_riscv_lib

(* 将表达式转换为字符串 *)
let rec string_of_expr = function
  | Num n -> string_of_int n
  | Var s -> s
  | Call (name, args) -> name ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Unary (op, e) -> (match op with Plus -> "+" | Minus -> "-" | Not -> "!") ^ string_of_expr e
  | Binary (op, e1, e2) -> (match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | Lt -> "<" | Gt -> ">" | Leq -> "<=" | Geq -> ">=" | Eq -> "==" | Neq -> "!=" | And -> "&&" | Or -> "||") ^ "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

(* 将语句转换为字符串 *)
let rec string_of_stmt = function
  | BlockStmt b -> "Block(" ^ String.concat "; " (List.map string_of_stmt b.stmts) ^ ")"
  | EmptyStmt -> ";"
  | ExprStmt e -> string_of_expr e ^ ";"
  | DeclStmt (t, name, Some e) -> (match t with Int -> "int" | Void -> "void") ^ " " ^ name ^ " = " ^ string_of_expr e ^ ";"
  | DeclStmt (t, name, None) -> (match t with Int -> "int" | Void -> "void") ^ " " ^ name ^ ";"
  | AssignStmt (name, e) -> name ^ " = " ^ string_of_expr e ^ ";"
  | IfStmt (cond, then_stmt, Some else_stmt) -> "if (" ^ string_of_expr cond ^ ") " ^ string_of_stmt then_stmt ^ " else " ^ string_of_stmt else_stmt
  | IfStmt (cond, then_stmt, None) -> "if (" ^ string_of_expr cond ^ ") " ^ string_of_stmt then_stmt
  | WhileStmt (cond, s) -> "while (" ^ string_of_expr cond ^ ") " ^ string_of_stmt s
  | BreakStmt -> "break;"
  | ContinueStmt -> "continue;"
  | ReturnStmt (Some e) -> "return " ^ string_of_expr e ^ ";"
  | ReturnStmt None -> "return;"

(* 将函数定义转换为字符串 *)
let string_of_func_def fd =
  (match fd.ret_type with Int -> "int" | Void -> "void") ^ " " ^ fd.name ^ "(" ^ String.concat ", " (List.map (fun p -> (match p.typ with Int -> "int" | Void -> "void") ^ " " ^ p.name) fd.params) ^ ")" ^ " {\n" ^ string_of_stmt (BlockStmt fd.body) ^ "\n}"

(* 将 AST 转换为字符串 *)
let string_of_ast ast =
  String.concat "\n" (List.map string_of_func_def ast)

(* 辅助函数：检查函数是否在全局作用域 *)
let is_global_scope = function
  | BlockStmt _ -> false
  | _ -> true

(* 辅助函数：检查 return 语句覆盖所有路径 *)
let rec check_return_coverage stmt =
  match stmt with
  | BlockStmt b ->  (* 检查块中所有语句，直到找到返回或确定没有返回 *)
  let rec check_block stmts =
    match stmts with
    | [] -> false
    | s::rest ->
        if check_return_coverage s 
        then true  (* 当前语句返回，整个块返回 *)
        else check_block rest  (* 继续检查后续语句 *)
  in
  check_block b.stmts
  | IfStmt (cond, then_stmt, Some else_stmt) ->
      check_return_coverage then_stmt && check_return_coverage else_stmt
  | IfStmt (cond, then_stmt, None) -> false
  | WhileStmt (cond, s) -> false
  | ReturnStmt _ -> true
  | _ -> false

(* 语义分析主函数 *)
let semantic_analysis ast =
  (* 检查 main 函数存在且符合要求 *)
  let has_main = ref false in
  List.iter (fun fd ->
    if fd.name = "main" then (
      has_main := true;
      if fd.params <> [] then
        raise (SemanticError "main function must have an empty parameter list");
      if fd.ret_type <> Int then
        raise (SemanticError "main function must return int");
      if not (check_return_coverage (BlockStmt fd.body)) then
        raise (SemanticError "main function must return a value on all paths")
    ) else if fd.ret_type = Int && not (check_return_coverage (BlockStmt fd.body)) then
      raise (SemanticError (fd.name ^ " function with int return type must return a value on all paths"))
  ) ast;
  if not !has_main then
    raise (SemanticError "program must contain a main function");

  (* 检查变量声明和使用 *)
  let rec check_stmt_vars stmt declared_vars =
    match stmt with
    | DeclStmt (t, name, Some e) ->
        if t <> Int then raise (SemanticError "only int type is supported for variables");
        if List.mem name declared_vars then raise (SemanticError ("variable " ^ name ^ " redeclared"));
        name :: declared_vars
    | AssignStmt (name, e) ->
        if not (List.mem name declared_vars) then raise (SemanticError ("variable " ^ name ^ " used before declaration"));
        declared_vars
    | BlockStmt b ->
        List.fold_left (fun acc s -> check_stmt_vars s acc) declared_vars b.stmts
    | IfStmt (_, then_stmt, else_stmt_opt) ->
        let new_vars = check_stmt_vars then_stmt declared_vars in
        (match else_stmt_opt with
         | Some else_stmt -> check_stmt_vars else_stmt new_vars
         | None -> new_vars)
    | WhileStmt (_, s) -> check_stmt_vars s declared_vars
    | _ -> declared_vars
  in
  List.iter (fun (fd : ToyC_riscv_lib.Ast.func_def) ->
    let param_names = List.map (fun (p : ToyC_riscv_lib.Ast.param)  -> p.name) fd.params in
    ignore (check_stmt_vars (BlockStmt fd.body) param_names)
  ) ast;

  (* 检查 break/continue 只在循环中 *)
  let rec check_control_flow stmt in_loop =
    match stmt with
    | BreakStmt | ContinueStmt ->
        if not in_loop then raise (SemanticError "break/continue must be inside a loop")
    | BlockStmt b -> List.iter (fun s -> check_control_flow s in_loop) b.stmts
    | IfStmt (_, then_stmt, else_stmt_opt) ->
        check_control_flow then_stmt in_loop;
        (match else_stmt_opt with Some else_stmt -> check_control_flow else_stmt in_loop | None -> ())
    | WhileStmt (_, s) -> check_control_flow s true
    | _ -> ()
  in
  List.iter (fun fd -> check_control_flow (BlockStmt fd.body) false) ast;

  print_endline "Semantic analysis passed!"

let parse_channel ch =
  let lex = Lexing.from_channel ch in
  try
    Parser.comp_unit Lexer.token lex
  with
  | LexicalError msg ->
      Printf.eprintf "Lexical error: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = lex.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, column %d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
      exit 1

let () =
  let ch = open_in "test/input.toyc" in
  let ast = parse_channel ch in
  close_in ch;
  semantic_analysis ast;
  let ast_str = string_of_ast ast in
  let out_ch = open_out "ast.txt" in
  Printf.fprintf out_ch "%s\n" ast_str;
  close_out out_ch;
  print_endline "Parsing successful! AST written to ast.txt";