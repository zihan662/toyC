exception LexicalError of string [@@warning "-38"]
exception SemanticError of string

open ToyC_riscv_lib.Ast
open ToyC_riscv_lib

module StringMap = Map.Make(String)

(* ==================== IR 定义 ==================== *)
type reg = 
  | RiscvReg of string  (* RISC-V寄存器如x1-x31 *)
  | Temp of int         (* 临时变量 *)

type ir_instr =
  | Li of reg * int                (* 加载立即数 *)
  | Mv of reg * reg                (* 寄存器间移动 *)
  | BinaryOp of string * reg * reg * reg (* 二元运算 *)
  | BinaryOpImm of string * reg * reg * int (* 带立即数的二元运算 *)
  | Branch of string * reg * reg * string (* 条件分支 *)
  | Jmp of string                  (* 无条件跳转 *)
  | Label of string                (* 标签 *)
  | Call of string                 (* 函数调用 *)
  | Ret                           (* 返回 *)
  | Store of reg * reg * int       (* 存储到内存 *)
  | Load of reg * reg * int        (* 从内存加载 *)
  | ReloadVar of reg * string      (* 在函数调用后重新加载变量 *)
type ir_func = {
  name: string;
  params: string list;
  body: ir_instr list;
}

(* ==================== 代码生成状态 ==================== *)
type codegen_state = {
  temp_counter: int;
  label_counter: int;
  var_offset: (string, int) Hashtbl.t; (* 变量在栈帧中的偏移量 *)
  stack_size: int; (* 当前栈帧大小 *)
  loop_labels: (string * string) list;  (* (end_label, loop_label) 的栈 *)
  scope_stack: (string, int) Hashtbl.t list; (* 作用域栈 *)
  free_temps: int list; (* 可重用的临时寄存器列表 *)
}

let initial_state = {
  temp_counter = 0;
  label_counter = 0;
  var_offset = Hashtbl.create 10;
  stack_size = 0;
  loop_labels = [];
  scope_stack = [];
  free_temps = [];
}

(* ==================== 辅助函数 ==================== *)
let fresh_temp state = 
  match state.free_temps with
  | temp :: rest -> 
      (Temp temp, {state with free_temps = rest})
  | [] -> 
      let temp = state.temp_counter in
      (Temp temp, {state with temp_counter = temp + 1})

(* 释放临时寄存器以便重用 *)
let free_temp state temp_reg = 
  match temp_reg with
  | Temp n -> {state with free_temps = n :: state.free_temps}
  | RiscvReg _ -> state (* RISC-V寄存器不回收 *)

let fresh_label state prefix =
  let label = Printf.sprintf "%s%d" prefix state.label_counter in
  (label, {state with label_counter = state.label_counter + 1})


(* 修改 get_var_offset_for_use 函数 *)
let get_var_offset_for_use state var =
  (* 在作用域栈中查找变量，从当前作用域开始向外 *)
  let rec lookup scopes =
    match scopes with
    | [] -> None
    | scope :: remaining_scopes ->
        (try
           Some (Hashtbl.find scope var)
         with Not_found ->
           lookup remaining_scopes)
  in
  
  (* 按优先级查找：1.局部作用域 2.全局作用域 *)
  match lookup state.scope_stack with
  | Some offset -> 
      (offset, state)  (* 在某个作用域中找到了变量 *)
  | None ->
      (* 在全局作用域中查找 *)
      (try
         (Hashtbl.find state.var_offset var, state)
       with Not_found ->
         (* 变量不存在，这应该是错误情况 *)
         failwith ("Variable " ^ var ^ " not found"))
let get_var_offset_for_declaration state var =
  match state.scope_stack with
  | current_scope :: _ ->
      (* 为局部变量预留独立的负偏移量空间，避免与参数(-20, -24, ...)冲突 *)
      let offset = -(20 + state.stack_size) in  
      Hashtbl.add current_scope var offset;
      let new_state = {state with stack_size = state.stack_size + 4} in
      (offset, new_state)
  | [] ->
      let offset = state.stack_size in
      Hashtbl.add state.var_offset var offset;
      let new_state = {state with stack_size = offset + 4} in
      (offset, new_state)

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

  type func_signature = { ret_type: typ; params: param list } 
  let func_table = Hashtbl.create 30
  
  let collect_functions ast =
    List.iter (fun (fd : Ast.func_def) ->
      Hashtbl.add func_table fd.name { ret_type = fd.ret_type; params = fd.params }
    ) ast
  (*检查函数调用*)
  let rec check_expr_calls (expr : Ast.expr) =
    match expr with
    | Call (name, args) ->
        if not (Hashtbl.mem func_table name) then
          raise (SemanticError ("function " ^ name ^ " called but not defined"));
        List.iter check_expr_calls args
    | Unary (_, e) -> check_expr_calls e
    | Binary (_, e1, e2) -> check_expr_calls e1; check_expr_calls e2
    | _ -> ()
(* 语义分析主函数 *)
let semantic_analysis ast =
  collect_functions ast;
  let has_main = ref false in
  (* Symbol table stack, initialized with global scope *)
  let scope_stack = ref [StringMap.empty] in
  (* Enter a new scope *)
  let enter_scope () =
    scope_stack := StringMap.empty :: !scope_stack
  in
  (* Exit the current scope *)
  let leave_scope () =
    scope_stack := List.tl !scope_stack
  in
  (* Add a variable to the current scope *)
  let add_var name typ =
    match !scope_stack with
    | current :: rest ->
        if StringMap.mem name current then
          raise (SemanticError ("variable " ^ name ^ " redeclared"));
        scope_stack := StringMap.add name typ current :: rest
    | [] -> failwith "scope stack empty"
  in
  (* Look up variable type *)
  let rec find_var name = function
    | [] -> None
    | scope :: rest ->
        match StringMap.find_opt name scope with
        | Some t -> Some t
        | None -> find_var name rest
  in
  (* Infer expression type *)
  let rec infer_expr_type expr =
    match expr with
    | Num _ -> Int
    | Var v ->
        (match find_var v !scope_stack with
         | Some t -> t
         | None -> raise (SemanticError ("variable " ^ v ^ " used before declaration")))
    | Call (name, args) ->
        let { ret_type; params } = Hashtbl.find func_table name in
        if List.length args <> List.length params then
          raise (SemanticError ("function " ^ name ^ " called with wrong number of arguments"));
        List.iter2 (fun arg param ->
          let arg_type = infer_expr_type arg in
          if arg_type <> param.typ then
            raise (SemanticError ("type mismatch in argument of function " ^ name))
        ) args params;
        ret_type
    | Unary (op, e) ->
        (match op with
         | Plus | Minus -> infer_expr_type e
         | Not -> Int)
    | Binary (op, e1, e2) ->
        let t1 = infer_expr_type e1 in
        let t2 = infer_expr_type e2 in
        if t1 <> Int || t2 <> Int then
          raise (SemanticError "binary operation only supports int types");
        Int
  in
  (* Check statement types, variable declarations, uses, and function calls *)
  let rec check_stmt stmt expected_ret_type in_loop =
    match stmt with
    | DeclStmt (t, name, e_opt) ->
        add_var name t; (* Add variable to scope before checking expression *)
        (match e_opt with
         | Some e ->
             let expr_type = infer_expr_type e in
             if expr_type <> t then
               raise (SemanticError ("type mismatch in declaration of " ^ name))
         | None -> ())
    | AssignStmt (name, e) ->
        (match find_var name !scope_stack with
         | None -> raise (SemanticError ("variable " ^ name ^ " used before declaration"))
         | Some t ->
             let expr_type = infer_expr_type e in
             if expr_type <> t then
               raise (SemanticError ("type mismatch in assignment to " ^ name)))
    | ExprStmt e -> ignore (infer_expr_type e); check_expr_calls e
    | ReturnStmt (Some e) ->
        let expr_type = infer_expr_type e in
        if expr_type <> expected_ret_type then
          raise (SemanticError "return type mismatch")
    | ReturnStmt None ->
        if expected_ret_type <> Void then
          raise (SemanticError "missing return value in non-void function")
    | BlockStmt b ->
        enter_scope ();
        List.iter (fun s -> check_stmt s expected_ret_type in_loop) b.stmts;
        leave_scope ()
    | IfStmt (cond, then_stmt, else_stmt_opt) ->
        ignore (infer_expr_type cond);
        check_stmt then_stmt expected_ret_type in_loop;
        Option.iter (fun s -> check_stmt s expected_ret_type in_loop) else_stmt_opt
    | WhileStmt (cond, s) ->
        ignore (infer_expr_type cond);
        check_stmt s expected_ret_type true
    | BreakStmt | ContinueStmt ->
      if not in_loop then raise (SemanticError "break/continue must be inside a loop")
    | EmptyStmt -> ()
  and check_expr_calls expr =
    match expr with
    | Call (name, args) ->
        if not (Hashtbl.mem func_table name) then
          raise (SemanticError ("function " ^ name ^ " called but not defined"));
        List.iter check_expr_calls args
    | Unary (_, e) -> check_expr_calls e
    | Binary (_, e1, e2) -> check_expr_calls e1; check_expr_calls e2
    | _ -> ()
  in
  List.iter (fun (fd : ToyC_riscv_lib.Ast.func_def) ->
    if fd.name = "main" then (
      has_main := true;
      if fd.params <> [] then raise (SemanticError "main function must have an empty parameter list");
      if fd.ret_type <> Int then raise (SemanticError "main function must return int");
      if not (check_return_coverage (BlockStmt fd.body)) then
        raise (SemanticError "main function must return a value on all paths")
    ) else if fd.ret_type = Int && not (check_return_coverage (BlockStmt fd.body)) then
      raise (SemanticError (fd.name ^ " function with int return type must return a value on all paths"));
    let param_names = List.map (fun (p : ToyC_riscv_lib.Ast.param) -> p.name) fd.params in
    let initial_scope = List.fold_left (fun acc name -> StringMap.add name Int acc) StringMap.empty param_names in
    scope_stack := initial_scope :: !scope_stack;
    check_stmt (BlockStmt fd.body) fd.ret_type false;
    scope_stack := List.tl !scope_stack
  ) ast;
  if not !has_main then raise (SemanticError "program must contain a main function")

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


(* ==================== AST到IR转换 ==================== *)
let rec expr_to_ir state expr =
  match expr with
  | Num n -> 
      let (temp, state') = fresh_temp state in
      (temp, [Li (temp, n)], state')
  | Var x -> 
      let offset, state' = get_var_offset_for_use state x in
      let (temp, state'') = fresh_temp state' in
      (temp, [Load (temp, RiscvReg "s0", offset)], state'')
  (* 在 expr_to_ir 函数的 Call 分支中 *)
  | Call (name, args) ->
    (* 处理参数 *)
    let (processed_args, final_state) = List.fold_left (
      fun (acc_results, acc_state) arg_expr ->
        let (reg, code, new_state) = expr_to_ir acc_state arg_expr in
        ((reg, code) :: acc_results, new_state)
    ) ([], state) args in
    
    let ordered_args = List.rev processed_args in
    let (arg_regs, arg_code_lists) = List.split ordered_args in
    let all_arg_codes = List.flatten arg_code_lists in
    
    let state_after_free = List.fold_left (fun st reg -> free_temp st reg) final_state arg_regs in
    
    (* 修改这里：正确处理超过8个参数 *)
    let move_instructions = List.mapi (
      fun i reg ->
        if i < 8 then
          [Mv (RiscvReg ("a" ^ string_of_int i), reg)]
        else
          (* 超过8个的参数需要压栈 *)
          [Store (reg, RiscvReg "sp", -(4 * (i - 8)))]  (* 简化的栈存储 *)
    ) arg_regs in
    
    let move_codes = List.flatten move_instructions in
    
    (* 调用函数 *)
    let is_void_func = 
      try
        let func_info = Hashtbl.find func_table name in
        func_info.ret_type = Void
      with Not_found -> false
    in
    if is_void_func then
      (* void函数调用 *)
      let (temp, state') = fresh_temp state_after_free in
      (temp, all_arg_codes @ move_codes @ [Call name; Li (temp, 0)], state')
    else
      (* 非void函数调用 *)
      let (result_reg, state') = fresh_temp state_after_free in
      (result_reg, all_arg_codes @ move_codes @ [Call name; Mv (result_reg, RiscvReg "a0")], state')
  | Unary (op, e) ->
      let (e_reg, e_code, state') = expr_to_ir state e in
      let (temp, state'') = fresh_temp state' in
      let state''' = free_temp state'' e_reg in (* 释放输入寄存器 *)
      (match op with
       | Plus -> (e_reg, e_code, state')
       | Minus -> (temp, e_code @ [BinaryOp ("sub", temp, RiscvReg "zero", e_reg)], state''')
       | Not -> 
           let (ne_temp, state'''') = fresh_temp state''' in
           let state_final = free_temp state'''' ne_temp in
           (temp, e_code @ [BinaryOp ("sltu", ne_temp, RiscvReg "zero", e_reg);
                            BinaryOpImm ("xori", temp, ne_temp, 1)], state_final))
    | Binary (op, e1, e2) ->
    let (e1_reg, e1_code, state') = expr_to_ir state e1 in
    let (e2_reg, e2_code, state'') = expr_to_ir state' e2 in
    let (temp, state''') = fresh_temp state'' in
    let state_final = free_temp (free_temp state''' e1_reg) e2_reg in (* 释放输入寄存器 *)
    
    match op with
    | Add -> 
        (temp, e1_code @ e2_code @ [BinaryOp ("add", temp, e1_reg, e2_reg)], state_final)
    | Sub -> 
      (match e2 with
      | Num n -> 
          (temp, e1_code @ [BinaryOpImm ("addi", temp, e1_reg, -n)], state_final)
      | _ -> 
          (temp, e1_code @ e2_code @ [BinaryOp ("sub", temp, e1_reg, e2_reg)], state_final))
    | Mul -> 
        (* 检查e1和e2中是否有函数调用 *)
        let e1_has_call = List.exists (function Call _ -> true | _ -> false) e1_code in
        let e2_has_call = List.exists (function Call _ -> true | _ -> false) e2_code in
        
        (match (e1, e2) with
        | (Var x1, _) when e2_has_call -> 
            (* 如果左侧是变量，右侧包含函数调用 *)
            (temp, e1_code @ e2_code @ [ReloadVar (e1_reg, x1); BinaryOp ("mul", temp, e1_reg, e2_reg)], state_final)
        | (_, Var x2) when e1_has_call -> 
            (* 如果右侧是变量，左侧包含函数调用 *)
            (temp, e1_code @ e2_code @ [ReloadVar (e2_reg, x2); BinaryOp ("mul", temp, e1_reg, e2_reg)], state_final)
        | _ -> 
            (temp, e1_code @ e2_code @ [BinaryOp ("mul", temp, e1_reg, e2_reg)], state_final))
    | Div -> 
        (temp, e1_code @ e2_code @ [BinaryOp ("div", temp, e1_reg, e2_reg)], state_final)
    | Mod -> 
        (temp, e1_code @ e2_code @ [BinaryOp ("rem", temp, e1_reg, e2_reg)], state_final)
    | Lt -> 
        (temp, e1_code @ e2_code @ [BinaryOp ("slt", temp, e1_reg, e2_reg)], state_final)
    | Gt -> 
        (* a > b 转换为 b < a *)
        let code = e1_code @ e2_code @ [BinaryOp ("slt", temp, e2_reg, e1_reg)] in
        (temp, code, state_final)
    | Leq ->
    (* a <= b 转换为 !(b < a) *)
    let (lt_temp, state'''') = fresh_temp state_final in
    let state_final' = free_temp state'''' lt_temp in
    let code = e1_code @ e2_code @
      [BinaryOp ("slt", lt_temp, e2_reg, e1_reg);
       BinaryOpImm ("xori", temp, lt_temp, 1)] in
    (temp, code, state_final')
    | Geq ->
        (* a >= b 转换为 !(a < b) *)
        let (lt_temp, state'''') = fresh_temp state_final in
        let state_final' = free_temp state'''' lt_temp in
        let code = e1_code @ e2_code @
          [BinaryOp ("slt", lt_temp, e1_reg, e2_reg);
          BinaryOpImm ("xori", temp, lt_temp, 1)] in
        (temp, code, state_final')
    | Eq ->
        (* a == b 转换为 (a ^ b) == 0 *)
        let (xor_temp, state'''') = fresh_temp state_final in
        let (sltu_temp, state''''') = fresh_temp state'''' in
        let state_final'' = free_temp (free_temp state''''' xor_temp) sltu_temp in
        let code = e1_code @ e2_code @
          [BinaryOp ("xor", xor_temp, e1_reg, e2_reg);
          BinaryOp ("sltu", sltu_temp, RiscvReg "zero", xor_temp);
          BinaryOpImm ("xori", temp, sltu_temp, 1)] in
        (temp, code, state_final'')
  | _ -> failwith "Unsupported expression"

let enter_scope state =
  let new_scope = Hashtbl.create 10 in
  {state with scope_stack = new_scope :: state.scope_stack}

let leave_scope state =
  match state.scope_stack with
  | [] -> state  (* 没有作用域可退出 *)
  | _ :: rest -> {state with scope_stack = rest}

let rec stmt_to_ir state stmt =
  match stmt with
  | BlockStmt b -> block_to_ir state b
  | DeclStmt (_, name, Some expr) -> (* 带初始化的声明 *)
      let (expr_reg, expr_code, state') = expr_to_ir state expr in
      let offset, state'' = get_var_offset_for_declaration state' name in
      let state''' = free_temp state'' expr_reg in (* 释放表达式寄存器 *)
      (expr_code @ [Store (expr_reg, RiscvReg "s0", offset)], state''')
  | DeclStmt (_, name, None) -> (* 不带初始化的声明 *)
      let offset, state' = get_var_offset_for_declaration state name in
      ([], state')
  | AssignStmt (name, expr) ->
      let (expr_reg, expr_code, state') = expr_to_ir state expr in
      let offset, state'' = get_var_offset_for_use state' name in
      let state''' = free_temp state'' expr_reg in (* 释放表达式寄存器 *)
      (expr_code @ [Store (expr_reg, RiscvReg "s0", offset)], state''')
  | IfStmt (cond, then_stmt, else_stmt_opt) ->
      let (cond_reg, cond_code, state') = expr_to_ir state cond in
      let (then_label, state'') = fresh_label state' "then" in
      let (else_label, state''') = fresh_label state'' "else" in
      let (merge_label, state'''') = fresh_label state''' "merge" in
      let (then_code, state''''') = stmt_to_ir state'''' then_stmt in
      let (else_code, state'''''') = 
        match else_stmt_opt with
        | Some s -> stmt_to_ir state''''' s
        | None -> ([], state''''') in
      let state_final = free_temp state'''''' cond_reg in (* 释放条件寄存器 *)
      (cond_code @ 
       [Branch ("bnez", cond_reg, RiscvReg "zero", then_label);
        Jmp else_label;
        Label then_label] @
       then_code @
       [Jmp merge_label;
        Label else_label] @
       else_code @
       [Label merge_label], state_final)
  | ReturnStmt (Some expr) ->
      let (expr_reg, expr_code, state') = expr_to_ir state expr in
      let state'' = free_temp state' expr_reg in (* 释放表达式寄存器 *)
      (expr_code @ [Mv (RiscvReg "a0", expr_reg); Ret], state'')
  | ReturnStmt None ->
      ([Ret], state)
  | ExprStmt expr ->
      let (expr_reg, expr_code, state') = expr_to_ir state expr in
      let state'' = free_temp state' expr_reg in (* 释放表达式寄存器 *)
      (expr_code, state'')
  | WhileStmt (cond, body) ->
      let (loop_label, state') = fresh_label state "loop" in
      let (end_label, state'') = fresh_label state' "end" in
      
      let state_with_loop = { state'' with loop_labels = (end_label, loop_label) :: state''.loop_labels } in
      
      let (cond_reg, cond_code, state''') = expr_to_ir state_with_loop cond in
      let (body_code, state'''') = stmt_to_ir state''' body in
      let state_final = free_temp state'''' cond_reg in (* 释放条件寄存器 *)
      
      ( [Label loop_label] @
        cond_code @
        [Branch ("beqz", cond_reg, RiscvReg "zero", end_label)] @
        body_code @
        [Jmp loop_label;
         Label end_label],
        { state_final with loop_labels = List.tl state_final.loop_labels } )
  
  | BreakStmt ->
      (match state.loop_labels with
       | (end_label, _) :: _ -> ([Jmp end_label], state)
       | [] -> failwith "break statement outside loop")
  | ContinueStmt ->
      (match state.loop_labels with
       | (_, loop_label) :: _ -> ([Jmp loop_label], state)
       | [] -> failwith "continue statement outside loop")
  | EmptyStmt ->
      ([], state)

and block_to_ir state block =
  let state_with_scope = enter_scope state in
  let (code, final_state) = List.fold_left (fun (code_acc, st) stmt ->
    let (code, st') = stmt_to_ir st stmt in
    (code_acc @ code, st')
  ) ([], state_with_scope) block.stmts in
  let exited_state = leave_scope final_state in
  (code, exited_state)


let func_to_ir (func : Ast.func_def) : (ir_func * (string, int) Hashtbl.t) =
  let state = { 
    initial_state with 
    var_offset = Hashtbl.create (List.length func.params);
  } in
    (* 为参数设置固定的偏移量，使用负偏移量与标准代码一致 *)
    let state' = 
    List.fold_left (fun st (i, (param : Ast.param)) ->
      (* 使用与标准代码相同的参数偏移量 *)
      let offset = -(20 + i * 4) in  (* 参数偏移量: -20, -24, -28, ... *)
      Hashtbl.add st.var_offset param.name offset;
      st
    ) state (List.mapi (fun i x -> (i, x)) func.params)
  in
  let (body_code, final_state) = block_to_ir state' func.body in
  {
    name = func.name;
    params = List.map (fun (p : Ast.param) -> p.name) func.params;
    body = body_code;
  }, final_state.var_offset


(* ==================== IR到RISC-V汇编转换 ==================== *)
module IRToRiscV = struct
  (* 寄存器分配映射 *)
  let reg_map var_offsets frame_size = function
    | RiscvReg s -> s
    | Temp n -> 
        if n < 7 then 
          Printf.sprintf "t%d" n
        else if n < 15 then 
          Printf.sprintf "a%d" (n - 7)
        else
          (* 当寄存器不足时，使用栈空间，统一使用s0作为基址 *)
          let stack_offset = -(20 + (n - 15 + 1) * 4) in
          Printf.sprintf "%d(s0)" stack_offset

  (* 修改instr_to_asm函数以处理栈访问 *)
  let instr_to_asm var_offsets frame_size = function
    | Li (r, n) -> 
        (match r with
        | Temp n when n >= 15 -> 
            (* 直接存储到栈上，使用s0作为基址 *)
            let stack_offset = -(20 + (n - 15 + 1) * 4) in
            Printf.sprintf "  li t0, %d\n  sw t0, %d(s0)" n stack_offset
        | _ -> 
            Printf.sprintf "  li %s, %d" (reg_map var_offsets frame_size r) n)
    | Mv (rd, rs) ->
        (match (rd, rs) with
        | (Temp n, _) when n >= 15 -> 
            (* 目标是栈位置，先移动到临时寄存器再存储 *)
            let stack_offset = -(20 + (n - 15 + 1) * 4) in
            Printf.sprintf "  mv t0, %s\n  sw t0, %d(s0)" (reg_map var_offsets frame_size rs) stack_offset
        | (_, Temp n) when n >= 15 -> 
            (* 源是栈位置，先加载再移动 *)
            let stack_offset = -(20 + (n - 15 + 1) * 4) in
            Printf.sprintf "  lw t0, %d(s0)\n  mv %s, t0" stack_offset (reg_map var_offsets frame_size rd)
        | _ -> 
            Printf.sprintf "  mv %s, %s" (reg_map var_offsets frame_size rd) (reg_map var_offsets frame_size rs))
    | BinaryOp (op, rd, rs1, rs2) ->
        (match (rd, rs1, rs2) with
        | (Temp n, _, _) when n >= 15 -> 
            (* 结果存储到栈 *)
            let stack_offset = -(20 + (n - 15 + 1) * 4) in
            let src1 = reg_map var_offsets frame_size rs1 in
            let src2 = reg_map var_offsets frame_size rs2 in
            let (reg1, load1) = 
              match rs1 with
              | Temp m when m >= 15 -> 
                  let offset = -(20 + (m - 15 + 1) * 4) in
                  ("t1", Printf.sprintf "  lw t1, %d(s0)\n" offset)
              | _ -> (src1, "")
            in
            let (reg2, load2) = 
              match rs2 with
              | Temp m when m >= 15 -> 
                  let offset = -(20 + (m - 15 + 1) * 4) in
                  ("t2", Printf.sprintf "  lw t2, %d(s0)\n" offset)
              | _ -> (src2, "")
            in
            load1 ^ load2 ^ 
            Printf.sprintf "  %s t0, %s, %s\n  sw t0, %d(s0)" op reg1 reg2 stack_offset
        | _ ->
            let dest = reg_map var_offsets frame_size rd in
            let (reg1, load1) = 
              match rs1 with
              | Temp m when m >= 15 -> 
                  let offset = -(20 + (m - 15 + 1) * 4) in
                  ("t1", Printf.sprintf "  lw t1, %d(s0)\n" offset)
              | _ -> (reg_map var_offsets frame_size rs1, "")
            in
            let (reg2, load2) = 
              match rs2 with
              | Temp m when m >= 15 -> 
                  let offset = -(20 + (m - 15 + 1) * 4) in
                  ("t2", Printf.sprintf "  lw t2, %d(s0)\n" offset)
              | _ -> (reg_map var_offsets frame_size rs2, "")
            in
            let dest_reg = 
              match rd with
              | Temp m when m >= 15 -> "t0"
              | _ -> dest
            in
            load1 ^ load2 ^ 
            Printf.sprintf "  %s %s, %s, %s" op dest_reg reg1 reg2 ^
            (match rd with
              | Temp m when m >= 15 -> 
                  let stack_offset = -(20 + (m - 15 + 1) * 4) in
                  Printf.sprintf "\n  sw t0, %d(s0)" stack_offset
              | _ -> ""))
    | BinaryOpImm (op, rd, rs, imm) ->
        (match (rd, rs) with
        | (Temp n, _) when n >= 15 -> 
            (* 结果存储到栈 *)
            let stack_offset = -(20 + (n - 15 + 1) * 4) in
            let src = reg_map var_offsets frame_size rs in
            let (reg, load) = 
              match rs with
              | Temp m when m >= 15 -> 
                  let offset = -(20 + (m - 15 + 1) * 4) in
                  ("t1", Printf.sprintf "  lw t1, %d(s0)\n" offset)
              | _ -> (src, "")
            in
            load ^ 
            Printf.sprintf "  %s t0, %s, %d\n  sw t0, %d(s0)" op reg imm stack_offset
        | _ ->
            let dest = reg_map var_offsets frame_size rd in
            let (reg, load) = 
              match rs with
              | Temp m when m >= 15 -> 
                  let offset = -(20 + (m - 15 + 1) * 4) in
                  ("t1", Printf.sprintf "  lw t1, %d(s0)\n" offset)
              | _ -> (reg_map var_offsets frame_size rs, "")
            in
            let dest_reg = 
              match rd with
              | Temp m when m >= 15 -> "t0"
              | _ -> dest
            in
            load ^ 
            Printf.sprintf "  %s %s, %s, %d" op dest_reg reg imm ^
            (match rd with
              | Temp m when m >= 15 -> 
                  let stack_offset = -(20 + (m - 15 + 1) * 4) in
                  Printf.sprintf "\n  sw t0, %d(s0)" stack_offset
              | _ -> ""))
    | Branch (cond, rs1, rs2, label) ->
        let (reg1, load1) = 
          match rs1 with
          | Temp m when m >= 15 -> 
              let offset = -(20 + (m - 15 + 1) * 4) in
              ("t1", Printf.sprintf "  lw t1, %d(s0)\n" offset)
          | _ -> (reg_map var_offsets frame_size rs1, "")
        in
        let (reg2, load2) = 
          match rs2 with
          | Temp m when m >= 15 -> 
              let offset = -(20 + (m - 15 + 1) * 4) in
              ("t2", Printf.sprintf "  lw t2, %d(s0)\n" offset)
          | _ -> (reg_map var_offsets frame_size rs2, "")
        in
        load1 ^ load2 ^
        (match cond with
        | "beqz" -> Printf.sprintf "  beq %s, zero, %s" reg1 label
        | "bnez" -> Printf.sprintf "  bne %s, zero, %s" reg1 label
        | _ -> Printf.sprintf "  %s %s, %s, %s" cond reg1 reg2 label)
    | Store (rs, base, offset) ->
        let (reg, load) = 
          match rs with
          | Temp m when m >= 15 -> 
              let stack_offset = -(20 + (m - 15 + 1) * 4) in
              ("t0", Printf.sprintf "  lw t0, %d(s0)\n" stack_offset)
          | _ -> (reg_map var_offsets frame_size rs, "")
        in
        load ^ Printf.sprintf "  sw %s, %d(%s)" reg offset (reg_map var_offsets frame_size base)
    | Load (rd, base, offset) ->
        (match rd with
        | Temp n when n >= 15 -> 
            (* 加载到栈位置 *)
            let stack_offset = -(20 + (n - 15 + 1) * 4) in
            Printf.sprintf "  lw t0, %d(%s)\n  sw t0, %d(s0)" 
              offset (reg_map var_offsets frame_size base) stack_offset
        | _ -> 
            Printf.sprintf "  lw %s, %d(%s)" 
              (reg_map var_offsets frame_size rd) offset (reg_map var_offsets frame_size base))
    (* 修改 ReloadVar 处理 *)
    | ReloadVar (reg, var_name) ->
        (try
          let offset = Hashtbl.find var_offsets var_name in
          (match reg with
          | Temp n when n >= 15 -> 
              let temp_offset = -(20 + (n - 15 + 1) * 4) in
              Printf.sprintf "  lw t0, %d(s0)\n  sw t0, %d(s0)" offset temp_offset
          | _ -> 
              Printf.sprintf "  lw %s, %d(s0)" (reg_map var_offsets frame_size reg) offset)
        with Not_found ->
          failwith ("Variable " ^ var_name ^ " not found during code generation"))
    | instr -> 
        (* 其他指令保持原有处理方式 *)
        match instr with
        | Jmp label -> "  j " ^ label
        | Label label -> label ^ ":"
        | Call func -> "  call " ^ func
        | Ret ->
            let ra_offset = frame_size - 4 in      
            let s0_offset = frame_size - 8 in      
            Printf.sprintf "  lw ra, %d(sp)\n" ra_offset ^
            Printf.sprintf "  lw s0, %d(sp)\n" s0_offset ^
            Printf.sprintf "  addi sp, sp, %d\n" frame_size ^
            "  jr ra"
        | _ -> failwith "Unhandled instruction"
        
  (* 计算函数需要的栈帧大小 *)
  let calculate_frame_size (ir_func : ir_func) =
    (* 基础保存空间：ra(4) + s0(4) = 8字节 *)
    let base_size = 8 in
    
    (* 参数空间：最多8个寄存器参数(a0-a7)，其余通过栈传递 *)
    let param_size = max 0 (List.length ir_func.params - 8) * 4 in
    
    (* 收集所有使用的栈偏移量 *)
    let used_offsets = Hashtbl.create 50 in
    
    (* 标记参数使用的偏移量 *)
    List.iteri (fun i _ ->
      if i >= 8 then Hashtbl.add used_offsets (-(20 + i * 4)) ()
    ) ir_func.params;
    
    (* 分析指令中的栈使用情况 *)
    let max_temp = ref (-1) in
    let local_vars = Hashtbl.create 50 in
    
    let analyze_instr = function
      | Store (_, RiscvReg "s0", offset) when offset < 0 ->
          Hashtbl.replace local_vars offset ()
      | Load (Temp n, RiscvReg "s0", offset) when offset < 0 ->
          Hashtbl.replace local_vars offset ();
          max_temp := max !max_temp n
      | Li (Temp n, _) | Mv (Temp n, _) | BinaryOp (_, Temp n, _, _)
      | BinaryOpImm (_, Temp n, _, _) | ReloadVar (Temp n, _) ->
          max_temp := max !max_temp n
      | _ -> ()
    in
    List.iter analyze_instr ir_func.body;
    
    (* 计算局部变量所需空间 *)
    let local_var_size = 
      if Hashtbl.length local_vars > 0 then
        let min_offset = Hashtbl.fold (fun k _ acc -> min k acc) local_vars 0 in
        abs min_offset - 20  (* 从-24开始计算 *)
      else 0 in
    
    (* 计算临时寄存器所需空间 *)
    let temp_stack_size = 
      if !max_temp >= 15 then (!max_temp - 14) * 4 else 0 in
    
    (* 总需求空间 *)
    let required_space = base_size + param_size + local_var_size + temp_stack_size in
    
    (* 对齐到16字节边界 *)
    let aligned_size = ((required_space + 15) / 16) * 16 in
    
    (* 确保最小栈帧大小 *)
    max aligned_size 32

  (* 函数序言 - 动态计算栈帧大小 *)
  let function_prologue name frame_size =
    let ra_offset = frame_size - 4 in      (* ra保存在栈帧顶部 *)
    let s0_offset = frame_size - 8 in      (* s0保存在ra下方 *)
    Printf.sprintf "%s:\n" name ^
    Printf.sprintf "  addi sp, sp, -%d\n" frame_size ^
    Printf.sprintf "  sw ra, %d(sp)\n" ra_offset ^
    Printf.sprintf "  sw s0, %d(sp)\n" s0_offset ^
    Printf.sprintf "  addi s0, sp, %d\n" frame_size

  (* 函数尾声 *)
  let function_epilogue frame_size =
    let ra_offset = frame_size - 4 in 
    let s0_offset = frame_size - 8 in 
    Printf.sprintf "  lw ra, %d(sp)\n" ra_offset ^
    Printf.sprintf "  lw s0, %d(sp)\n" s0_offset ^
    Printf.sprintf "  addi sp, sp, %d\n" frame_size ^
    "  ret\n"  (* 使用ret而不是jr ra *)

  (* 转换整个IR函数 *)
  let func_to_asm var_offsets (ir_func : ir_func) =
    let buf = Buffer.create 256 in
    let frame_size = calculate_frame_size ir_func in
    
    Buffer.add_string buf (function_prologue ir_func.name frame_size);
      
    (* 保存参数到栈帧 - 使用负偏移量 *)
    List.iteri (fun i param ->
      let offset = -(20 + i * 4) in
      if i < 8 then
        (* 前8个参数从a0-a7寄存器加载 *)
        Buffer.add_string buf (Printf.sprintf "  sw a%d, %d(s0)\n" i offset)
      else
        (* 超过8个的参数已经在调用时通过栈传递，这里只需要注释 *)
        Buffer.add_string buf (Printf.sprintf "  # 参数%d (%s) 已通过栈传递到偏移量 %d(s0)\n" i param offset)
    ) ir_func.params;
    
    (* 转换指令，传入var_offsets和frame_size参数 *)
    List.iter (fun instr ->
      Buffer.add_string buf (instr_to_asm var_offsets frame_size instr ^ "\n")
    ) ir_func.body;
    
    (* 检查是否已经有显式的返回指令 *)
    let has_explicit_return = List.exists (function Ret -> true | _ -> false) ir_func.body in
    
    (* 如果没有显式返回指令，则添加函数尾声 *)
    if not has_explicit_return then (
      Buffer.add_string buf (function_epilogue frame_size);
    );
    
    Buffer.contents buf
end
  
(* 修改最后的输出部分 *)
let () =
  (* 从标准输入读取 *)
  let ast = parse_channel stdin in
  semantic_analysis ast;
  
  (* 转换为IR *)
  let ir_with_offsets = List.map func_to_ir ast in
  
  (* 分离IR函数和变量偏移量表 *)
  let (ir_funcs, var_offsets_list) = List.split ir_with_offsets in
  
  (* 创建一个合并的变量偏移量表 *)
  let combined_var_offsets = Hashtbl.create 50 in
  List.iter (fun var_offsets ->
    Hashtbl.iter (fun name offset ->
      Hashtbl.add combined_var_offsets name offset
    ) var_offsets
  ) var_offsets_list;
  
  (* 转换为RISC-V汇编 *)
  let riscv_asm = List.map (IRToRiscV.func_to_asm combined_var_offsets) ir_funcs in
  
  (* 只输出RISC-V汇编到标准输出 *)
  (* 添加汇编头部 *)
  Printf.printf ".global main\n";
  (* 输出每个函数 *)
  List.iter (fun f -> 
    Printf.printf "%s\n" f
  ) riscv_asm;
  
  (* 完成提示到标准错误 *)
  prerr_endline "Compilation successful!";