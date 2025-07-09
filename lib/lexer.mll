{
open Parser  (* 导入语法分析器的Token类型 *)
exception LexicalError of string
}

rule token = parse
  (* 关键字：必须在标识符前匹配 *)
  | "int"       { INT }
  | "void"      { VOID }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "break"     { BREAK }
  | "continue"  { CONTINUE }
  | "return"    { RETURN }

  (* 运算符与标点：严格匹配ToyC文法 *)
  | "=="        { EQ }
  | "!="        { NEQ }
  | "<="        { LEQ }
  | ">="        { GEQ }
  | "&&"        { AND }
  | "||"        { OR }   (* ToyC是||，对应OR Token *)
  | "="         { ASSIGN }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { MUL }
  | "/"         { DIV }
  | "%"         { MOD }
  | "!"         { NOT }
  | "<"         { LT }
  | ">"         { GT }
  | ";"         { SEMI }
  | ","         { COMMA }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "{"         { LBRACE }
  | "}"         { RBRACE }

  (* 标识符：变量名/函数名 *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }

  (* 数字常量 *)
  | ['0'-'9']+ as n { NUMBER (int_of_string n) }

  (* 空白：跳过 *)
  | [' ' '\t' '\n' '\r']+  { token lexbuf }  (* 递归调用继续分词 *)

  (* 文件结束 *)
  | eof         { EOF }

  (* 未知字符：抛词法错误 *)
  | _ as c      { raise (LexicalError (Printf.sprintf "Unexpected character: %c" c)) }