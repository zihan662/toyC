
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WHILE
    | VOID
    | SEMI
    | RPAREN
    | RETURN
    | RBRACE
    | PLUS
    | OR
    | NUMBER of (
# 7 "lib/parser.mly"
       (int)
# 22 "lib/parser.ml"
  )
    | NOT
    | NEQ
    | MUL
    | MOD
    | MINUS
    | LT
    | LPAREN
    | LEQ
    | LBRACE
    | INT
    | IF
    | ID of (
# 6 "lib/parser.mly"
       (string)
# 38 "lib/parser.ml"
  )
    | GT
    | GEQ
    | EQ
    | EOF
    | ELSE
    | DIV
    | CONTINUE
    | COMMA
    | BREAK
    | ASSIGN
    | AND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState105
  | MenhirState101
  | MenhirState99
  | MenhirState95
  | MenhirState90
  | MenhirState82
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState66
  | MenhirState60
  | MenhirState58
  | MenhirState53
  | MenhirState46
  | MenhirState44
  | MenhirState41
  | MenhirState36
  | MenhirState31
  | MenhirState19
  | MenhirState18
  | MenhirState16
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState3
  | MenhirState0

# 1 "lib/parser.mly"
  
open Ast  (* 导入 AST 结构，用于构造节点 *)

# 95 "lib/parser.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (args : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 159 "lib/parser.mly"
                                                ( args )
# 107 "lib/parser.ml"
         in
        _menhir_goto_expr_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 118 "lib/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_lor_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NUMBER _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | COMMA | RPAREN | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 84 "lib/parser.mly"
                 ( e )
# 158 "lib/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState53 | MenhirState18 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | NOT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | NUMBER _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
                | PLUS ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 194 "lib/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState16 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.expr) = 
# 153 "lib/parser.mly"
                           ( e )
# 216 "lib/parser.ml"
                 in
                _menhir_goto_primary_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState11 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | CONTINUE ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | ID _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                | IF ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | INT ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | LBRACE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | NOT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | NUMBER _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                | PLUS ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | RETURN ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | SEMI ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState60 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.stmt) = 
# 77 "lib/parser.mly"
                       (ReturnStmt(Some e))
# 286 "lib/parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState66 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), (name : (
# 6 "lib/parser.mly"
       (string)
# 307 "lib/parser.ml"
                ))), _, (expr : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.stmt) = 
# 68 "lib/parser.mly"
                                          ( DeclStmt (Int, name, Some expr) )
# 312 "lib/parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState70 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | CONTINUE ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | ID _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | IF ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | INT ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | LBRACE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NOT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NUMBER _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | PLUS ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | RETURN ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | SEMI ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState74 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (name : (
# 6 "lib/parser.mly"
       (string)
# 381 "lib/parser.ml"
                ))), _, (expr : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.stmt) = 
# 67 "lib/parser.mly"
                                      ( AssignStmt (name, expr) )
# 386 "lib/parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState9 | MenhirState90 | MenhirState58 | MenhirState72 | MenhirState82 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (expr : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.stmt) = 
# 66 "lib/parser.mly"
                     ( ExprStmt expr )
# 408 "lib/parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_rel_op : _menhir_env -> 'ttv_tail -> (Ast.bin_op) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_goto_land_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | OR | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 90 "lib/parser.mly"
                                    ( Binary (Or, e1, e2) )
# 489 "lib/parser.ml"
             in
            _menhir_goto_lor_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 | MenhirState90 | MenhirState58 | MenhirState72 | MenhirState82 | MenhirState74 | MenhirState70 | MenhirState66 | MenhirState60 | MenhirState11 | MenhirState16 | MenhirState53 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | OR | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 89 "lib/parser.mly"
                  ( e )
# 511 "lib/parser.ml"
             in
            _menhir_goto_lor_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run25 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 111 "lib/parser.mly"
        ( Neq )
# 530 "lib/parser.ml"
     in
    _menhir_goto_rel_op _menhir_env _menhir_stack _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 106 "lib/parser.mly"
       ( Lt )
# 541 "lib/parser.ml"
     in
    _menhir_goto_rel_op _menhir_env _menhir_stack _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 108 "lib/parser.mly"
        ( Leq )
# 552 "lib/parser.ml"
     in
    _menhir_goto_rel_op _menhir_env _menhir_stack _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 107 "lib/parser.mly"
       ( Gt )
# 563 "lib/parser.ml"
     in
    _menhir_goto_rel_op _menhir_env _menhir_stack _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 109 "lib/parser.mly"
        ( Geq )
# 574 "lib/parser.ml"
     in
    _menhir_goto_rel_op _menhir_env _menhir_stack _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 110 "lib/parser.mly"
       ( Eq )
# 585 "lib/parser.ml"
     in
    _menhir_goto_rel_op _menhir_env _menhir_stack _v

and _menhir_goto_add_op : _menhir_env -> 'ttv_tail -> (Ast.bin_op) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_goto_rel_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 | MenhirState90 | MenhirState58 | MenhirState72 | MenhirState82 | MenhirState74 | MenhirState70 | MenhirState66 | MenhirState60 | MenhirState11 | MenhirState16 | MenhirState53 | MenhirState44 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 95 "lib/parser.mly"
                 ( e )
# 640 "lib/parser.ml"
             in
            _menhir_goto_land_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 96 "lib/parser.mly"
                                     ( Binary (And, e1, e2) )
# 672 "lib/parser.ml"
             in
            _menhir_goto_land_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run39 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 121 "lib/parser.mly"
         ( Add )
# 691 "lib/parser.ml"
     in
    _menhir_goto_add_op _menhir_env _menhir_stack _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 122 "lib/parser.mly"
          ( Sub )
# 702 "lib/parser.ml"
     in
    _menhir_goto_add_op _menhir_env _menhir_stack _v

and _menhir_goto_mul_op : _menhir_env -> 'ttv_tail -> (Ast.bin_op) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_goto_add_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), (op : (Ast.bin_op))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 102 "lib/parser.mly"
                                            ( Binary (op, e1, e2) )
# 749 "lib/parser.ml"
             in
            _menhir_goto_rel_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 | MenhirState90 | MenhirState11 | MenhirState58 | MenhirState70 | MenhirState72 | MenhirState82 | MenhirState74 | MenhirState66 | MenhirState60 | MenhirState16 | MenhirState18 | MenhirState53 | MenhirState44 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 101 "lib/parser.mly"
                 ( e )
# 773 "lib/parser.ml"
             in
            _menhir_goto_rel_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run33 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 132 "lib/parser.mly"
        ( Mul )
# 792 "lib/parser.ml"
     in
    _menhir_goto_mul_op _menhir_env _menhir_stack _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 134 "lib/parser.mly"
        ( Mod )
# 803 "lib/parser.ml"
     in
    _menhir_goto_mul_op _menhir_env _menhir_stack _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.bin_op) = 
# 133 "lib/parser.mly"
        ( Div )
# 814 "lib/parser.ml"
     in
    _menhir_goto_mul_op _menhir_env _menhir_stack _v

and _menhir_goto_mul_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 | MenhirState90 | MenhirState58 | MenhirState72 | MenhirState82 | MenhirState74 | MenhirState70 | MenhirState66 | MenhirState60 | MenhirState11 | MenhirState16 | MenhirState53 | MenhirState46 | MenhirState44 | MenhirState18 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 116 "lib/parser.mly"
                 ( e )
# 839 "lib/parser.ml"
             in
            _menhir_goto_add_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), (op : (Ast.bin_op))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 117 "lib/parser.mly"
                                            ( Binary (op, e1, e2) )
# 865 "lib/parser.ml"
             in
            _menhir_goto_add_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_func_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.func_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EOF ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_goto_unary_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Ast.expr)) = _v in
        let (_menhir_stack, _menhir_s, (op : (Ast.unary_op))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 140 "lib/parser.mly"
                                 ( Unary (op, e) )
# 906 "lib/parser.ml"
         in
        _menhir_goto_unary_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 | MenhirState9 | MenhirState58 | MenhirState82 | MenhirState72 | MenhirState74 | MenhirState70 | MenhirState66 | MenhirState60 | MenhirState11 | MenhirState16 | MenhirState53 | MenhirState46 | MenhirState44 | MenhirState41 | MenhirState31 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Ast.expr)) = _v in
        let _v : (Ast.expr) = 
# 127 "lib/parser.mly"
                   ( e )
# 916 "lib/parser.ml"
         in
        _menhir_goto_mul_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e2 : (Ast.expr)) = _v in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), (op : (Ast.bin_op))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 128 "lib/parser.mly"
                                              ( Binary (op, e1, e2) )
# 927 "lib/parser.ml"
         in
        _menhir_goto_mul_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (name : (
# 6 "lib/parser.mly"
       (string)
# 947 "lib/parser.ml"
        ))), _, (args : (Ast.expr list))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 154 "lib/parser.mly"
                                             ( Call (name, args) )
# 952 "lib/parser.ml"
         in
        _menhir_goto_primary_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.stmt list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.stmt))) = _menhir_stack in
        let _v : (Ast.stmt list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 973 "lib/parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (stmts : (Ast.stmt list)) = _v in
        let _v : (Ast.stmt list) = 
# 59 "lib/parser.mly"
                       ( stmts )
# 983 "lib/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (stmts : (Ast.stmt list))) = _menhir_stack in
            let _v : (Ast.block) = 
# 54 "lib/parser.mly"
                                    ( { stmts } )
# 998 "lib/parser.ml"
             in
            (match _menhir_s with
            | MenhirState9 | MenhirState90 | MenhirState58 | MenhirState72 | MenhirState82 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (block : (Ast.block)) = _v in
                let _v : (Ast.stmt) = 
# 64 "lib/parser.mly"
                  ( BlockStmt block )
# 1008 "lib/parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | MenhirState8 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (body : (Ast.block)) = _v in
                let (((_menhir_stack, _menhir_s), (name : (
# 6 "lib/parser.mly"
       (string)
# 1018 "lib/parser.ml"
                ))), _, (params : (Ast.param list))) = _menhir_stack in
                let _v : (Ast.func_def) = 
# 38 "lib/parser.mly"
    ( { ret_type = Void; name; params; body } )
# 1023 "lib/parser.ml"
                 in
                _menhir_goto_func_def _menhir_env _menhir_stack _menhir_s _v
            | MenhirState101 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (body : (Ast.block)) = _v in
                let (((_menhir_stack, _menhir_s), (name : (
# 6 "lib/parser.mly"
       (string)
# 1033 "lib/parser.ml"
                ))), _, (params : (Ast.param list))) = _menhir_stack in
                let _v : (Ast.func_def) = 
# 36 "lib/parser.mly"
    ( { ret_type = Int; name; params; body } )
# 1038 "lib/parser.ml"
                 in
                _menhir_goto_func_def _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_primary_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Ast.expr)) = _v in
    let _v : (Ast.expr) = 
# 139 "lib/parser.mly"
                     ( e )
# 1060 "lib/parser.ml"
     in
    _menhir_goto_unary_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_unary_op : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.unary_op) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_reduce28 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "lib/parser.mly"
       (string)
# 1091 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (name : (
# 6 "lib/parser.mly"
       (string)
# 1097 "lib/parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expr) = 
# 151 "lib/parser.mly"
              ( Var name )
# 1102 "lib/parser.ml"
     in
    _menhir_goto_primary_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "lib/parser.mly"
       (string)
# 1109 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState18 in
        let _v : (Ast.expr list) = 
# 160 "lib/parser.mly"
              ( [] )
# 1133 "lib/parser.ml"
         in
        _menhir_goto_expr_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "lib/parser.mly"
       (string)
# 1144 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DIV | EQ | GEQ | GT | LEQ | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | RPAREN | SEMI ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | CONTINUE ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | INT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LBRACE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NOT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NUMBER _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | PLUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | RETURN ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SEMI ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | BREAK | CONTINUE | ID _ | IF | INT | LBRACE | LPAREN | MINUS | NOT | NUMBER _ | PLUS | RBRACE | RETURN | SEMI | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (cond : (Ast.expr))), _, (then_stmt : (Ast.stmt))) = _menhir_stack in
            let _v : (Ast.stmt) = 
# 70 "lib/parser.mly"
    ( IfStmt (cond, then_stmt, None) )
# 1214 "lib/parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (cond : (Ast.expr))), _, (then_stmt : (Ast.stmt))), _, (else_stmt : (Ast.stmt))) = _menhir_stack in
        let _v : (Ast.stmt) = 
# 72 "lib/parser.mly"
    ( IfStmt (cond, then_stmt, Some else_stmt) )
# 1230 "lib/parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (cond : (Ast.expr))), _, (stmt : (Ast.stmt))) = _menhir_stack in
        let _v : (Ast.stmt) = 
# 73 "lib/parser.mly"
                                                ( WhileStmt (cond, stmt) )
# 1240 "lib/parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BREAK ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | CONTINUE ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | ID _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LBRACE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NUMBER _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RETURN ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMI ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RBRACE ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | _ ->
        _menhir_fail ()

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.stmt list) = 
# 211 "<standard.mly>"
    ( [] )
# 1290 "lib/parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NUMBER _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.stmt) = 
# 65 "lib/parser.mly"
         ( EmptyStmt )
# 1335 "lib/parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState60 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.stmt) = 
# 76 "lib/parser.mly"
                ( ReturnStmt None )
# 1366 "lib/parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.unary_op) = 
# 144 "lib/parser.mly"
         ( Plus )
# 1381 "lib/parser.ml"
     in
    _menhir_goto_unary_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "lib/parser.mly"
       (int)
# 1388 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (num : (
# 7 "lib/parser.mly"
       (int)
# 1396 "lib/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 152 "lib/parser.mly"
                 ( Num num )
# 1401 "lib/parser.ml"
     in
    _menhir_goto_primary_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.unary_op) = 
# 146 "lib/parser.mly"
        ( Not )
# 1412 "lib/parser.ml"
     in
    _menhir_goto_unary_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.unary_op) = 
# 145 "lib/parser.mly"
          ( Minus )
# 1423 "lib/parser.ml"
     in
    _menhir_goto_unary_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NOT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NUMBER _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | PLUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NUMBER _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "lib/parser.mly"
       (string)
# 1533 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NUMBER _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | LPAREN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
    | AND | DIV | EQ | GEQ | GT | LEQ | LT | MINUS | MOD | MUL | NEQ | OR | PLUS | SEMI ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.stmt) = 
# 75 "lib/parser.mly"
                  ( ContinueStmt )
# 1586 "lib/parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.stmt) = 
# 74 "lib/parser.mly"
               ( BreakStmt )
# 1610 "lib/parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | CONTINUE ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LBRACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NUMBER _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | PLUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RETURN ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | SEMI ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RBRACE ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_param_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState99 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (params : (Ast.param list)) = _v in
        let _v : (Ast.param list) = 
# 42 "lib/parser.mly"
                                                 ( params )
# 1727 "lib/parser.ml"
         in
        _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.param list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.param))) = _menhir_stack in
        let _v : (Ast.param list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1738 "lib/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_func_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.comp_unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (funcs : (Ast.comp_unit))) = _menhir_stack in
            let _v : (Ast.comp_unit) = 
# 30 "lib/parser.mly"
                               ( funcs )
# 1760 "lib/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.comp_unit)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.func_def))), _, (xs : (Ast.comp_unit))) = _menhir_stack in
        let _v : (Ast.comp_unit) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1779 "lib/parser.ml"
         in
        _menhir_goto_list_func_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.param list) = 
# 43 "lib/parser.mly"
            ( [] )
# 1790 "lib/parser.ml"
     in
    _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (name : (
# 6 "lib/parser.mly"
       (string)
# 1807 "lib/parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.param) = 
# 47 "lib/parser.mly"
                  ( { typ = Int; name } )
# 1813 "lib/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.param))) = _menhir_stack in
            let _v : (Ast.param list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1837 "lib/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.comp_unit) = 
# 211 "<standard.mly>"
    ( [] )
# 1965 "lib/parser.ml"
     in
    _menhir_goto_list_func_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | RPAREN ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | RPAREN ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and comp_unit : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.comp_unit) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2083 "lib/parser.ml"
