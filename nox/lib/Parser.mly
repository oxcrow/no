%{
    let nextEntyId = ref 0
    let nextStmtId = ref 0
    let nextExprId = ref 0

    let entyId () =
        let id = !nextEntyId in
        nextEntyId := !nextEntyId + 1;
        id
    ;;

    let stmtId () =
        let id = !nextStmtId in
        nextStmtId := !nextStmtId + 1;
        id
    ;;

    let exprId () =
        let id = !nextExprId in
        nextExprId := !nextExprId + 1;
        id
    ;;

    let nullUuid () : Ast.uuids =
        let modId = 0 in
        let entyId = !nextEntyId - 1 in
        let fileId = -1 in
        let varId = -1 in
        let elemId = 0 in
        {modId; entyId; fileId; varId; elemId}
    ;;

    let location (x: Lexing.position) =
        let lineIndex = x.pos_lnum in
        let colIndex = x.pos_cnum - x.pos_bol + 1 in
        let loc = Ast.Location {lineIndex; colIndex} in
        loc
    ;;

    let loc (loc: Lexing.position * Lexing.position) =
        let startpos, endloc = loc in
        location startpos
    ;;

%}

%token <string> XINT
%token <string> XFLOAT
%token <string> XNAME

%token AS AND ELSE ENUM EXPORT FALSE FLOAT FN IF INT LET LOCAL MACRO MOD MUT NOT OR RETURN STRUCT TRUE TYPE UNDEFINED USE YIELD
%token EQEQ NOTEQ EQ LTEQ GTEQ LT GT

%token SEMICOLON COLON COMMA AT DOTDOT DOT QUESTION TICK EXCLAMATION AMPERSAND HASH
%token PERCENT DOLLAR LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK LANGLE RANGLE
%token PLUS MINUS STAR SLASH CARET BAR

%token EOF

(* Lowest priority at the top *)
%nonassoc LTEQ GTEQ LT GT
%nonassoc EQEQ NOTEQ
%left NOT
%left PLUS MINUS
%left STAR SLASH
%left UPLUS UMINUS
%left DEREF
%left CONREF MUTREF
%left TRY UNWRAP
%left SLICE DOT
(* Highest priority at the bottom *)

(* Start parsing from the file node *)
%start      file
%type       <Ast.file> file

%%

file:
    | EOF { Ast.File {modules=[]; entities=[]; file="I DON'T KNOW!"} }
    | m=list(modules); e=nonempty_list(entities); EOF { Ast.File {modules=m; entities=e; file="I DON'T KNOW!"} }

modules:
    | MOD n=names; SEMICOLON {
        Ast.Mod {name=n; loc=(loc $loc)}
    }

entities:
    | f=functions; { f }

functions:
    | s=scopes; FN n=names; LPAREN RPAREN t=returnTypes; LBRACE stmts=list(stmts); e=option(endExprs); RBRACE {
        let b = match e with
            | Some e -> stmts @ [Ast.ReturnStmt{expr=e; stmtId=(stmtId()); loc=(loc $loc)}]
            | None -> (
                let endUnitExpr () =
                    stmts @ [Ast.ReturnStmt {
                        expr=(Ast.UnitExpr{types=Ast.UnitType; exprId=(exprId()); loc=(loc $loc)}); stmtId=(stmtId()); loc=(loc $loc)
                    }]
                in
                match (Core.lastOfList stmts) with
                | Some s -> (
                    match s with
                    | Ast.ReturnStmt _ -> stmts
                    | _ -> endUnitExpr()
                )
                | None -> endUnitExpr())
        in
        Ast.Function {scope=s; name=n; args=[]; block=(Ast.Block{stmts=b; types=Ast.TodoType}); types=t; entyId=(entyId()); loc=(loc $loc)}
    }

blockStmts:
    | s=list(stmts); e=option(endExprs) {
        (match e with Some e -> s @ [Ast.YieldStmt {expr=e; stmtId=(stmtId()); loc=(loc $loc)}] | None -> s)
    }

endExprs:
    | COLON e=exprs; {
        e
    }

stmts:
    | RETURN e=exprs; SEMICOLON {
        Ast.ReturnStmt {expr=e; stmtId=(stmtId()); loc=(loc $loc)}
    }

exprs:
    | e=compExprs; { e }

compExprs:
    | e=termExprs; { e }

termExprs:
    | e=names; {
        Ast.NameExpr {value=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=XINT; {
        Ast.IntExpr {value=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | LPAREN RPAREN {
        Ast.UnitExpr {types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

returnTypes:
    | { Ast.UnitType }
    | t=types; { t }

types:
    | INT {
        Ast.IntType
    }
    | LPAREN RPAREN {
        Ast.UnitType
    }


states:
    | MUT { Ast.MutState }
    | { Ast.ConState }

scopes:
    | EXPORT { Ast.ExportScope }
    | LOCAL { Ast.LocalScope }
    | { Ast.ModuleScope }

names:
    | n=XNAME; {
        let id = Store.internString n in
        Ast.Name {name=n; nameKind=Ast.IdentifierName; nameId=id; uuid=(nullUuid()); loc=(loc $loc)}
    }
    | AT x=XNAME; {
        let n = "@" ^ x in
        let id = Store.internString n in
        Ast.Name {name=n; nameKind=Ast.ReservedName; nameId=id; uuid=(nullUuid()); loc=(loc $loc)}
    }

(* Collect a tuple of NODEs seperated by SEP delimiter *)
septuple(SEP, NODE):
    | x=NODE; SEP y=NODE; { x::[y] }
    | x=NODE; SEP y=NODE; SEP { x::[y] }
    | x=NODE; SEP y=septuple(SEP,NODE); { x::y }

(* Collect a list of NODEs seperated by SEP delimiter *)
(* Since an empty list is allowed in y=seplist(SEP,NODE); we can parse trailing delimiters. *)
seplist(SEP, NODE):
    | { [] }
    | x=NODE; { [x] }
    | x=NODE; SEP y=seplist(SEP,NODE); { x::y }

(* Collect a non-empty list of NODEs separated by SEP delimiter *)
nonempty_seplist(SEP, NODE):
    | x=NODE; { [x] }
    | x=NODE; SEP y=nonempty_seplist(SEP,NODE); { x::y }
