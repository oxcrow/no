%{
    let nextEntyId = ref 0
    let nextStmtId = ref 0
    let nextExprId = ref 0
    let nextVarId = ref 0

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

    let varId () =
        let id = !nextVarId in
        nextVarId := !nextVarId + 1;
        id
    ;;

    let uuid () : Ast.uuids =
        let modId = 0 in
        let entyId = !nextEntyId - 1 in
        let varId = varId () in
        let elemId = 0 in
        {modId; entyId; varId; elemId}
    ;;

    let nullUuid () : Ast.uuids =
        let modId = 0 in
        let entyId = !nextEntyId - 1 in
        let varId = -1 in
        let elemId = 0 in
        {modId; entyId; varId; elemId}
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
%left UNWRAP TRY MOVE COPY
%left DOT
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
                        expr=(Ast.UnitExpr{exprId=(exprId()); loc=(loc $loc)}); stmtId=(stmtId()); loc=(loc $loc)
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
        let numExprs = !nextExprId in
        nextExprId := 0;
        nextStmtId := 0;
        nextVarId := 0;
        Ast.Function {
            scope=s; name=n; args=[];
            block=(Ast.Block{stmts=b; types=Ast.TodoType});
            types=t; numExprs; entyId=(entyId()); loc=(loc $loc)
        }
    }

blocks:
    | LBRACE s=blockStmts; RBRACE {
        Ast.Block {stmts=s; types=Ast.TodoType}
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
    | LET p=nonempty_seplist(COMMA,pats); EQ e=exprs; SEMICOLON {
        Ast.LetStmt {expr=e; vars=[]; pats=p; stmtId=(stmtId()); loc=(loc $loc)}
    }
    | RETURN e=exprs; SEMICOLON {
        Ast.ReturnStmt {expr=e; stmtId=(stmtId()); loc=(loc $loc)}
    }

exprs:
    | LPAREN e=exprs; RPAREN { e }
    | e=postExprs; { e }
    | e=biopExprs; { e }
    | e=condExprs; { e }
    | e=compExprs; { e }
    | e=ifExprs; { e }

postExprs:
    | x=exprs; DOT y=exprs; %prec DOT {
        Ast.DotExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT EXCLAMATION %prec UNWRAP {
        Ast.UnwrapExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT QUESTION %prec TRY {
        Ast.TryExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT STAR %prec MOVE {
        Ast.MoveExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT PLUS %prec COPY {
        Ast.CopyExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

biopExprs:
    | x=exprs; PLUS  y=exprs; %prec PLUS  {
        Ast.AddExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; MINUS y=exprs; %prec MINUS {
        Ast.SubExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; STAR  y=exprs; %prec STAR  {
        Ast.MulExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; SLASH y=exprs; %prec SLASH {
        Ast.DivExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

condExprs:
    | x=boolExprs; {
        x
    }

compExprs:
    | LBRACE b=blockStmts; RBRACE {
        Ast.BlockExpr {block=(Ast.Block{stmts=b; types=Ast.TodoType}); types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=groupExprs; { e }
    | e=termExprs; { e }

groupExprs:
    | LPAREN e=septuple(COMMA,exprs); RPAREN {
        Ast.TupleExpr {exprs=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | LBRACK e=nonempty_seplist(COMMA,exprs); RBRACK {
        Ast.ArrayExpr {exprs=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

termExprs:
    | e=names; {
        Ast.NameExpr {value=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=XINT; {
        Ast.IntExpr {value=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | LPAREN RPAREN {
        Ast.UnitExpr {exprId=(exprId()); loc=(loc $loc)}
    }

ifStmts:
    | IF c=condExprs; b=blocks; o=option(elseBranch); {
        Ast.IfStmt {expr=Ast.IfExpr {cond=c; block=b; rest=o; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}; stmtId=(stmtId()); loc=(loc $loc)}
    }
ifExprs:
    | IF c=condExprs; b=blocks; o=elseBranch; {
        Ast.IfExpr {cond=c; block=b; rest=(Some o); types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

elseIfExprs:
    | ELSE IF c=exprs; b=blocks; o=option(elseBranch); {
        Ast.ElseIfExpr {cond=c; block=b; rest=o; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
elseExprs:
    | ELSE b=blocks; {
        Ast.ElseExpr {block=b; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
elseBranch:
    | x=elseIfExprs; { x }
    | x=elseExprs; { x }

boolExprs:
    | TRUE {
        Ast.BoolExpr{value=Ast.True; exprId=(exprId()); loc=(loc $loc)}
    }
    | FALSE {
        Ast.BoolExpr{value=Ast.False; exprId=(exprId()); loc=(loc $loc)}
    }

pats:
    | LPAREN p=nonempty_seplist(COMMA,pats); RPAREN t=option(types); {
        Ast.TuplePattern {pats=p; types=t}
    }
    | LBRACK p=nonempty_seplist(COMMA,pats); RBRACK t=option(types); {
        Ast.ArrayPattern {pats=p; types=t}
    }
    | v=vars; {
        Ast.LonePattern {var=v}
    }

vars:
    | s=states; n=names; t=option(types); {
        Ast.Var {
            state=s; name=n; exprId=(-1);
            type'=(match t with Some t -> t | None -> Ast.TodoType);
            uuid=(uuid ()); loc=(loc $loc)
        }
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
