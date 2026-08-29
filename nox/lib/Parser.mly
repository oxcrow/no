%{
    let nextStmtId = ref 0
    let nextExprId = ref 0

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

%token AS AND ELSE ENUM EXPORT FALSE FLOAT FN IF INT LATER LET LOCAL MACRO MOD MUT NEVER NONE NOT OK OR RETURN SOME STRUCT TODO TRUE TYPE UNDEFINED USE YIELD
%token EQEQ NOTEQ EQ LTEQ GTEQ LT GT

%token SEMICOLON COLON COMMA AT DOTDOT DOT QUESTION TICK EXCLAMATION AMPERSAND HASH
%token PERCENT DOLLAR LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK LANGLE RANGLE
%token PLUS MINUS STAR SLASH BACKSLASH CARET BAR

%token EOF

(* Lowest priority at the top *)
%nonassoc LTEQ GTEQ LT GT
%nonassoc EQEQ NOTEQ
%left NOT
%left PLUS MINUS
%left STAR SLASH
%left UPLUS UMINUS
%left CONREF MUTREF DEREF
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
    | s=scopes; STRUCT n=names; LBRACE m=seplist(COMMA,args); e=list(functions); RBRACE {
        Ast.Struct {scope=s; name=n; elems=m; entys=e; loc=(loc $loc)}
    }
    | f=functions; { f }

functions:
    | s=scopes; FN n=names; LPAREN a=seplist(COMMA,args); RPAREN t=returnTypes; b=blocks; {
        Ast.Function {scope=s; name=n; block=b; loc=(loc $loc)}
    }

blocks:
    | LBRACE s=list(stmts); e=option(endExprs) RBRACE {
        (match e with Some e -> s @ [e] | None -> s)
    }

endExprs:
    | COLON e=exprs; {
        Ast.YieldStmt {expr=e; stmtId=(stmtId()); loc=(loc $loc)}
    }

stmts:
    | LET v=nonempty_seplist(COMMA,pats); EQ e=exprs; SEMICOLON {
        Ast.LetStmt {expr=e; vars=v; stmtId=(stmtId()); loc=(loc $loc)}
    }
    | RETURN e=exprs; SEMICOLON {
        Ast.ReturnStmt {expr=e; stmtId=(stmtId()); loc=(loc $loc)}
    }
    | v=seplist(COMMA,lvalExprs); EQ e=exprs; SEMICOLON {
        Ast.AssignStmt {vars=v; expr=e; stmtId=(stmtId()); loc=(loc $loc)}
    }
    | e=lvalExprs; SEMICOLON { Ast.CallStmt {expr=e; stmtId=(stmtId()); loc=(loc $loc)} }
    | s=ifStmts; { s }

exprs:
    | DOT n=names; {
        Ast.GhostExpr {expr=NameExpr{value=n; exprId=(exprId());  types=Ast.TodoType; loc=(loc $loc)}; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | OK LPAREN e=exprs; RPAREN {
        Ast.ResultExpr {expr=Some(e); types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | SOME LPAREN e=exprs; RPAREN {
        Ast.OptionExpr {expr=Some(e); types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | LPAREN e=exprs; RPAREN { e }
    | e=biopExprs; { e }
    | e=condExprs; { e }
    | e=postExprs; { e }
    | e=compExprs; { e }
    | e=ifExprs; { e }

lvalExprs:
    | n=names; LPAREN a=seplist(COMMA,exprs); RPAREN {
        Ast.CallExpr {name=n; args=a; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | n=names; {
        Ast.NameExpr {value=n; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=lvalPostExprs; { e }

lvalPostExprs:
    | x=lvalExprs; DOT y=lvalExprs; %prec DOT {
        Ast.DotExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=lvalExprs; DOT QUESTION %prec TRY {
        Ast.TryExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=lvalExprs; DOT EXCLAMATION %prec UNWRAP {
        Ast.UnwrapExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=lvalExprs; DOT HASH %prec DEREF {
        Ast.DerefExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

postExprs:
    | x=exprs; DOT y=exprs; %prec DOT {
        Ast.DotExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT QUESTION %prec TRY {
        Ast.TryExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT EXCLAMATION %prec UNWRAP {
        Ast.UnwrapExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=exprs; DOT STAR %prec DEREF {
        Ast.DerefExpr {expr=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
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
    | AMPERSAND x=exprs; %prec CONREF {
        Ast.ConRefExpr{expr=x; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

condExprs:
    | x=exprs; NOTEQ y=exprs; %prec NOTEQ {
        Ast.NotEqExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; EQEQ y=exprs; %prec EQEQ {
        Ast.EqEqExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; LTEQ y=exprs; %prec LTEQ {
        Ast.LtEqExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; GTEQ y=exprs; %prec GTEQ {
        Ast.GtEqExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; LT y=exprs; %prec LT {
        Ast.LtExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | x=exprs; GT y=exprs; %prec GT {
        Ast.GtExpr {lexpr=x; rexpr=y; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

compExprs:
    | LPAREN e=septuple(COMMA,exprs); RPAREN {
        Ast.TupleExpr {exprs=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | n=names; LBRACE e=seplist(COMMA,elemExprs); RBRACE {
        Ast.StructExpr {name=Some(n); elems=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=groupExprs; LBRACK a=seplist(COMMA,exprs); RBRACK %prec SLICE {
        Ast.SliceExpr {expr=e; args=a; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | DOT LBRACE e=seplist(COMMA,elemExprs); RBRACE {
        Ast.StructExpr {name=None; elems=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | BACKSLASH t=types; {
        Ast.TypeExpr {types=t; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=groupExprs; { e }
    | e=termExprs; { e }

groupExprs:
    | LBRACK e=seplist(COMMA,exprs); RBRACK {
        Ast.ArrayExpr {exprs=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | n=names; LPAREN a=seplist(COMMA,exprs); RPAREN {
        Ast.CallExpr {name=n; args=a; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | e=names; {
        Ast.NameExpr {value=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

termExprs:
    | e=XINT; {
        Ast.IntExpr {value=e; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
    | LATER {
        Ast.LaterExpr {exprId=(exprId()); loc=(loc $loc)}
    }

elemExprs:
    | DOT n=names; EQ e=exprs; {
        Ast.Elem {name=n; expr=e; loc=(loc $loc)}
    }

ifStmts:
    | IF LPAREN c=condExprs; RPAREN b=blocks; o=option(elseBranch); {
        Ast.IfStmt {expr=Ast.IfExpr {cond=c; block=b; rest=o; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}; stmtId=(stmtId()); loc=(loc $loc)}
    }
ifExprs:
    | IF LPAREN c=condExprs; RPAREN b=blocks; o=elseBranch; {
        Ast.IfExpr {cond=c; block=b; rest=(Some o); types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }

elseIfExprs:
    | ELSE IF LPAREN c=exprs; RPAREN b=blocks; o=option(elseBranch); {
        Ast.ElseIfExpr {cond=c; block=b; rest=o; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
elseExprs:
    | ELSE b=blocks; {
        Ast.ElseExpr {block=b; types=Ast.TodoType; exprId=(exprId()); loc=(loc $loc)}
    }
elseBranch:
    | x=elseIfExprs; { x }
    | x=elseExprs; { x }

pats:
    | LPAREN p=nonempty_seplist(COMMA,pats); RPAREN {
        Ast.TuplePattern {pats=p}
    }
    | LBRACK p=nonempty_seplist(COMMA,pats); RBRACK {
        Ast.ArrayPattern {pats=p}
    }
    | v=vars; {
        Ast.LonePattern {var=v}
    }

vars:
    | s=states; n=names; t=option(types); {
        Ast.Var {state=s; name=n; type'=(match t with Some t -> t | None -> Ast.NoneType); varId=0}
    }

args:
    | n=names; t=types; {
        Ast.Var {state=Ast.ConState; name=n; type'=t; varId=0}
    }

returnTypes:
    | { Ast.UnitType }
    | t=types; { t }

types:
    | STRUCT LBRACE m=seplist(COMMA,args); RBRACE {
        Ast.StructType {
            types = Ast.Struct {scope=Ast.ModuleScope; name=(Ast.Name{name=""; nameId=0; loc=(loc $loc)}); elems=m; entys=[]; loc=(loc $loc)};
            offsets=[];
            align=0;
            size=0
        }
    }
    | EXCLAMATION t=types {
        Ast.ResultType {types=t; align=0; size=0}
    }
    | QUESTION t=types {
        Ast.OptionType {types=t; align=0; size=0}
    }
    | AMPERSAND t=types {
        Ast.ConRefType {types=t}
    }
    | n=names {
        Ast.NameType {name=n}
    }
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
        let id = Store.Module.internString n in
        Ast.Name {name=n; nameId=id; loc=(loc $loc)}
    }
    | AT n=XNAME; {
        let id = Store.Module.internString n in
        Ast.Name {name=("@" ^ n); nameId=id; loc=(loc $loc)}
    }

septuple(SEP, NODE):
    | x=NODE; SEP y=NODE; { x::[y] }
    | x=NODE; SEP y=septuple(SEP,NODE); { x::y }

seplist(SEP, NODE):
    | { [] }
    | x=NODE; { [x] }
    | x=NODE; SEP y=seplist(SEP,NODE); { x::y }

nonempty_seplist(SEP, NODE):
    | x=NODE; { [x] }
    | x=NODE; SEP y=nonempty_seplist(SEP,NODE); { x::y }
