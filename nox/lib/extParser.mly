%{
  open Core

  let exprId = ref 0
  let nxid () =
    let id = !exprId in
    incr exprId;
    id
  ;;
  let rsid() =
    exprId := 0
  ;;

  let location (x: Lexing.position) =
    let lineIdx = x.pos_lnum in
    let colIdx = x.pos_cnum - x.pos_bol in
    let loc = Ast.Location {lineIdx=lineIdx; colIdx=colIdx} in
    loc
  ;;

  let loc (loc: Lexing.position * Lexing.position) =
    let startpos, endloc = loc in
    location startpos
  ;;
%}

%token <string> INTVAL
%token <string> FLOATVAL
%token <string> IDVAL

%token AS AND ELSE EXPORT FALSE FLOAT FN IF INT LET MOD MUT NOT OR RETURN SET TRUE UNDEFINED USE
%token EQEQ NE EQ LE GE LT GT

%token SEMICOLON COLON COMMA AT DOTDOT DOT QUESTION TICK EXCLAMATION AMPERSAND HASH
%token PERCENT DOLLAR LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK LANGLE RANGLE
%token PLUS MINUS STAR SLASH CARET BAR

%token EOF

(* Lowest priority at the top *)
%nonassoc LE GE LT GT
%nonassoc EQEQ NE
%nonassoc NOT
%left PLUS MINUS
%left STAR SLASH
%left DOT
%nonassoc UPLUS UMINUS
%nonassoc TRY UCONREF UMUTREF UDEREF
(* Highest priority at the bottom *)

(* Start parsing from the file node *)
%start      file
%type       <Ast.file> file

%%

file:
    | EOF { Ast.File {entities=[]; file="I DON'T KNOW!"} }
    | u=use; e=nonempty_list(entity); EOF { Ast.File {entities=e; file="I DON'T HECCIN' KNOW!"} }

use:
    | s=scope; USE n=name; SEMICOLON { Ast.Use {name=n; scope=s; import=true; loc=(loc $loc)} }
    | s=scope; MOD n=name; SEMICOLON { Ast.Use {name=n; scope=s; import=false; loc=(loc $loc)} }

entity:
    | s=scope; FN n=name; LPAREN a=seplist(COMMA,args); RPAREN t=returnType; LBRACE bs=list(stmt); be=option(endExpr); RBRACE
    {
        let numExprs=nxid() in
        rsid();
        Ast.Function
        {
            name=n;
            type'=(Ast.FunctionType{args=(List.map (fun arg -> match arg with Ast.Variable a -> (a.type' |> xSOME uPOS ) ) a); type'=t});
            args=a;
            body=(Ast.Block {stmts=(match be with Some x -> bs @ [match x with Ast.SetStmt xx -> Ast.ReturnStmt {expr=xx.expr; loc=(loc $loc)} |_ -> failwith "wut?"] | None -> bs)});
            scope=s; loc=(loc $loc); numExprs
        }
    }

block:
    | LBRACE b=blockBody; RBRACE { b }
blockBody:
    | s=list(stmt); e=option(endExpr); { Ast.Block {stmts=(match e with Some x -> s @ [x] | None -> s)} }
endExpr:
    | COLON e=expr; { Ast.SetStmt {expr=e; loc=(loc $loc)} }

stmt:
    | LET v=seplist(COMMA,vars); EQ e=expr; SEMICOLON { Ast.LetStmt {vars=v; expr=e; loc=(loc $loc)} }
    | SET e=expr; SEMICOLON { Ast.SetStmt {expr=e; loc=(loc $loc)} }
    | v=seplist(COMMA,lvalExpr); EQ e=expr; SEMICOLON { Ast.AssignStmt {vars=v; expr=e; loc=(loc $loc)} }
    | RETURN e=expr; SEMICOLON { Ast.ReturnStmt {expr=e; loc=(loc $loc)} }
    | x=lvalExpr; SEMICOLON { Ast.InvokeStmt {expr=x; loc=(loc $loc)} }
    | b=block; { Ast.BlockStmt {block=b; loc=(loc $loc)} }
    | x=ifStmt; { x }

expr:
    | LPAREN x=expr; RPAREN { x }
    | x=postExpr; { x }
    | x=biopExpr; { x }
    | x=unopExpr; { x }

postExpr:
    | x=compExpr; { x }
    | x=postExpr; QUESTION %prec TRY { Ast.UnOpExpr {expr=x; op=Ast.TryOp; id=(nxid()); loc=(loc $loc)} }
    | x=postExpr; AMPERSAND %prec UDEREF { Ast.UnOpExpr {expr=x; op=Ast.DerefOp; id=(nxid()); loc=(loc $loc)} }

biopExpr:
    | x=expr; PLUS  y=expr; %prec PLUS  { Ast.BinOpExpr {lexpr=x; op=Ast.AddOp; rexpr=y; id=(nxid()); loc=(loc $loc)} }
    | x=expr; MINUS y=expr; %prec MINUS { Ast.BinOpExpr {lexpr=x; op=Ast.SubOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; STAR  y=expr; %prec STAR  { Ast.BinOpExpr {lexpr=x; op=Ast.MulOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; SLASH y=expr; %prec SLASH { Ast.BinOpExpr {lexpr=x; op=Ast.DivOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; DOT y=expr; %prec DOT { Ast.BinOpExpr {lexpr=x; op=Ast.DotOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=condExpr; { x }

unopExpr:
    | PLUS x=expr; %prec UPLUS { Ast.UnOpExpr {expr=x; op=Ast.PosOp; id=(nxid());loc=(loc $loc)} }
    | MINUS x=expr; %prec UMINUS { Ast.UnOpExpr {expr=x; op=Ast.NegOp; id=(nxid());loc=(loc $loc)} }
    | NOT x=expr; %prec NOT { Ast.UnOpExpr {expr=x; op=Ast.NotOp; id=(nxid());loc=(loc $loc)} }
    | AMPERSAND x=expr; %prec UCONREF { Ast.UnOpExpr {expr=x; op=Ast.ConRefOp; id=(nxid()); loc=(loc $loc)} }
    | MUT AMPERSAND x=expr; %prec UMUTREF { Ast.UnOpExpr {expr=x; op=Ast.MutRefOp;id=(nxid()); loc=(loc $loc)} }
    | STAR x=expr; %prec UMUTREF { Ast.UnOpExpr {expr=x; op=Ast.MutRefOp; id=(nxid()); loc=(loc $loc)} }

condExpr:
    | x=expr; EQEQ y=expr; %prec EQEQ { Ast.BinOpExpr {lexpr=x; op=Ast.EqOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; NE y=expr; %prec NE { Ast.BinOpExpr {lexpr=x; op=Ast.NeOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; LE y=expr; %prec LE { Ast.BinOpExpr {lexpr=x; op=Ast.LeOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; GE y=expr; %prec GE { Ast.BinOpExpr {lexpr=x; op=Ast.GeOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; LT y=expr; %prec LT { Ast.BinOpExpr {lexpr=x; op=Ast.LtOp; rexpr=y; id=(nxid());loc=(loc $loc)} }
    | x=expr; GT y=expr; %prec GT { Ast.BinOpExpr {lexpr=x; op=Ast.GtOp; rexpr=y; id=(nxid());loc=(loc $loc)} }

compExpr:
    | n=name; LPAREN a=seplist(COMMA,expr); RPAREN { Ast.InvokeExpr {name=n; args=a; id=(nxid()); loc=(loc $loc)} }
    | b=block; { Ast.BlockExpr {block=b; id=(nxid()); loc=(loc $loc)} }
    | e=ifExpr; { e }
    | e=termExpr; { e }

termExpr:
    | LPAREN x=septuple(COMMA,expr); RPAREN { Ast.TupleVal {value=x; id=(nxid()); loc=(loc $loc)} }
    | x=name; { Ast.IdVal {name=x; id=(nxid()); loc=(loc $loc)} }
    | x=FLOATVAL; { Ast.FloatVal {value=x; id=(nxid()); loc=(loc $loc)} }
    | x=INTVAL; { Ast.IntVal {value=x; id=(nxid()); loc=(loc $loc)} }
    | x=bools; { Ast.BoolVal {value=x; id=(nxid()); loc=(loc $loc)} }
    | UNDEFINED { Ast.UndefinedVal {id=(nxid()); loc=(loc $loc)} }
    | LPAREN RPAREN { Ast.UnitVal {id=(nxid()); loc=(loc $loc)} }

ifStmt:
    | IF c=condExpr; b=block; o=option(elseBranch); { Ast.IfStmt {expr=Ast.IfExpr { cond=c; block=b; rest=o; id=(nxid()); loc=(loc $loc)}; loc=(loc $loc)} }
ifExpr:
    | IF c=condExpr; b=block; o=elseBranch; { Ast.IfExpr {cond=c; block=b; rest=(Some o); id=(nxid()); loc=(loc $loc)} }

elseIfExpr:
    | ELSE IF c=expr; b=block; o=option(elseBranch); { Ast.ElseIfExpr {cond=c; block=b; rest=o; id=(nxid()); loc=(loc $loc)} }
elseExpr:
    | ELSE b=block; { Ast.ElseExpr {block=b; id=(nxid()); loc=(loc $loc)} }
elseBranch:
    | x=elseIfExpr; { x }
    | x=elseExpr; { x }

lvalExpr:
    | x=lvalPostExpr; { x }
    | x=lvalBiopExpr; { x }

lvalPostExpr:
    | x=lvalComp; { x }
    | x=lvalPostExpr; QUESTION %prec TRY { Ast.UnOpExpr {expr=x; op=Ast.TryOp; id=(nxid()); loc=(loc $loc)} }
    | x=lvalPostExpr; AMPERSAND %prec UDEREF { Ast.UnOpExpr {expr=x; op=Ast.DerefOp; id=(nxid()); loc=(loc $loc)} }

lvalComp:
    | n=name; LPAREN a=seplist(COMMA,expr); RPAREN { Ast.InvokeExpr {name=n; args=a; id=(nxid()); loc=(loc $loc) } }
    | n=name; { Ast.IdVal {name=n; id=(nxid()); loc=(loc $loc)} }

lvalBiopExpr:
    | l=lvalExpr; DOT r=lvalExpr; %prec DOT { Ast.BinOpExpr {lexpr=l; op=Ast.DotOp; rexpr=r; id=(nxid()); loc=(loc $loc) } }

bools:
    | TRUE { true }
    | FALSE { false }

args:
    | s=states; h=shadow; n=name; t=types; { Ast.Variable {name=n; state=s; shadow=h; type'=(Some t)} }

vars:
    | s=states; h=shadow; n=name; t=option(types); { Ast.Variable {name=n; state=s; shadow=h; type'=t;} }

returnType:
    | { Ast.UnitType }
    | t=types; { t }

types:
    | LPAREN RPAREN { Ast.UnitType }
    | INT { Ast.IntType }
    | FLOAT { Ast.FloatType }
    | LPAREN t=septuple(COMMA,types); RPAREN { Ast.TupleType { types=t; } }
    | AMPERSAND t=types; { Ast.ConRefType { life=None; types=t; }  }
    | STAR t=types; { Ast.MutRefType { life=None; types=t; }  }

states:
    | { Ast.ImmutableState }
    | MUT { Ast.MutableState }

scope:
    | { Ast.PrivateScope }
    | EXPORT { Ast.PublicScope }

name:
    | id=IDVAL; { Ast.Name { name=id; loc=(loc $loc)}  }

shadow:
    | { false }
    | PLUS { true }

seplist(SEP, NODE):
    | { [] }
    | x=NODE; { [x] }
    | x=NODE; SEP y=seplist(SEP,NODE); { x::y }

septuple(SEP, NODE):
    | x=NODE; SEP y=NODE; { x::[y] }
    | x=NODE; SEP y=septuple(SEP,NODE); { x::y }
