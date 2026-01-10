%{
  let location (x: Lexing.position) =
    let lnum = x.pos_lnum in
    let cnum = x.pos_cnum - x.pos_bol in
    let loc = Ast.Loc{lnum=lnum; cnum=cnum} in
    loc
  ;;

  let position (startpos: Lexing.position) (endpos: Lexing.position) =
    let start = location startpos in
    let end' = location endpos in
    let pos = Ast.Pos{start=start; end'=end'} in
    pos
  ;;

  let pos (loc: Lexing.position * Lexing.position) =
    let startpos, endpos = loc in
    position startpos endpos
  ;;
%}

%token <int> INTVAL
%token <float> FLOATVAL
%token <string> IDVAL

%token AS
%token AND
%token CON
%token ELSE
%token FALSE
%token FLOAT
%token FN
%token IF
%token INT
%token LET
%token MUT
%token NOT
%token OR
%token RETURN
%token SET
%token TRUE
%token UNDEFINED

%token EQEQ
%token EQ
%token NE
%token LE
%token GE
%token LT
%token GT

%token SEMICOLON
%token COLON
%token COMMA
%token AT
%token DOTDOT
%token DOT
%token QUESTION
%token TICK
%token EXCLAMATION
%token AMPERSAND
%token HASH
%token TILDE
%token PERCENT
%token DOLLAR
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LANGLE
%token RANGLE
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token CARET
%token BAR

%token EOF

(* Lowest priority at the top *)
%nonassoc LE GE LT GT
%nonassoc EQ NE
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
    | EOF {
        Ast.File {entities = []; file = "I DON'T KNOW!"} }
    | e = nonempty_list(entities) EOF {
        Ast.File {entities = e; file = "I DON'T HECCIN' KNOW!"} }

entities:
    | FN n=id; LPAREN a=seplist(COMMA,args); RPAREN t=rtypes; b=blocks; {
        Ast.Function {name=n; args=a; type'=t; block=b; pos=(pos $loc)} }

blocks:
    | LBRACE s=list(stmts); RBRACE {
        Ast.Block {stmts=s; pos=(pos $loc)} }

stmts:
    | LET v=seplist(COMMA,vars); EQ e=exprs; SEMICOLON {
        Ast.LetStmt {vars=v; expr=e; pos=(pos $loc)} }
    | SET e=exprs; SEMICOLON {
        Ast.SetStmt {expr=e; pos=(pos $loc)} }
    | v=seplist(COMMA,primary); EQ e=exprs; SEMICOLON {
        Ast.AssignStmt {vars=v; expr=e; pos=(pos $loc)} }
    | RETURN e=exprs; SEMICOLON {
        Ast.ReturnStmt {expr=e; pos=(pos $loc)} }
    | x=postfix; SEMICOLON {
        Ast.InvokeStmt {expr=x; pos=(pos $loc)} }
    | b=blocks; {
        Ast.BlockStmt {block=b; pos=(pos $loc)} }
    | x=ifstmts; { x }

exprs:
    | LPAREN x=exprs; RPAREN { x }
    | x=postfix; { x }
    | x=biops; { x }
    | x=unops; { x }

postfix:
    | x=compound; { x }
    | x=postfix; QUESTION %prec TRY {
        Ast.UnOpExpr {value=x; op=Ast.TryOp; type'=Ast.NoneType; pos=(pos $loc)} }

biops:
    | x=exprs; PLUS  y=exprs; %prec PLUS  {
        Ast.BinOpExpr {lvalue=x; op=Ast.AddOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; MINUS y=exprs; %prec MINUS {
        Ast.BinOpExpr {lvalue=x; op=Ast.SubOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; STAR  y=exprs; %prec STAR  {
        Ast.BinOpExpr {lvalue=x; op=Ast.MulOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; SLASH y=exprs; %prec SLASH {
        Ast.BinOpExpr {lvalue=x; op=Ast.DivOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; DOT y=exprs; %prec DOT {
        Ast.BinOpExpr {lvalue=x; op=Ast.DotOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=conds; { x }

unops:
    | PLUS x=exprs; %prec UPLUS {
         Ast.UnOpExpr {value=x; op=Ast.PosOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | MINUS x=exprs; %prec UMINUS {
        Ast.UnOpExpr {value=x; op=Ast.NegOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | NOT x=exprs; %prec NOT {
        Ast.UnOpExpr {value=x; op=Ast.NotOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | AMPERSAND x=exprs; %prec UCONREF {
        Ast.UnOpExpr {value=x; op=Ast.ConRefOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | MUT AMPERSAND x=exprs; %prec UMUTREF {
        Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | STAR x=exprs; %prec UMUTREF {
        Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | EXCLAMATION x=exprs; %prec UDEREF {
        Ast.UnOpExpr {value=x; op=Ast.DerefOp; type'=Ast.NoneType; pos=(pos $loc)} }

conds:
    | x=exprs; EQ y=exprs; %prec EQ {
        Ast.BinOpExpr {lvalue=x; op=Ast.EqOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; NE y=exprs; %prec NE {
        Ast.BinOpExpr {lvalue=x; op=Ast.NeOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; LE y=exprs; %prec LE {
        Ast.BinOpExpr {lvalue=x; op=Ast.LeOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; GE y=exprs; %prec GE {
        Ast.BinOpExpr {lvalue=x; op=Ast.GeOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; LT y=exprs; %prec LT {
        Ast.BinOpExpr {lvalue=x; op=Ast.LtOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=exprs; GT y=exprs; %prec GT {
        Ast.BinOpExpr {lvalue=x; op=Ast.GtOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }

compound:
    | x=primary; { x }
    | AT x=ifexprs; { x }
    | AT x=blocks; {
        Ast.BlockExpr {block=x; type'=Ast.NoneType; pos=(pos $loc)} }

primary:
    | x=terms; {
        Ast.TerminalExpr {value=x; type'=Ast.NoneType; pos=(pos $loc)} }
    | i=id; LPAREN a=seplist(COMMA,exprs); RPAREN {
        Ast.InvokeExpr {value=i; args=a; type'=Ast.NoneType; pos=(pos $loc)} }

ifstmts:
    | IF c=conds; b=blocks; o=option(elsebranch); {
        Ast.IfStmt {expr=Ast.IfExpr {
            cond=c; block=b; other=o; is_stmt=true; type'=Ast.NoneType; pos=(pos $loc)}; pos=(pos $loc)} }
ifexprs:
    | IF c=conds; b=blocks; o=option(elsebranch); {
        Ast.IfExpr {cond=c; block=b; other=o; is_stmt=false; type'=Ast.NoneType; pos=(pos $loc)} }
elseifexprs:
    | ELSE IF c=exprs; b=blocks; o=option(elsebranch); {
        Ast.ElseIfExpr {cond=c; block=b; other=o; type'=Ast.NoneType; pos=(pos $loc)} }
elseexprs:
    | ELSE b=blocks; {
        Ast.ElseExpr {block=b; type'=Ast.NoneType; pos=(pos $loc)} }
elsebranch:
    | x=elseifexprs; { x }
    | x=elseexprs; { x }

terms:
    | LPAREN RPAREN { Ast.UnitVal }
    | UNDEFINED { Ast.UndefinedVal }
    | x=INTVAL; { Ast.IntVal {value=x} }
    | x=FLOATVAL; { Ast.FloatVal {value=x} }
    | x=id; { Ast.IdVal {value=x} }
    | AT LPAREN x=septuple(COMMA,exprs); RPAREN { Ast.TupleVal {value=x} }
    | x=bools; { Ast.BoolVal {value=x} }

bools:
    | TRUE { true }
    | FALSE { false }

args:
    | s=states; h=shadow; n=id; t=types; {
        Ast.Var {name=n; state=s; shadow=h; type'=t} }

vars:
    | s=states; h=shadow; n=id; t=option(types); {
        Ast.Var {
            name=n; state=s; shadow=h;
            type'=(match t with | Some x -> x | None -> Ast.NoneType )} }

rtypes:
    | { Ast.UnitType }
    | t=types; { t }

types:
    | LPAREN RPAREN { Ast.UnitType }
    | INT { Ast.IntType }
    | FLOAT { Ast.FloatType }
    | LPAREN t=septuple(COMMA,types); RPAREN { Ast.TupleType { types=t; } }

states:
    | { Ast.ConState }
    | CON { Ast.ConState }
    | MUT { Ast.MutState }
    | SET { Ast.SetState }

id:
    | i=IDVAL; { Ast.Id {value=i; pos=(pos $loc)} }

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
