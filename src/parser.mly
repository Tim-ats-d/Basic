%{
  open Ast

  let max_line = ref 0

  let update_line n =
    if n <= 0 || n > 99999 then raise ErrKind.(Parse_error IllegalLineNumber)
    else if n > !max_line then max_line := n
%}

%token LET READ DATA PRINT GO IF THEN FOR TO STEP NEXT END STOP DEF FN GOSUB RETURN DIM
%token PLUS MINUS TIMES DIV POWER
%token EQ NEQ LT LTE GT GTE
%token LPARENT RPARENT
%token COMMA NEWLINE EOF

%token <string> BUILTIN
%token <string> LETTER
%token <string> VAR
%token <Buffer.t> LABEL
%token <float> FLOAT
%token <int> INT

%start prgrm

%type <Ast.parsed_ast> prgrm

%%

prgrm:
  | body=line* EOF { { body; max_line = !max_line } }

line:
  | n=INT s=stmt NEWLINE { update_line n; (n, s) }

stmt:
  | s=assign
  | s=read
  | s=data
  | s=print
  | s=goto
  | s=if_then
  | s=for_
  | s=next
  | s=stop
  | s=def
  | s=gosub
  | s=return  { s }
  | END       { End }

assign:
  | LET name=ident EQ expr=expr { Let { name; expr } }

var:
  | s=subscript { s }
  | i=LETTER
  | i=VAR       { Ident i }

subscript:
  | name=VAR LPARENT args=separated_nonempty_list(COMMA, expr) RPARENT
    { SubScript { name; args } }

ident:
  | i=LETTER
  | i=VAR    { i }

expr:
  | PLUS e=eb
  | MINUS e=eb
  | e=eb                         { e }
  | left=eb op=op right=eb       { BinOp { left; op; right } }
  | PLUS left=eb op=op right=eb  { BinOp { left = UnaryOp { op = UPlus; operand = left }; op; right } }
  | MINUS left=eb op=op right=eb { BinOp { left = UnaryOp { op = Invert; operand = left }; op; right } }

eb:
  | LPARENT e=expr RPARENT { e }
  | f=fun_call             { f }
  | n=num                  { n }
  | v=var                  { v }

num:
  | i=INT   { Int i }
  | f=FLOAT { Float f }

op:
  | PLUS  { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV   { Div }
  | POWER { Power }

fun_call:
  | FN name=LETTER LPARENT arg=expr RPARENT { FunCall { name; arg } }
  | name=BUILTIN LPARENT arg=expr RPARENT   { FunCall { name; arg } }

read:
  | READ vars=separated_nonempty_list(COMMA, var) { Read vars }

data:
  | DATA numbers=separated_nonempty_list(COMMA, snum) { Data numbers }

print:
  | PRINT items=separated_list(COMMA, pitem) { Print items }

pitem:
  | e=expr { e }
  | l=LABEL { Label l }
  | label=LABEL expr=expr { LabelConcat { label; expr} }

goto:
  | GO TO i=INT { GoTo i }

if_then:
  | IF left=expr op=comp_op right=expr THEN line=INT
    { IfThen { left; op; right; line } }

comp_op:
  | EQ  { Equal }
  | NEQ { NotEqual }
  | LT  { LessThan }
  | LTE { LessThanOrEqual }
  | GT  { GreaterThanOrEqual }
  | GTE { GreaterThanOrEqual }

for_:
  | FOR var=ident EQ start=expr TO stop=expr
    { For { var; start; stop; step = None } }
  | FOR var=ident EQ start=expr TO stop=expr STEP step=expr
    { For { var; start; stop; step = Some step } }

next:
  | NEXT i=ident { Next i }

stop:
  | STOP { Stop }

def:
  | DEF FN name=LETTER LPARENT param=ident RPARENT EQ body=expr
    { Def { name; param; body } }

gosub:
  | GOSUB i=INT { GoSub i }

return:
  | RETURN { Return }

snum:
  | PLUS n=INT    { Pos, `Int n }
  | PLUS n=FLOAT  { Pos, `Float n }
  | MINUS n=INT   { Neg, `Int n }
  | MINUS n=FLOAT { Neg, `Float n }
