%{
  open Ast
  open ParsingCtx


  let ctx = ParsingCtx.create ()
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
  | body=line* EOF
    { raise_no_data ctx;
      raise_no_end ctx;
      let data = Utils.Stack.of_list ctx.data in
      { body; data; max_line = ctx.max_line } }

line:
  | n=INT s=stmt NEWLINE
    { incr_line_nb ctx;
      update_max_line ctx n;
      (n, s) }

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
  | s=return   { raise_end_not_last ctx; s }
  | e=end_     { e }

assign:
  | LET name=ident EQ expr=expr
    { incr_constant_label ctx;
      Let { name; expr } }

var:
  | s=subscript      { s }
  | i=LETTER | i=VAR { Name i }

subscript:
  | name=VAR LPARENT args=separated_nonempty_list(COMMA, expr) RPARENT
    { SubScript { name; args } }

ident:
  | i=LETTER | i=VAR { i }

expr:
  | PLUS e=eb | MINUS e=eb | e=eb { e }
  | left=eb op=op right=eb
    { BinOp { left; op; right } }
  | PLUS left=eb op=op right=eb
    { BinOp { left = UnaryOp { op = UPlus; operand = left }; op; right } }
  | MINUS left=eb op=op right=eb
    { BinOp { left = UnaryOp { op = Invert; operand = left }; op; right } }

eb:
  | LPARENT e=expr RPARENT { e }
  | f=fun_call             { f }
  | n=num                  { n }
  | v=var                  { v }

num:
  | i=INT   { raise_illegal_consti i; Int i }
  | f=FLOAT { raise_illegal_constf f; Float f }

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
  | READ vars=separated_nonempty_list(COMMA, var)
    { set_read_encounter ctx;
      Read vars }

data:
  | DATA numbers=separated_nonempty_list(COMMA, snum)
    { set_data_encounter ctx;
      update_data ctx numbers;
      Data }

snum:
  | n=INT | PLUS n=INT     { raise_illegal_consti n; Pos, `Int n }
  | n=FLOAT | PLUS n=FLOAT { raise_illegal_constf n; Pos, `Float n }
  | MINUS n=INT            { raise_illegal_consti n; Neg, `Int n }
  | MINUS n=FLOAT          { raise_illegal_constf n; Neg, `Float n }

print:
  | PRINT items=separated_list(COMMA, pitem)
    { Print { items; end_newline = false } }

pitem:
  | e=expr { e }
  | l=LABEL
    { incr_constant_label ctx;
      update_printed_length ctx @@ Buffer.length l;
      Label l }
  | label=LABEL expr=expr
    { incr_constant_label ctx;
      update_printed_length ctx @@ Buffer.length label;
      LabelConcat { label; expr } }

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

end_:
  | END { set_end_encounter ctx; End }
