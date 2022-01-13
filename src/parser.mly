%{
  open Ast

  let max_line = ref 0

  let update_line n =
    if n <= 0 || n > 99999 then raise ErrKind.(Parse_error IllegalLineNumber)
    else if n > !max_line then max_line := n
%}

%token LET READ DATA PRINT GO IF THEN FOR TO STEP NEXT END STOP DEF FN GOSUB RETURN DIM REM
%token PLUS MINUS TIMES DIV POWER
%token EQ NEQ LT LTE GT GTE
%token LPARENT RPARENT
%token COMMA NEWLINE EOF

%token <string> VAR
%token <string> LABEL
%token <float> FLOAT
%token <int> INT

%start prgrm

%type <Ast.parsed_ast> prgrm

%%

prgrm:
  | lines=line* EOF { Preprocess.pp lines @@ !max_line + 1 }

line:
  | n=INT s=stmt NEWLINE { update_line n; (n, s) }

stmt:
  | LET name=VAR EQ expr=expr                              { Let { name; expr} }
  | READ vars=separated_nonempty_list(COMMA, VAR)          { Read vars }
  | DATA numbers=separated_nonempty_list(COMMA, number)    { Data numbers }
  | PRINT pables=separated_nonempty_list(COMMA, printable) { Print pables }
  | GO TO n=INT                                            { GoTo n }
  | IF left=expr op=relational right=expr THEN line=INT
    { IfThen { left; op; right; line } }
  | f=for_                                                 { f }
  | NEXT var=VAR                                           { Next var }
  | END                                                    { End }

expr:
  | left=term PLUS right=expr  { BinOp { left; op = Plus; right } }
  | left=term MINUS right=expr { BinOp { left; op = Minus; right } }
  | t=term                     { t }

number:
  | i=INT { NInt i }
  | i=FLOAT { NFloat i }

relational:
  | EQ  { Equal }
  | NEQ { NotEqual }
  | LT  { LessThan }
  | LTE { LessThanOrEqual }
  | GT  { GreaterThanOrEqual }
  | GTE { GreaterThanOrEqual }

term:
  | left=factor TIMES right=term { BinOp { left; op = Times ; right } }
  | left=factor DIV right=term   { BinOp { left; op = Div; right } }
  | f=factor                     { f }
  | MINUS operand=factor         { UnaryOp { op = Invert; operand } }

parent_expr:
  | LPARENT e=expr RPARENT { e }

factor:
  | i=INT         { Number (NInt i) }
  | f=FLOAT       { Number (NFloat f) }
  | v=VAR         { Ident v }
  | p=parent_expr { p }

for_:
  | FOR var=VAR EQ start=expr TO stop=expr
    { For { var; start; stop; step = None } }
  | FOR var=VAR EQ start=expr TO stop=expr STEP step=expr
    { For { var; start; stop; step = Some step } }

printable:
  | l=LABEL               { Label l }
  | label=LABEL expr=expr { LabelConcat { label; expr} }
  | expr=expr             { expr }
