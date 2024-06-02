%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}


%token EOF
%token AND
%token BLUE
%token BOOL
%token COLOR
%token COORD
%token COS 
%token DRAW
%token ELSE
%token FALSE
%token FLOOR
%token FOR
%token FOREACH
%token FROM
%token GREEN
%token HEAD
%token IF
%token IN
%token INTTYPE
%token LIST
%token NOT
%token OR
%token PIXEL
%token PRINT
%token REALTYPE
%token REAL_OF_INT
%token RED
%token SET
%token SIN
%token STEP
%token TAIL
%token TO 
%token TRUE
%token X
%token Y
%token PI 
%token BLOCKOPEN
%token BLOCKCLOSE
%token ADD
%token SUB
%token MUL 
%token DIV 
%token MOD 
%token EQ 
%token NEQ 
%token LT 
%token GT 
%token LEQ 
%token GEQ 
%token COLON
%token DOUBLE_COLON
%token DOT
%token COMMA
%token SEMICOLON
%token LPAR
%token RPAR
%token L_SQ_BRK
%token R_SQ_BRK
(* Extension *)
%token WHILE
%token QUESTION
%token POW2
(* Fin de Extension *)
%token <int> INT
%token <string> ID 
%token <float> REAL

(* If without else *)
%nonassoc IfThen
%nonassoc ELSE

(* Binary_operator *)
%left OR
%left AND
%nonassoc EQ NEQ LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%left POW2

(* . and :: *)
%right DOT DOUBLE_COLON 

(* Unary_operator *)
%nonassoc Unop

%start <program> main
%%

main:
| p = program EOF { p }

program:
| LT arg_list = argument_list GT stmt = statement { Program(arg_list,stmt) }
| stmt = statement { Program([],stmt) }

argument:
| t = type_expression COLON id = ID { Argument(id,t,Annotation.create $loc) }

argument_list:
| { [] }
| arg = argument { [arg] }
| arg = argument SEMICOLON arg_list = argument_list { arg::arg_list }

statement:
| SET LPAR expr1 = expression COMMA expr2 = expression RPAR { Affectation(expr1,expr2,Annotation.create $loc) }
| t = type_expression COLON id = ID { Declaration(id,t,Annotation.create $loc) } 
| BLOCKOPEN stmt_list = statement_list BLOCKCLOSE { Block(stmt_list,Annotation.create $loc) }
| LPAR test = expression RPAR QUESTION stmt1 = statement COLON stmt2 = statement { IfThenElse(test,stmt1,stmt2,Annotation.create $loc) }
| IF LPAR test = expression RPAR stmt1 = statement ELSE stmt2 = statement { IfThenElse(test,stmt1,stmt2,Annotation.create $loc) }
| IF LPAR test = expression RPAR stmt = statement %prec IfThen { IfThenElse(test,stmt, Nop, Annotation.create $loc) }
| FOR id = ID FROM expr1 = expression TO expr2 = expression STEP expr3 = expression stmt = statement { For(id,expr1, expr2, expr3,stmt,Annotation.create $loc) }
| FOREACH id = ID IN expr = expression stmt = statement { Foreach(id,expr,stmt,Annotation.create $loc) }
| DRAW LPAR expr = expression RPAR { Draw_pixel(expr,Annotation.create $loc) }
| PRINT LPAR expr = expression RPAR { Print(expr,Annotation.create $loc) }
| WHILE LPAR test = expression RPAR stmt = statement { While(test,stmt,Annotation.create $loc) }
| { Nop }

statement_list:
| stmt = statement { [stmt] }
| stmt = statement SEMICOLON stmt_list = statement_list { stmt::stmt_list }

expression:
| i = INT { Const_int(i,Annotation.create $loc) }
| r = REAL { Const_real(r,Annotation.create $loc) }
| PI { Const_real(Float.pi,Annotation.create $loc) }
| TRUE { Const_bool(true,Annotation.create $loc) }
| FALSE { Const_bool(false,Annotation.create $loc) }
| id = ID  { Variable(id,Annotation.create $loc) }
| COORD LPAR expr1 = expression COMMA expr2 = expression RPAR { Coord(expr1,expr2,Annotation.create $loc) }
| COLOR LPAR expr1 = expression COMMA expr2 = expression COMMA expr3 = expression  RPAR { Color(expr1,expr2,expr3,Annotation.create $loc) }
| PIXEL LPAR expr1 = expression COMMA expr2 = expression RPAR { Pixel(expr1,expr2,Annotation.create $loc) }
| expr = expression POW2 { Binary_operator(Times,expr,expr,Annotation.create $loc) }
| expr1 = expression op = binary_operator expr2 = expression { Binary_operator(op, expr1, expr2,Annotation.create $loc) }
| op = unary_operator expr = expression %prec Unop { Unary_operator(op, expr,Annotation.create $loc) }
| expr = expression DOT f = field_accessor { Field_accessor(f, expr,Annotation.create $loc) }
| L_SQ_BRK expr_list = expression_list R_SQ_BRK { List(expr_list,Annotation.create $loc) }
| expr1 = expression DOUBLE_COLON expr2 = expression { Append(expr1,expr2,Annotation.create $loc) }
| LPAR expr = expression RPAR { expr }

expression_list:
| expr = expression { [expr] }
| expr = expression COMMA expr_list = expression_list { expr::expr_list }
| { [] }

%inline binary_operator:
| ADD {Plus}
| SUB {Minus}
| MUL {Times}
| DIV {Div}
| MOD {Rem}
| AND {And}
| OR  {Or}
| EQ  {Equal}
| NEQ {Diff}
| LT  {Lt}
| GT  {Gt}
| LEQ {Leq}
| GEQ {Geq}

%inline unary_operator:
| SUB {Opposite}
| NOT {Not}
| HEAD {Head}
| TAIL {Tail}
| FLOOR {Floor}
| REAL_OF_INT {Real_of_int}
| COS {Cos}
| SIN {Sin}

%inline field_accessor:
| COLOR {Color_field}
| COORD {Coord_field}
| X {X_field}
| Y {Y_field}
| RED {Red_field}
| GREEN {Green_field}
| BLUE {Blue_field}

type_expression:
| INTTYPE { Type_int }
| REALTYPE { Type_real }
| BOOL { Type_bool }
| COORD { Type_coord }
| COLOR { Type_color }
| PIXEL { Type_pixel }
| LIST LPAR t = type_expression RPAR { Type_list(t) }