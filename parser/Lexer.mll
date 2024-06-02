
{
    open Parser
    exception Error of string
}

let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let alphamin = ['a'-'z']
let digit = ['0'-'9']
let skip = [' ' '\t' '\r']

rule token = parse
    | "And"             { AND }
    | "Blue"            { BLUE }
    | "Bool"            { BOOL }
    | "Color"           { COLOR }
    | "Coord"           { COORD }
    | "Cos"             { COS }
    | "Draw"            { DRAW }
    | "Else"            { ELSE }
    | "False"           { FALSE }
    | "Floor"           { FLOOR }
    | "For"             { FOR }
    | "Foreach"         { FOREACH }
    | "From"            { FROM }
    | "Green"           { GREEN }
    | "Head"            { HEAD }
    | "If"              { IF }
    | "In"              { IN }
    | "Int"             { INTTYPE }
    | "List"            { LIST }
    | "Not"             { NOT }
    | "Or"              { OR }
    | "Pixel"           { PIXEL }
    | "Print"           { PRINT }
    | "Real"            { REALTYPE }
    | "Real_of_int"     { REAL_OF_INT }
    | "Red"             { RED }
    | "Set"             { SET }
    | "Sin"             { SIN }
    | "Step"            { STEP }
    | "Tail"            { TAIL }
    | "To"              { TO }
    | "True"            { TRUE }
    | "X"               { X }
    | "Y"               { Y }
    | "Pi"              { PI }
    | "$<"              { BLOCKOPEN }
    | ">$"              { BLOCKCLOSE }
    | "+"               { ADD }
    | "-"               { SUB }
    | "*"               { MUL }
    | "/"               { DIV }
    | "%"               { MOD }
    | "="               { EQ }
    | "<>"              { NEQ }
    | "<="              { LEQ }
    | ">="              { GEQ }
    | "<"               { LT }
    | ">"               { GT }
    | ":"               { COLON }
    | "::"              { DOUBLE_COLON }
    | "."               { DOT }
    | ","               { COMMA }
    | ";"               { SEMICOLON }
    | "("               { LPAR }
    | ")"               { RPAR }
    | "["               { L_SQ_BRK }
    | "]"               { R_SQ_BRK }
    (* Extension *)
    | "While"           { WHILE }
    | "?"               { QUESTION }
    | "**2"              { POW2 }
    (* Fin de l'extension *)
    (*ID*)
    | (alphamin)+ (alphanum)* as s { ID s } 
    (*INT*)
    | "0x" (['0'-'9' 'A'-'F'])+ as s { INT (int_of_string s) }
    | (digit)+ as s     { INT (int_of_string s) }
    (*REAL*)
    | (digit)* "." (digit)* as s { REAL (float_of_string s) }
    (*commentaries*)
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
    | "/*"              {commentary lexbuf}
    (*WHITESPACE AND NEWLINE*)
    | (skip)+           { token lexbuf } 
    | '\n'              { Lexing.new_line lexbuf; token lexbuf }
    | eof               { EOF }
    (*ERROR*)
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }


    and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*/"      { token lexbuf }
    | _ { commentary lexbuf }