%{

open Expr

%}
/* ordre de priorité et de précedence*/
%token <int>    VAL
%token <string> IDE
%token IN
%token SUB ADD
%token MUL
%token LT LE GT GE EQ NEQ
%token LPAREN RPAREN
%token LET REC
%token IF THEN ELSE TRY WITH RAISE EX
%token FUN ARROW
%token EOL
%token EOF


%nonassoc EOL
%nonassoc EOF
%nonassoc TEMP
%nonassoc LET REC FUN EQ
%right ARROW
%nonassoc VAL IDE
%nonassoc LPAREN RPAREN NEQ GE GT LE LT
%nonassoc IF THEN ELSE TRY WITH EX
%right IN
%left ADD SUB
%left MUL
%nonassoc UMINUS


%start main
%type <Expr.expr list> main
%%
/* Je défini 5 regles ou expr est la regle principal avec expr2 et expr3 qui le complete */
/* fct permet de gérer les fonctions. Les fonctions a un seul argument sont gérées dans expr. */
/* expressionbooleene est présente pour la condition du if */
main:
  | expr EOL main { $1::$3 }
  | EOF           { [] }
;


expr:
  | expr ADD expr                                   { Ope ($1, Add, $3) } /* + */
  | expr SUB expr                                   { Ope ($1, Sub, $3) } /* - */
  | expr MUL expr                                   { Ope ($1, Mul, $3) } /* x */
  | SUB expr %prec UMINUS                           { Ope (Val (Int 0), Sub, $2) } /* - unitaire */
  | LET IDE fct                                     { Let ($2, $3) } /* let avec plusieurs arguments sans in */
  | LET IDE EQ expr                                 { Let ($2, $4) } /* let 1 seul argument sans in */
  | LET IDE EQ expr IN expr                         { LetI ($2, $4, $6) } /* let 1 seul argument avec in */
  | LET IDE fct IN expr                             { LetI($2,$3,$5)} /* let plusieurs arguments avec in */
  | LET REC IDE fct                                 { LetR ($3, $4) } /* let rec plusieurs arguments sans in */
  | LET REC IDE EQ expr                             { LetR ($3, $5) } /* let rec 1 seul argument sans in */
  | LET REC IDE EQ expr IN expr                     { LetIR ($3, $5, $7) } /* let rec 1 seul argument avec in */
  | LET REC IDE fct IN expr                         { LetIR ($3, $4, $6) } /* let rec plusieurs arguments avec in */
  | IF expressionbooleene THEN expr ELSE expr       { IfE ($2, $4, $6) } /* if condition then e1 else e2 */
  | TRY expr WITH EX IDE ARROW expr                 { TrW ($2, $5,$7) } /* try expr with E ide -> expr */
  | TRY expr WITH LPAREN EX IDE RPAREN ARROW expr   { TrW ($2, $6,$9) } /* try expr with E ide -> expr */
  | RAISE LPAREN EX expr RPAREN                     { Rai ($4) } /* raise (E val) */
  | RAISE EX expr                                   { Rai ($3) } /* raise E val */
  | expr2                                           { $1 }
  | FUN IDE ARROW expr                              { Fun ($2,$4) } /* fun ide -> e */
  | FUN IDE fct                                     { Fun ($2, $3) } /* fun plusieurs arguments */
;
expr2:
  | expr2 expr3                                     { App ($1,$2)} /* (fun ide -> e1) e2 */
  | expr3                                           { $1 }
;

expr3:
  | VAL                                             { Val (Int $1) } /* nombre entier */
  | IDE                                             { Ide ($1) } /* nom de l'argument */
  | LPAREN expr RPAREN                              { $2 } /* ( e ) */
;

fct:
  | IDE EQ expr                                     { Fun ($1,$3) } /* deuxieme argument = */
  | IDE ARROW expr                                  { Fun ($1,$3) } /* deuxieme argument -> */
  | IDE fct                                         { Fun ($1,$2) } /* plus de deux arguments */
;



expressionbooleene:
  | expr LT expr                                    { Ope ($1,Lt, $3) } /* e1 < e2 */
  | expr LE expr                                    { Ope($1, Le ,$3) } /* e1 <= e2 */
  | expr GT expr                                    { Ope ($1, Gt,  $3) } /* e1 > e2 */
  | expr GE expr                                    { Ope ($1, Ge, $3) } /* e1 >= e2 */
  | expr EQ expr                                    { Ope ($1, Eq,  $3) } /* e1 = e2 */
  | expr NEQ expr                                   { Ope ($1,Neq,  $3) } /* e1 <> e2 */
  | LPAREN expressionbooleene RPAREN                { $2 } /* si on a des parenthèses dans la condition */
;
