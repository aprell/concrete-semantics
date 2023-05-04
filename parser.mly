%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> NAME
%token PLUS LESS
%token NOT AND
%token GETS
%token IF ELSE
%token WHILE
%token SKIP
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token EOF

%left SEMICOLON
%left AND
%left PLUS
%right NOT

%type <Ast.command> program stmt block
%type <Ast.aexpr> aexpr
%type <Ast.bexpr> bexpr

%start program

%%

program:
  | stmt EOF { $1 }
  ;

stmt:
  | NAME GETS aexpr { Assign ($1, $3) }
  | stmt SEMICOLON stmt { Seq ($1, $3) }
  | IF bexpr block { If ($2, $3, Skip) }
  | IF bexpr block ELSE block { If ($2, $3, $5) }
  | WHILE bexpr block { While ($2, $3) }
  | SKIP { Skip }
  ;

block:
  | delimited(LBRACE, stmt, RBRACE) { $1 }
  ;

aexpr:
  | INT { Int $1 }
  | NAME { Var $1 }
  | aexpr PLUS aexpr { Add ($1, $3) }
  | LPAREN aexpr RPAREN { $2 }
  ;

bexpr:
  | BOOL { Bool $1 }
  | NOT bexpr { Not $2 }
  | bexpr AND bexpr { And ($1, $3) }
  | aexpr LESS aexpr { Less ($1, $3) }
  | LPAREN bexpr RPAREN { $2 }
  ;
