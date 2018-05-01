%{

%}

%token BLANK
%token STAR
%token NEWLINE
%token DIVISION
%token EOF

%start input
%type <string list> input
%%

input:
  | examples EOF {$1}

examples:
  | example examples {$1::$2} 
  | example {[$1]}

example:
  | DIVISION NEWLINE seq {$3}

seq:
  | character seq {$1^$2}
  | character {$1}

character:
  | STAR {"*"} 
  | BLANK {" "}
  | NEWLINE {"\n"}

%%
