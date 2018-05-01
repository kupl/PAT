{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
} 

rule start = parse
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | "*" {STAR}
     | " " {BLANK}
     | "\n" {NEWLINE}
     | "--- EXAMPLE ---" {DIVISION}
     | eof   {EOF}
     | _ { raise LexicalError }

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
