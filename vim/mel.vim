syn match Function '[^a-z#0-9.:*]\+'
syn match Number '[0-9.:]\+'
syn match Label '[a-z#]\+'
syn region Comment start='\*' end='\*'

let b:current_syntax = "mel"

hi Comment ctermfg=Red
