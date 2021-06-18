syntax match Function '[^a-z#0-9.:*]\+'
syntax match Number '[0-9.:]\+'
syntax match Label '[a-z#]\+'
syntax region Comment start='\*' end='\*'

let b:current_syntax = 'mel'

highlight Comment ctermfg=Red
