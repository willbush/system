" up/down/left/right movements
nnoremap m h|xnoremap m h|onoremap m h
nnoremap n j|xnoremap n j|onoremap n j
nnoremap e k|xnoremap e k|onoremap e k
nnoremap i l|xnoremap i l|onoremap i l

" new mnemonic: f/F = far word/WORD
nnoremap f e|xnoremap f e|onoremap f e
nnoremap F E|xnoremap F E|onoremap F E

" insert mode / insert beginning of line
nnoremap l i
nnoremap L I
" redo
nnoremap U <C-R>

" swap replace to 'v' now revise and visual to 'r' now range
nnoremap v r|xnoremap v r|onoremap v r
nnoremap V R|xnoremap V R|onoremap V R
nnoremap r v|xnoremap r v
nnoremap R V|xnoremap R V
nnoremap gr gv " range restore
nnoremap <C-R> <C-V> " range block

" new mnemonic: s/S = search
nnoremap s f|xnoremap s f|onoremap s f
nnoremap S F|xnoremap S F|onoremap S F

" next / prev
nnoremap h n|xnoremap h n|onoremap h n
nnoremap H N|xnoremap H N|onoremap H N

" hard beginning of line
nnoremap M 0|xnoremap M 0|onoremap M 0
" end of line
nnoremap I $|xnoremap I $|onoremap I $

" scroll down / up
nnoremap N 10j|xnoremap N 10j|onoremap N 10j
nnoremap E 10k|xnoremap E 10k|onoremap E 10k
nnoremap <C-S-n> <C-D>|xnoremap <C-S-n> <C-D>|onoremap <C-S-n> <C-D>
nnoremap <C-S-e> <C-U>|xnoremap <C-S-e> <C-U>|onoremap <C-S-e> <C-U>

" join lines
nnoremap <C-J> J

" set mark
nnoremap k m
" goto mark
nnoremap j `|onoremap j `
nnoremap J '|onoremap J '

" repeat find char
nnoremap ' ;|xnoremap ' ;|onoremap ' ;
" rev repeat find char
nnoremap " ,|xnoremap " ,|onoremap " ,

" Use register on a easier key for my personal keyboard layout.
" The insanity that is vim script uses '"' for comments... sometimes.
" After a map command it doesn't need to be escaped.
nnoremap & "|xnoremap & "| onoremap & "

" width of a tab character
set tabstop=2
" indents width
set shiftwidth=2
" number of columns for a TAB
set softtabstop=2
" Expand TABs to spaces
set expandtab
set autoindent
