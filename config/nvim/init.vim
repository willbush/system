" up/down/left/right movements
nnoremap m h|xnoremap m h|onoremap m h
nnoremap n j|xnoremap n j|onoremap n j
nnoremap e k|xnoremap e k|onoremap e k
nnoremap i l|xnoremap i l|onoremap i l

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

" find forward/back now search
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
nnoremap E <C-U>|xnoremap E <C-U>|onoremap E <C-U>
nnoremap N <C-D>|xnoremap N <C-D>|onoremap N <C-D>

" join lines
nnoremap <C-J> J

" set mark
nnoremap k m
" goto mark
nnoremap j `|onoremap j `
nnoremap J '|onoremap J '

" use register on a easier key for my personal keyboard layout
" nnoremap & \"|onoremap & \"|

" f/F = far word/WORD
nnoremap f e|xnoremap f e|onoremap f e
nnoremap F E|xnoremap F E|onoremap F E
