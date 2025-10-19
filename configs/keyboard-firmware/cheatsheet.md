Abbreviations:

- sel: selection
- sels: selections
- []: [mode] (leader keys)

# lowercase

```
q         w         f         p         b         j         l         u         ,         ;
quit      move      move      paste     move      [jump]    insert    undo      keep      remove
          next      next      after     prev                mode                primary   primary
          word      word end            word                                    sel       sel

a         r         s         t         g         m         n         e         i         o          '
append    select    [search]  [tap]     [goto]    move      move      move      move      open       repeat
mode      mode                                    left      down      up        right     below      last
                                                                                                     motion

z         x         c         d         v         k         h         y         .         /
view      extend    change    delete    replace   [knit]    search    yank      repeat    search
mode      line      sel       sel                           next                last
          below                                                                 insert
```

```
"s" => { "Search"
  "s" => find_next_char,
  "t" => find_till_char,
  "w" => search_selection,
  "W" => search_selection_detect_word_boundaries,
},
"S" => find_prev_char,
"T" => till_prev_char,
"t" => { "Tap"
  "a" => select_all,
  "c" => collapse_selection,

  "f" => ensure_selections_forward,
  "r" => select_regex,
  "s" => split_selection,
  "t" => flip_selections,

  "k" => keep_selections,
  "K" => remove_selections,

  "M" => merge_consecutive_selections,
  "m" => merge_selections,

  "l" => split_selection_on_newline,

  "i" => shrink_selection,
  "o" => expand_selection,

  "n" => select_next_sibling,
  "p" => select_prev_sibling,
},
"j" => { "Jump"
  "j" => goto_line,
  "s" => move_parent_node_start,
  "e" => move_parent_node_end,
  "(" => rotate_selections_first,
  ")" => rotate_selections_last,
},
```

# shift keys

```
Q         W         F         P         B         J         L         U         <         :
record    move      move      paste     move      join      insert    redo      unindent  command
macro     next      next      before    prev      sels      at line                       mode
          WORD      WORD end            WORD                start

A         R         S         T         G         M         N         E         I         O          "
append              find      till      goto      goto      page      page      goto      open       rep last
at line             prev      prev      last      line      cursor    cursor    line      above      motion
end                 chart     char      line      start     half down half up   end                  reverse

Z         X         C         D         V         K         H         Y         >         ?
sticky    extend    copy                replace             search    yank      indent    reverse
view      line      sel                 with                prev      joined              search
mode      above     to next             yanked
```

# ctrl keys (insert mode)


```
C-q       C-w       C-f       C-p       C-b       C-j       C-l       C-u       C-,       C-;
          del word                                RET                 kill to
          backward                                                    line
                                                                      start

C-a       C-r       C-s       C-t       C-g       C-m       C-n       C-e       C-i       C-o        C-'
goto                commit                                            goto
line                undo                                              line end
start               checkpt                                           newline

C-z       C-x       C-c       C-d       C-v       C-k       C-h       C-y       C-.       C-/
          complete            del char            kill to   backspace
                              forward             line
                                                  end
```

# ctrl keys (normal mode)


```
C-q       C-w       C-f       C-p       C-b       C-j       C-l       C-u       C-,       C-;
          window    page                page                align
          mode      down                up                  view top


C-a       C-r       C-s       C-t       C-g       C-m       C-n       C-e       C-i       C-o        C-'
                    save                                                        jump      jump
                    sel                                                         forward   backward

C-z       C-x       C-c       C-d       C-v       C-k       C-h       C-y       C-.       C-/
suspend                       half page
                              down

```

# alt keys

```
A-q       A-w       A-f       A-p       A-b       A-j       A-l       A-u       A-,       A-;
                                                                      earlier



A-a       A-r       A-s       A-t       A-g       A-m       A-n       A-e       A-i       A-o        A-'



A-z       A-x       A-c       A-d       A-v       A-k       A-h       A-y       A-.       A-/
                    change    delete
                    sel       sel
                    noyank    noyank
```

# lower layer

```
1         2         3         4         5         6         7         8         9         0

~         (         !         =         )         {         %         &         }         |          Del
switch    rotate              align     rotate              match     select
case      sels                sels      sels                brackets  register
          backward                      forward

                              _
                              trim
                              sels
```

# hyper layer

```
1         2         3         4         5         6         7         8         9         0

\`        [         +         -         ]         ^         *         #         $         \          Del
switch    [left     increment decrement [right    goto                          goto
lowercase bracket]                      bracket]  first                         line
                                                  non                           end
                                                  whitespace

                              _                             @
                              trim
                              sels
```
