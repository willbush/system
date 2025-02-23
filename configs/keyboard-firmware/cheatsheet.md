Abbreviations:

- sel: selection
- sels: selections

Lowercase:

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

```rust
"s" => { "Search"
  "s" => find_next_char,
  "S" => find_prev_char,
  "t" => find_till_char,
  "T" => till_prev_char,
},
"t" => { "Tap select"
  "a" => select_all,
  "c" => collapse_selection,
  "r" => select_regex,
  "s" => split_selection,
  "t" => flip_selections,

  "i" => shrink_selection,
  "o" => expand_selection,
},

"j" => { "Jump"
  "j" => goto_line,

  "n" => select_next_sibling,
  "p" => select_prev_sibling,

  "b" => move_parent_node_start,
  "e" => move_parent_node_end,
},
```

S-*:

```
Q         W         F         P         B         J         L         U         <         :
record    move      move      paste     move      join      insert    redo      unindent  command
macro     next      next      before    prev      sels      at line                       mode
          WORD      WORD end            WORD                start

A         R         S         T         G         M         N         E         I         O          "
append                                  goto      goto      page      page      goto      open       rep last
at line                                 last      line      cursor    cursor    line      above      motion
end                                     line      start     half down half up   end                  reverse

Z         X         C         D         V         K         H         Y         >         ?
sticky    extend    copy                replace   keep      search              indent    reverse
view      line      sel                 with      sels      prev                          search
mode      above     to next             yanked
```

C-*:

```
C-q       C-w       C-f       C-p       C-b       C-j       C-l       C-u       C-,       C-;
          window    page                page                align     kill to
          mode      down                up                  view top  line
                                                                      start

C-a       C-r       C-s       C-t       C-g       C-m       C-n       C-e       C-i       C-o        C-'
                    save                wezterm                                 jump      jump
                    sel                 prefix                                  forward   backward

C-z       C-x       C-c       C-d       C-v       C-k       C-h       C-y       C-.       C-/
suspend                       half page           kill to
                              down                line
                                                  end
```

A-*:

```
A-q       A-w       A-f       A-p       A-b       A-j       A-l       A-u       A-,       A-;
                              select    move                          earlier
                              prev      parent
                              sibling   node start

A-a       A-r       A-s       A-t       A-g       A-m       A-n       A-e       A-i       A-o        A-'
select              split                                   select    move
all                 sel on                                  next      parent
siblings            newline                                 sibling   node end

A-z       A-x       A-c       A-d       A-v       A-k       A-h       A-y       A-.       A-/
          shrink    change    delete              remove              flip
          to line   sel       sel                 sels                sels
          bounds    noyank    noyank
```

Lower Layer:

```
1         2         3         4         5         6         7         8         9         0

~         (         !         =         )         {         %         &         }         |          Del
switch    rotate    shell     align     rotate              match     select              shell
case      sels      insert    sels      sels                brackets  register            pipe
          backward  output              forward

                              _
                              trim
                              sels
```

Hyper Layer:

```
1         2         3         4         5         6         7         8         9         0

\`        [         +         -         ]         ^         *         #         $         \          Del
switch              increment decrement           goto      search              goto
lowercase                                         first     sel                 line
                                                  non                           end
                                                  whitespace

                              _                             @
                              trim
                              sels
```
