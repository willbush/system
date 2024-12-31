Abbreviations:

- sel: selection
- sels: selections

Lowercase:

```
q         w         f         p         b         j         l         u         ,         ;
quit      move      move      paste     move      goto      insert    undo      keep      collapse
          next      next      after     prev      line      mode                primary   sel
          word      word end            word                                    sel

a         r         s         t         g         m         n         e         i         o          '
append    select    find      find      goto      move      move      move      move      open       N/A
mode      mode      next      till      mode      left      down      up        right     below
                    char      char

z         x         c         d         v         k         h         y         .         /
view      extend    change    delete    replace   match     search    yank      repeat    search
mode      line      sel       sel                 mode      next                last
          below                                                                 motion
```

Uppercase:

```
Q         W         F         P         B         J         L         U         <         :
record    move      move      paste     move      join      insert    redo      unindent  command
macro     next      next      before    prev      sels      at line                       mode
          WORD      WORD end            WORD                start

A         R         S         T         G         M         N         E         I         O          "
append    N/A       find      find      goto      goto      page      page      goto      open       N/A
at line             prev      prev      last      line      cursor    cursor    line      above
end                 char      char      line      start     half down half up   end

Z         X         C         D         V         K         H         Y         >         ?
sticky    extend    copy      N/A       replace   keep      search    N/A       indent    reverse
view      to line   sel                 with      sels      prev                          search
mode      bounds    to next             yanked
```

Ctrl + key:

```
C-q       C-w       C-f       C-p       C-b       C-j       C-l       C-u       C-,       C-;
N/A       window    page      N/A       page      N/A       align     kill to   N/A
          mode      down                up                  view top  line
                                                                      start

C-a       C-r       C-s       C-t       C-g       C-m       C-n       C-e       C-i       C-o        C-'
N/A       N/A       save      N/A       N/A       N/A       N/A       N/A       jump      jump       N/A
                    sel                                                         forward   backward

C-z       C-x       C-c       C-d       C-v       C-k       C-h       C-y       C-.       C-/
suspend   N/A       N/A       half page N/A       kill to   N/A
                              down                line
                                                  end
```

Alt + key:

```
A-q       A-w       A-f       A-p       A-b       A-j       A-l       A-u       A-,       A-;
N/A       N/A       N/A       select    move      N/A       N/A       earlier   remove    repeat
                              prev      parent                                  primary   last
                              sibling   node start                              sel       motion

A-a       A-r       A-s       A-t       A-g       A-m       A-n       A-e       A-i       A-o        A-'
select    N/A       split     N/A       N/A       N/A       select    move      shrink    expand     N/A
all                 sel on                                  next      parent    sel       sel
siblings            newline                                 sibling   node end

A-z       A-x       A-c       A-d       A-v       A-k       A-h       A-y       A-.       A-/
N/A       shrink    change    delete    N/A       remove    N/A       flip      repeat    N/A
          to line   sel       sel                 sels                sels      last
          bounds    noyank    noyank                                            insert
```

Lower Layer:

```
1         2         3         4         5         6         7         8         9         0

~         (         !         =         )         {         %         &         }         |          Del
switch    rotate    shell     align     rotate    N/A       select    select    N/A       shell      N/A
case      sels      insert    sels      sels                all       register            pipe
          backward  output              forward

N/A       N/A       N/A       _         N/A       N/A       N/A       N/A       N/A       N/A
                              trim
                              sels
```

Hyper Layer:

```
1         2         3         4         5         6         7         8         9         0

\`        [         +         -         ]         ^         *         #         $         \          Del
switch    N/A       increment decrement N/A       goto      search    N/A       goto      N/A        N/A
lowercase                                         first     sel                 line
                                                  non                           end
                                                  whitespace

N/A       <         >         _         N/A       N/A       @         N/A       N/A       N/A
          unindent  indent    trim
                              sels
```
