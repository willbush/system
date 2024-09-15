{ ... }:
{
  # No automatic theme stylix
  stylix.targets.helix.enable = false;

  programs.helix = {
    enable = true;

    settings = {
      theme = "hex_lavender";
      # use bar cursor in insert mode like vim
      editor = {
        cursor-shape.insert = "bar";
        cursorline = true;
        color-modes = true;
      };

      # Custom COLEMAK-DHm based keybindings for helix.
      #
      # q w f p b j l u , ;
      # a r s t g m n e i o '
      # z x c d v k h y . /
      #
      # This will bind everything explicitly even if it's the same as the
      # default because that makes it easier to not lose track of what's bound
      # to what.
      #
      # EXCEPTIONS:
      # - modes I don't rebind
      # - arrow keys I don't rebind
      #
      # DEFAULTS:
      # https://github.com/helix-editor/helix/blob/1b5295a3f3d7cccd96eed5bfd394807a4dae87fc/helix-term/src/keymap/default.rs#L8
      keys =

        let
          # Nuke the site from orbit, it's the only way to be sure.
          no_op_all = {
            # alphas
            "a" = "no_op";
            "b" = "no_op";
            "c" = "no_op";
            "d" = "no_op";
            "e" = "no_op";
            "f" = "no_op";
            "g" = "no_op";
            "h" = "no_op";
            "i" = "no_op";
            "j" = "no_op";
            "k" = "no_op";
            "l" = "no_op";
            "m" = "no_op";
            "n" = "no_op";
            "o" = "no_op";
            "p" = "no_op";
            "q" = "no_op";
            "r" = "no_op";
            "s" = "no_op";
            "t" = "no_op";
            "u" = "no_op";
            "v" = "no_op";
            "w" = "no_op";
            "x" = "no_op";
            "y" = "no_op";
            "z" = "no_op";

            "C-a" = "no_op";
            "C-b" = "no_op";
            "C-c" = "no_op";
            "C-d" = "no_op";
            "C-e" = "no_op";
            "C-f" = "no_op";
            "C-g" = "no_op";
            "C-h" = "no_op";
            "C-i" = "no_op";
            "C-j" = "no_op";
            "C-k" = "no_op";
            "C-l" = "no_op";
            "C-m" = "no_op";
            "C-n" = "no_op";
            "C-o" = "no_op";
            "C-p" = "no_op";
            "C-q" = "no_op";
            "C-r" = "no_op";
            "C-s" = "no_op";
            "C-t" = "no_op";
            "C-u" = "no_op";
            "C-v" = "no_op";
            "C-w" = "no_op";
            "C-x" = "no_op";
            "C-y" = "no_op";
            "C-z" = "no_op";

            "A-a" = "no_op";
            "A-b" = "no_op";
            "A-c" = "no_op";
            "A-d" = "no_op";
            "A-e" = "no_op";
            "A-f" = "no_op";
            "A-g" = "no_op";
            "A-h" = "no_op";
            "A-i" = "no_op";
            "A-j" = "no_op";
            "A-k" = "no_op";
            "A-l" = "no_op";
            "A-m" = "no_op";
            "A-n" = "no_op";
            "A-o" = "no_op";
            "A-p" = "no_op";
            "A-q" = "no_op";
            "A-r" = "no_op";
            "A-s" = "no_op";
            "A-t" = "no_op";
            "A-u" = "no_op";
            "A-v" = "no_op";
            "A-w" = "no_op";
            "A-x" = "no_op";
            "A-y" = "no_op";
            "A-z" = "no_op";

            "A" = "no_op";
            "B" = "no_op";
            "C" = "no_op";
            "D" = "no_op";
            "E" = "no_op";
            "F" = "no_op";
            "G" = "no_op";
            "H" = "no_op";
            "I" = "no_op";
            "J" = "no_op";
            "K" = "no_op";
            "L" = "no_op";
            "M" = "no_op";
            "N" = "no_op";
            "O" = "no_op";
            "P" = "no_op";
            "Q" = "no_op";
            "R" = "no_op";
            "S" = "no_op";
            "T" = "no_op";
            "U" = "no_op";
            "V" = "no_op";
            "W" = "no_op";
            "X" = "no_op";
            "Y" = "no_op";
            "Z" = "no_op";

            "C-A" = "no_op";
            "C-B" = "no_op";
            "C-C" = "no_op";
            "C-D" = "no_op";
            "C-E" = "no_op";
            "C-F" = "no_op";
            "C-G" = "no_op";
            "C-H" = "no_op";
            "C-I" = "no_op";
            "C-J" = "no_op";
            "C-K" = "no_op";
            "C-L" = "no_op";
            "C-M" = "no_op";
            "C-N" = "no_op";
            "C-O" = "no_op";
            "C-P" = "no_op";
            "C-Q" = "no_op";
            "C-R" = "no_op";
            "C-S" = "no_op";
            "C-T" = "no_op";
            "C-U" = "no_op";
            "C-V" = "no_op";
            "C-W" = "no_op";
            "C-X" = "no_op";
            "C-Y" = "no_op";
            "C-Z" = "no_op";

            "A-A" = "no_op";
            "A-B" = "no_op";
            "A-C" = "no_op";
            "A-D" = "no_op";
            "A-E" = "no_op";
            "A-F" = "no_op";
            "A-G" = "no_op";
            "A-H" = "no_op";
            "A-I" = "no_op";
            "A-J" = "no_op";
            "A-K" = "no_op";
            "A-L" = "no_op";
            "A-M" = "no_op";
            "A-N" = "no_op";
            "A-O" = "no_op";
            "A-P" = "no_op";
            "A-Q" = "no_op";
            "A-R" = "no_op";
            "A-S" = "no_op";
            "A-T" = "no_op";
            "A-U" = "no_op";
            "A-V" = "no_op";
            "A-W" = "no_op";
            "A-X" = "no_op";
            "A-Y" = "no_op";
            "A-Z" = "no_op";

            # special
            "$" = "no_op";
            "!" = "no_op";
            "#" = "no_op";
            "%" = "no_op";
            "&" = "no_op";
            "'" = "no_op";
            "(" = "no_op";
            ")" = "no_op";
            "*" = "no_op";
            "+" = "no_op";
            "," = "no_op";
            "minus" = "no_op";
            "." = "no_op";
            "/" = "no_op";
            ";" = "no_op";
            "<" = "no_op";
            "=" = "no_op";
            ">" = "no_op";
            "@" = "no_op";
            "[" = "no_op";
            "\"" = "no_op";
            "\\" = "no_op";
            "]" = "no_op";
            "^" = "no_op";
            "_" = "no_op";
            "`" = "no_op";
            "{" = "no_op";
            "|" = "no_op";
            "}" = "no_op";
            "~" = "no_op";

            "C-$" = "no_op";
            "C-!" = "no_op";
            "C-#" = "no_op";
            "C-%" = "no_op";
            "C-&" = "no_op";
            "C-'" = "no_op";
            "C-(" = "no_op";
            "C-)" = "no_op";
            "C-*" = "no_op";
            "C-+" = "no_op";
            "C-," = "no_op";
            "C-minus" = "no_op";
            "C-." = "no_op";
            "C-/" = "no_op";
            "C-;" = "no_op";
            "C-<" = "no_op";
            "C-=" = "no_op";
            "C->" = "no_op";
            "C-@" = "no_op";
            "C-[" = "no_op";
            "C-\"" = "no_op";
            "C-\\" = "no_op";
            "C-]" = "no_op";
            "C-^" = "no_op";
            "C-_" = "no_op";
            "C-`" = "no_op";
            "C-{" = "no_op";
            "C-|" = "no_op";
            "C-}" = "no_op";
            "C-~" = "no_op";

            "A-$" = "no_op";
            "A-!" = "no_op";
            "A-#" = "no_op";
            "A-%" = "no_op";
            "A-&" = "no_op";
            "A-'" = "no_op";
            "A-(" = "no_op";
            "A-)" = "no_op";
            "A-*" = "no_op";
            "A-+" = "no_op";
            "A-," = "no_op";
            "A-minus" = "no_op";
            "A-." = "no_op";
            "A-/" = "no_op";
            "A-;" = "no_op";
            "A-<" = "no_op";
            "A-=" = "no_op";
            "A->" = "no_op";
            "A-@" = "no_op";
            "A-[" = "no_op";
            "A-\"" = "no_op";
            "A-\\" = "no_op";
            "A-]" = "no_op";
            "A-^" = "no_op";
            "A-_" = "no_op";
            "A-`" = "no_op";
            "A-{" = "no_op";
            "A-|" = "no_op";
            "A-}" = "no_op";
            "A-~" = "no_op";
          };
          # WINDOW mode
          window = no_op_all // {
            C-k = "wclose";
            k = "wclose";

            C-m = "jump_view_left";
            C-n = "jump_view_down";
            C-e = "jump_view_up";
            C-i = "jump_view_right";

            m = "jump_view_left";
            n = "jump_view_down";
            e = "jump_view_up";
            i = "jump_view_right";

            M = "swap_view_left";
            N = "swap_view_down";
            E = "swap_view_up";
            I = "swap_view_right";

            C-b = "vsplit_new";
            b = "vsplit_new";

            # Defaults:
            C-w = "rotate_view";
            w = "rotate_view";

            C-s = "hsplit";
            s = "hsplit";

            C-t = "transpose_view";
            t = "transpose_view";

            f = "goto_file_hsplit";
            F = "goto_file_vsplit";

            C-q = "wclose";
            q = "wclose";

            C-o = "wonly";
            o = "wonly";
          };

          view = no_op_all // {
            E = "page_cursor_half_up";
            N = "page_cursor_half_down";
            C-E = "page_up";
            C-N = "page_down";

            G = "goto_last_line";

            e = "scroll_up";
            n = "scroll_down";

            h = "search_next";
            H = "search_prev";

            # Defaults:
            z = "align_view_center";
            c = "align_view_center";

            t = "align_view_top";
            b = "align_view_bottom";
            m = "align_view_middle";
            C-b = "page_up";
            C-f = "page_down";
            C-u = "page_cursor_half_up";
            C-d = "page_cursor_half_down";
            space = "page_cursor_half_down";

            "/" = "search";
            "?" = "rsearch";
          };

          # NORMAL mode
          normal = no_op_all // {
            E = "page_cursor_half_up";
            N = "page_cursor_half_down";
            C-E = "page_up";
            C-N = "page_down";

            G = "goto_last_line";

            m = "move_char_left";
            n = "move_visual_line_down";
            e = "move_visual_line_up";
            i = "move_char_right";

            M = "goto_line_start";
            I = "goto_line_end";
            "^" = "goto_first_nonwhitespace";
            "$" = "goto_line_end";

            # new mnemonic: s/S = search
            s = "find_next_char";
            S = "find_prev_char";

            # new mnemonic: revise
            v = "replace";
            V = "replace_with_yanked";

            # new mnemonic: f/F = far word/WORD
            f = "move_next_word_end";
            F = "move_next_long_word_end";

            # new mnemonic: r/R = range
            r = "select_mode";

            l = "insert_mode";
            L = "insert_at_line_start";

            C-c = "command_mode";
            C-l = "align_view_top";

            C-u = "kill_to_line_start";
            C-k = "kill_to_line_end";

            h = "search_next";
            H = "search_prev";

            # GOTO mode
            g = no_op_all // {
              m = "goto_line_start";
              i = "goto_line_end";

              I = "goto_implementation";
              M = "goto_last_modified_file";
              s = "select_regex";
              S = "split_selection";

              # Defaults:
              g = "goto_file_start";
              e = "goto_last_line";
              f = "goto_file";
              d = "goto_definition";
              D = "goto_declaration";
              y = "goto_type_definition";
              r = "goto_reference";
              t = "goto_window_top";
              c = "goto_window_center";
              b = "goto_window_bottom";
              a = "goto_last_accessed_file";
              n = "goto_next_buffer";
              p = "goto_previous_buffer";
              "." = "goto_last_modification";
              w = "goto_word";
            };
            # new mnemonic: KNIT mode
            k = no_op_all // {
              k = "match_brackets";
              # Defaults:
              s = "surround_add";
              r = "surround_replace";
              d = "surround_delete";
              a = "select_textobject_around";
              i = "select_textobject_inner";
            };
            # VIEW mode
            z = view;
            # sticky VIEW mode
            Z = view;

            "C-w" = window;

            space = {
              w = window;
              # buffer
              b = {
                b = "buffer_picker";
                r = ":reload";
                s = ":new"; # new scratch buffer
                R = ":reload-all";
              };
              # quit
              q = {
                q = ":write-quit-all";
                Q = ":write-quit-all!";
              };
              # rapid
              r = {
                s = ":write";
              };
              # text manipulation
              x = {
                x = ":reflow";
                l = ":pipe sort";
                L = ":pipe sort --reverse";
                u = ":pipe uniq";
                U = ":pipe uniq --repeated";
                r = ":pipe tac"; # reverse lines
              };
            };

            # Defaults:
            t = "find_till_char";
            T = "till_prev_char";

            "A-." = "repeat_last_motion";

            "~" = "switch_case";
            "`" = "switch_to_lowercase";
            "A-`" = "switch_to_uppercase";

            home = "goto_line_start";
            end = "goto_line_end";

            w = "move_next_word_start";
            W = "move_next_long_word_start";
            b = "move_prev_word_start";
            B = "move_prev_long_word_start";

            j = "goto_line"; # new mnemonic: jump
            ":" = "command_mode";

            a = "append_mode";
            A = "insert_at_line_end";
            o = "open_below";
            O = "open_above";

            d = "delete_selection";
            A-d = "delete_selection_noyank";
            c = "change_selection";
            A-c = "change_selection_noyank";

            C = "copy_selection_on_next_line";
            A-C = "copy_selection_on_prev_line";

            A-s = "split_selection_on_newline";
            A-minus = "merge_selections";
            "A-_" = "merge_consecutive_selections";
            ";" = "collapse_selection";
            "A-;" = "flip_selections";
            A-o = "expand_selection";
            A-i = "shrink_selection";
            A-I = "select_all_children";
            A-p = "select_prev_sibling";
            A-n = "select_next_sibling";
            A-e = "move_parent_node_end";
            A-b = "move_parent_node_start";
            A-a = "select_all_siblings";

            "%" = "select_all";
            x = "extend_line_below";
            X = "extend_to_line_bounds";
            A-x = "shrink_to_line_bounds";

            "/" = "search";
            "?" = "rsearch";
            "*" = "search_selection";

            u = "undo";
            U = "redo";
            A-u = "earlier";
            A-U = "later";

            y = "yank";
            p = "paste_after";
            P = "paste_before";

            Q = "record_macro";
            q = "replay_macro";

            ">" = "indent";
            "<" = "unindent";
            "=" = "format_selections";
            J = "join_selections";
            A-J = "join_selections_space";
            K = "keep_selections";
            A-K = "remove_selections";

            "," = "keep_primary_selection";
            "A-," = "remove_primary_selection";

            "&" = "align_selections";
            "_" = "trim_selections";

            "(" = "rotate_selections_backward";
            ")" = "rotate_selections_forward";
            "A-(" = "rotate_selection_contents_backward";
            "A-)" = "rotate_selection_contents_forward";

            "A-:" = "ensure_selections_forward";

            esc = "normal_mode";
            C-b = "page_up";
            C-f = "page_down";

            C-i = "jump_forward";
            C-o = "jump_backward";
            C-s = "save_selection";

            # TODO rebind these
            "\\" = "select_register";
            "|" = "shell_pipe";
            "A-|" = "shell_pipe_to";
            "!" = "shell_insert_output";
            "A-!" = "shell_append_output";
            # "$" = "shell_keep_pipe";

            C-z = "suspend";

            # My keyboard layers make this easier to reach
            "+" = "increment";
            "minus" = "decrement";
          };
        in
        {
          inherit normal;
          select = normal // {
            E = "page_cursor_half_up";
            N = "page_cursor_half_down";

            h = "extend_search_next";
            H = "extend_search_prev";

            s = "extend_next_char";
            S = "extend_prev_char";

            m = "extend_char_left";
            n = "extend_visual_line_down";
            e = "extend_visual_line_up";
            i = "extend_char_right";

            f = "extend_next_word_end";
            F = "extend_next_long_word_end";

            r = "normal_mode";

            # GOTO mode
            g = no_op_all // {
              n = "extend_line_down";
              e = "extend_line_up";
              w = "extend_to_word";
            };

            # Defaults:
            w = "extend_next_word_start";
            W = "extend_next_long_word_start";

            b = "extend_prev_word_start";
            B = "extend_prev_long_word_start";

            "A-e" = "extend_parent_node_end";
            "A-b" = "extend_parent_node_start";

            t = "extend_till_char";
            T = "extend_till_prev_char";

            "home" = "extend_to_line_start";
            "end" = "extend_to_line_end";
            "esc" = "exit_select_mode";
          };
        };
    };
  };
}
