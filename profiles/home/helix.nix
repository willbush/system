{ ... }:
{
  # No automatic theme stylix
  stylix.targets.helix.enable = false;

  programs.helix = {
    enable = true;

    settings = {
      theme = "rose_pine";
      # use bar cursor in insert mode like vim
      editor = {
        cursor-shape.insert = "bar";
        cursorline = true;
      };

      # Custom COLEMAK-DHm based keybindings for helix.
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
          # WINDOW mode
          window = {
            C-h = "no_op";
            C-j = "no_op";
            C-l = "no_op";

            h = "no_op";
            j = "no_op";
            l = "no_op";

            H = "no_op";
            J = "no_op";
            K = "no_op";
            L = "no_op";

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

          view = {
            k = "no_op";
            j = "no_op";

            E = "page_cursor_half_up";
            N = "page_cursor_half_down";

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
          normal = {
            j = "no_op";
            R = "no_op";
            E = "page_cursor_half_up";
            N = "page_cursor_half_down";

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

            h = "search_next";
            H = "search_prev";

            # GOTO mode
            g = {
              h = "no_op";
              j = "no_op";
              k = "no_op";
              l = "no_op";

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
            k = {
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
            space.w = window;

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

            G = "goto_line";
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
            C-u = "page_cursor_half_up";
            C-d = "page_cursor_half_down";

            C-i = "jump_forward";
            "tab" = "jump_forward";
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
            j = "no_op";
            l = "no_op";
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
            g = {
              j = "no_op";
              k = "no_op";
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
