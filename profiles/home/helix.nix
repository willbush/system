{ inputs, pkgs, ... }:
let
  inherit (pkgs) writeScriptBin;
  get-git-url = writeScriptBin "get-git-url" (builtins.readFile ./scripts/get-get-url.fish);
in
{
  # No automatic theme stylix
  stylix.targets.helix.enable = false;

  # custom scripts I use in helix
  home.packages = [
    get-git-url
  ];

  programs.helix = {
    enable = true;
    defaultEditor = true;
    package = inputs.helix.packages.${pkgs.system}.default;

    settings = {
      # Some of me favorite themes:
      #
      # ao
      # ashen
      # beans
      # carbonfox
      # catppuccin
      # doom-one
      # dracula_at_night
      # github_dark_tritanopia
      # hex
      # kanagawa
      # material_deep_ocean
      # nightfox
      # night_owl
      # onedarker
      # poimandres
      # rose_pine
      # snazzy
      # tokyonight
      theme = "rose_pine";
      # use bar cursor in insert mode like vim
      editor = {
        color-modes = true;
        continue-comments = false;
        cursorline = true;
        cursor-shape.insert = "bar";
        # Favor the home row of Colemak-dh first, non-pinky finger keys second.
        jump-label-alphabet = "ntesiroamgjblpufwdhcyxzq";
        line-number = "relative";
        trim-final-newlines = true;
        trim-trailing-whitespace = true;

        # 5 is supposed to be instant
        completion-timeout = 5;

        # Minimum severity to show a diagnostic after the end of a line:
        end-of-line-diagnostics = "disable";

        inline-diagnostics = {
          # Minimum severity to show a diagnostic on the primary cursor's line.
          # Note that `cursor-line` diagnostics are hidden in insert mode.
          cursor-line = "hint";
          # Minimum severity to show a diagnostic on other lines:
          other-lines = "disable";
        };
        # don't ignore hidden files. hx ignores .gitignore files and that should be enough.
        file-picker.hidden = false;
        soft-wrap = {
          # false is default, but must be set explicitly whe modifying other settings:
          # https://github.com/helix-editor/helix/issues/12512
          enable = false;
          # hide indicator. Can tell it's wrapping b/c of the lack of line-number.
          wrap-indicator = "";
        };
        lsp = {
          display-messages = true;
          display-progress-messages = true;
        };
        smart-tab.enable = false;
      };

      keys =
        let
          # NORMAL mode
          normal = {
            E = "@10e";
            N = "@10n";

            C-q = ":write-quit-all!";

            # [goto] definition other window
            g.o = "@<C-w>o<C-w>vgd";

            space = {
              space = "command_mode";

              # buffer
              b = {
                b = "buffer_picker";
                r = ":reload";
                R = ":reload-all";

                s = ":open /tmp/scratch";
                # Using "messages" as mnemonic (habbit from Emacs)
                m = ":log-open";
                n = ":new";

                a = ":buffer-close-all";
                A = ":buffer-close-all!";
                d = ":buffer-close";
                D = ":buffer-close!";
                y = ":yank-diagnostic";
              };
              f = {
                # Yank document path to `"` register.
                # NOTE:` rc` (SPC r c) maps to copy_between_registers
                y = "@ rc%\"";
                # Yank document path to system clipboard register.
                Y = "@ rc%+";
              };
              # git
              g = {
                B = ":echo %sh{git blame -L %{cursor_line},+1 %{buffer_name}}";
                o = ":sh get-git-url %{buffer_name} %{cursor_line} | wl-copy";
              };
              # quit
              q = ":write-quit-all!";
              # rapid
              r = {
                s = ":update";
                c = "copy_between_registers";
              };
              # toggle
              t = {
                c = "switch_case";
                d = {
                  # diagnostics
                  e = ":toggle end-of-line-diagnostics disable hint";
                  l = ":toggle inline-diagnostics.cursor-line disable hint";
                  L = ":toggle inline-diagnostics.other-lines disable hint";
                };
                f = {
                  h = ":toggle-option file-picker.hidden";
                  g = ":toggle-option file-picker.git-ignore";
                  x = ":toggle-option file-picker.git-exclude";
                };
                l = ":toggle soft-wrap.enable";
                n = ":toggle line-number absolute relative";
                w = ":toggle whitespace.render all none";
              };
              u = "switch_to_lowercase";
              U = "switch_to_uppercase";
              # text manipulation
              x = {
                x = ":reflow";
                l = ":pipe sort";
                L = ":pipe sort --reverse";
                u = ":pipe uniq";
                U = ":pipe uniq --repeated";
                r = ":pipe tac"; # reverse lines
              };
              # Open yazi directly within helix. Note fzf doesn't work for some reason.
              # e is for `file_explorer` which these (`e` and `E`) bind over the default.
              e = [
                ":sh rm -f /tmp/hx-yazi-picker1"
                ":insert-output yazi %{buffer_name} --chooser-file=/tmp/hx-yazi-picker1"
                ":insert-output echo '\x1b[?1049h' > /dev/tty"
                # Doesn't support opening multiple files.
                ":open %sh{cat /tmp/hx-yazi-picker1 | head -n1}"
                ":redraw"
                ":set mouse false"
                ":set mouse true"
              ];
              E = [
                ":sh rm -f /tmp/hx-yazi-picker2"
                ":insert-output yazi %{workspace_directory} --chooser-file=/tmp/hx-yazi-picker2"
                ":insert-output echo '\x1b[?1049h' > /dev/tty"
                # Doesn't support opening multiple files.
                ":open %sh{cat /tmp/hx-yazi-picker2 | head -n1}"
                ":redraw"
                ":set mouse false"
                ":set mouse true"
              ];
            };
          };
        in
        {
          inherit normal;
          select = normal;
        };
    };

    languages = {
      language-server.rust-analyzer = {
        config.check.command = "clippy";
      };

      language = [
        {
          name = "lua";
          auto-format = true;
          formatter = {
            command = "stylua";
            args = [ "-" ];
          };
        }
        {
          name = "nix";
          auto-format = true;
          formatter = {
            command = "nixfmt";
            args = [ ];
          };
        }
      ];
    };
  };
}
