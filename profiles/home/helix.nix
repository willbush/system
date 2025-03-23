{ inputs, pkgs, ... }:
let
  inherit (pkgs) writeScriptBin;
  get-git-url = writeScriptBin "get-git-url" (builtins.readFile ./scripts/get-get-url.fish);
  yazi-file-picker = writeScriptBin "yazi-file-picker" (
    builtins.readFile ./scripts/yazi-file-picker.sh
  );
in
{
  # No automatic theme stylix
  stylix.targets.helix.enable = false;

  # custom scripts I use in helix
  home.packages = [
    get-git-url
    yazi-file-picker
  ];

  programs.helix = {
    enable = true;
    defaultEditor = true;
    package = inputs.helix.packages.${pkgs.system}.default;

    settings = {
      # Some of me favorite themes:
      #
      # ao
      # beans
      # carbonfox
      # catppuccin
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
        cursorline = true;
        cursor-shape.insert = "bar";
        # Favor the home row of Colemak-dh first, non-pinky finger keys second.
        jump-label-alphabet = "ntesiroamgjblpufwdhcyxzq";
        line-number = "relative";
        trim-final-newlines = true;
        trim-trailing-whitespace = true;

        # Minimum severity to show a diagnostic after the end of a line:
        end-of-line-diagnostics = "hint";

        inline-diagnostics = {
          # Minimum severity to show a diagnostic on the primary cursor's line.
          # Note that `cursor-line` diagnostics are hidden in insert mode.
          cursor-line = "hint";
          # Minimum severity to show a diagnostic on other lines:
          other-lines = "disable";
        };
      };

      keys =
        let
          # NORMAL mode
          normal = {
            E = "@10e";
            N = "@10n";

            C-q = ":write-quit-all!";

            space = {
              space = "command_mode";

              # buffer
              b = {
                b = "buffer_picker";
                r = ":reload";
                R = ":reload-all";

                s = ":open /tmp/scratch";
                n = ":new";

                a = ":buffer-close-all";
                A = ":buffer-close-all!";
                d = ":buffer-close";
                D = ":buffer-close!";
                y = ":yank-diagnostic";
              };
              # git
              g = {
                B = ":echo %sh{git blame -L %{cursor_line},+1 %{buffer_name}}";
                b = ":sh wezterm cli spawn --new-window --cwd $PWD tig blame %{buffer_name} +%{cursor_line} > /dev/null 2>&1";
                o = ":sh get-git-url %{buffer_name} %{cursor_line} | wl-copy";
              };
              # quit
              q = ":write-quit-all!";
              # rapid
              r = {
                s = ":update";
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
              # yazi directly within helix. fzf doesn't work for some reason.
              # https://github.com/sxyazi/yazi/pull/2461
              y = [
                ":sh rm -f /tmp/hx-yazi-picker"
                ":insert-output yazi %{buffer_name} --chooser-file=/tmp/hx-yazi-picker"
                ":insert-output echo '\x1b[?1049h\x1b[?2004h' > /dev/tty"
                # Doesn't support opening multiple files.
                ":open %sh{cat /tmp/hx-yazi-picker | head -n1}"
                ":redraw"
                ":set mouse false"
                ":set mouse true"
              ];
              Y = ":sh wezterm cli spawn --new-window --cwd $PWD -- yazi-file-picker $WEZTERM_PANE > /dev/null 2>&1";
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

      language-server.lsp-ai = {
        command = "lsp-ai";
        config = {
          memory.file_store = { };
          models.model1 = {
            type = "open_ai";
            chat_endpoint = "https://openrouter.ai/api/v1/chat/completions";
            model = "anthropic/claude-3.7-sonnet";
            auth_token_env_var_name = "OPENROUTER_API_KEY";
          };
          models.model2 = {
            type = "ollama";
            model = "deepseek-r1:32b";
          };
          completion = {
            model = "model2";
            parameters = {
              max_tokens = 64;
              max_context = 1024;
            };
          };
          chat = [
            {
              trigger = "!C";
              action_display_name = "Chat";
              model = "model2";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a helpful assistant.";
              };
            }
          ];
        };
      };

      language = [
        {
          name = "markdown";
          language-servers = [ "lsp-ai" ];
        }
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
