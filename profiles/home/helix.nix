{ inputs, pkgs, ... }:
{
  # No automatic theme stylix
  stylix.targets.helix.enable = false;

  programs.helix = {
    enable = true;
    defaultEditor = true;
    package = inputs.helix.packages.${pkgs.system}.default;

    settings = {
      theme = "tokyonight";
      # use bar cursor in insert mode like vim
      editor = {
        color-modes = true;
        cursorline = true;
        cursor-shape.insert = "bar";
        line-number = "relative"; 

        # Minimum severity to show a diagnostic after the end of a line:
        end-of-line-diagnostics = "hint";

        inline-diagnostics = {
          # Minimum severity to show a diagnostic on the primary cursor's line.
          # Note that `cursor-line` diagnostics are hidden in insert mode.
          cursor-line = "hint";
          # Minimum severity to show a diagnostic on other lines:
          other-lines = "hint";
        };
      };

      keys =
        let
          # NORMAL mode
          normal = {
            E = "@10e";
            N = "@10n";

            ret = "goto_word";

            space = {
              space = "command_mode";

              # buffer
              b = {
                b = "buffer_picker";
                r = ":reload";
                s = ":new"; # new scratch buffer
                R = ":reload-all";

                A = ":buffer-close-all!";
                a = ":buffer-close-all";
                D = ":buffer-close!";
                d = ":buffer-close";
              };
              # quit
              q = {
                q = ":write-quit-all";
                Q = ":write-quit-all!";
              };
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
                n = ":toggle line-number absolute relative";
                u = "switch_to_lowercase";
                U = "switch_to_uppercase";
                w = ":toggle whitespace.render all none";
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
            model = "anthropic/claude-3.5-sonnet:beta";
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
