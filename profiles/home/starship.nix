{ pkgs, ... }:
{
  programs.starship = {
    enable = true;
    settings =
      let
        format = "[[ $symbol ($version) ](fg:#769ff0 bg:#212736)]($style)";
        style = "bg:#212736";
        languages = {
          aws = "";
          buf = "";
          c = "";
          conda = "";
          crystal = "";
          dart = "";
          docker_context = "";
          elixir = "";
          elm = "";
          fennel = "";
          fossil_branch = "";
          git_branch = "";
          golang = "";
          guix_shell = "";
          haskell = "";
          haxe = "";
          hg_branch = "";
          java = "";
          julia = "";
          kotlin = "";
          lua = "";
          memory_usage = "󰍛";
          meson = "󰔷";
          nim = "󰆥";
          nix_shell = "";
          nodejs = "";
          ocaml = "";
          package = "󰏗";
          perl = "";
          php = "";
          pijul_channel = "";
          python = "";
          rlang = "󰟔";
          ruby = "";
          rust = "󱘗";
          scala = "";
          swift = "";
          zig = "";
        };
      in
      {
        format = "[░▒▓](#a3aed2)[  ](bg:#a3aed2 fg:#090c0c)[](bg:#769ff0 fg:#a3aed2)$directory[](fg:#769ff0 bg:#394260)$git_branch$git_status[](fg:#394260 bg:#212736)$nodejs$rust$golang$php$gcloud$conda[](fg:#212736 bg:#1d2230)$time[ ](fg:#1d2230)\n$character";

        directory = {
          style = "fg:#e3e5e5 bg:#769ff0";
          format = "[ $path ]($style)";
          truncation_length = 3;
          truncation_symbol = "…/";
          substitutions = {
            "documents" = "󰈙";
            "downloads" = "";
            "music" = "";
            "pictures" = "";
            "images" = "";
          };
        };

        git_branch = {
          symbol = "";
          style = "bg:#394260";
          format = "[[ $symbol $branch ](fg:#769ff0 bg:#394260)]($style)";
        };

        git_status = {
          style = "bg:#394260";
          format = "[[($all_status$ahead_behind )](fg:#769ff0 bg:#394260)]($style)";
        };

        time = {
          disabled = false;
          time_format = "%F %T";
          style = "bg:#1d2230";
          format = "[[  $time ](fg:#a0a9cb bg:#1d2230)]($style)";
        };
      }
      // pkgs.lib.attrsets.genAttrs (builtins.attrNames languages) (name: {
        symbol = languages.${name};
        inherit style format;
      });
  };
}
