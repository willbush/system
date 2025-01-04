{ inputs, pkgs, ... }:
{
  # No automatic theme stylix
  stylix.targets.helix.enable = false;

  programs.helix = {
    enable = true;
    defaultEditor = true;
    package = inputs.helix.packages.${pkgs.system}.default.overrideAttrs (self: {
      makeWrapperArgs =
        with pkgs;
        self.makeWrapperArgs or [ ]
        ++ [
          "--suffix"
          "PATH"
          ":"
          (lib.makeBinPath [
            nil
          ])
        ];
    });

    settings = {
      theme = "tokyonight";
      # use bar cursor in insert mode like vim
      editor = {
        cursor-shape.insert = "bar";
        cursorline = true;
        color-modes = true;
      };

      keys =
        let
          # NORMAL mode
          normal = {
            E = "@10e";
            N = "@10n";

            space = {
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
              # toggle
              t = {
                c = "switch_case";
                u = "switch_to_lowercase";
                U = "switch_to_uppercase";
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
  };
}
