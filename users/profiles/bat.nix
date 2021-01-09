{ pkgs, ... }: {

  programs.bat = {
    enable = true;
    config.theme = "TwoDark";
  };

  home = {
    packages = with pkgs; [
      bat-extras.batdiff # pretty print git diff (see: https://github.com/eth-p/bat-extras)
      bat-extras.batgrep # pretty print ripgrep output
      bat-extras.prettybat # formats and then bats output (useful for minified code)
    ];
    sessionVariables = {
      # https://github.com/sharkdp/bat#man
      MANPAGER = "sh -c 'col -bx | ${pkgs.bat}/bin/bat -l man -p'";
    };
  };
}
