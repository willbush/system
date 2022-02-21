{ pkgs, ... }: {

  programs.git = {
    enable = true;
    userName = "sonia-yb";
    userEmail = "sonia.y.santos@gmail.com";

    extraConfig = {
      init.defaultBranch = "main";
      merge.conflictstyle = "diff3";
      pull.rebase = false;
    };

    # enables https://github.com/dandavison/delta
    delta.enable = true;
  };
}
