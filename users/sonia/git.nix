{ pkgs, ... }: {

  programs = {
    git = {
      enable = true;
      userName = "sonia-yb";
      userEmail = "sonia.y.santos@gmail.com";

      extraConfig = {
        core.autocrlf = "input";
        init.defaultBranch = "main";
        merge.conflictstyle = "zdiff3";
        pull.rebase = false;
        push.autoSetupRemote = true;
      };

      # enables https://github.com/dandavison/delta
      delta.enable = true;
    };

    ssh = {
      enable = true;
      serverAliveInterval = 30;
      matchBlocks = import ../../secrets/ssh-matchblocks-sonia.nix;
    };
  };
}
