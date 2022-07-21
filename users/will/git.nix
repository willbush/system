{ pkgs, ... }: {

  programs = {
    git = {
      enable = true;
      userName = "Will Bush";
      userEmail = "will.g.bush@gmail.com";
      signing = {
        # public key fingerprint
        key = "4441422E61E4C8F3EBFE5E333823864B54B13BDA";
        signByDefault = true;
      };

      extraConfig = {
        core.autocrlf = "input";
        init.defaultBranch = "main";
        merge.conflictstyle = "zdiff3";
        pull.rebase = false;
      };

      difftastic.enable = true;
    };

    ssh = {
      enable = true;
      serverAliveInterval = 30;
      matchBlocks = import ../../secrets/ssh-matchblocks-will.nix;
    };
  };
}
