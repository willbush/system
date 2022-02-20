{ pkgs, ... }: {

  programs = {
    git = {
      enable = true;
      userName = "willbush";
      userEmail = "will.g.bush@gmail.com";
      signing = {
        # public key fingerprint
        key = "4441422E61E4C8F3EBFE5E333823864B54B13BDA";
        signByDefault = true;
      };

      extraConfig = {
        merge.conflictstyle = "diff3";
        pull.rebase = false;
      };

      # enables https://github.com/dandavison/delta
      delta.enable = true;
    };

    ssh = {
      enable = true;
      serverAliveInterval = 30;
      matchBlocks = import ../../secrets/ssh-matchblocks.nix;
    };
  };
}
