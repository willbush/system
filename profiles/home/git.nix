{ ... }:
{

  programs = {
    git = {
      enable = true;
      userName = "Will Bush";
      userEmail = "git@willbush.dev";

      extraConfig = {
        blame.ignoreRevsFile = ".git-blame-ignore-revs";
        core.autocrlf = "input";
        init.defaultBranch = "main";
        merge.conflictstyle = "zdiff3";
        pull.rebase = false;
        push.autoSetupRemote = true;
      };

      delta.enable = true;
    };

    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";
        prompt = "enabled";
      };
    };
    gitui.enable = true;
  };
}
