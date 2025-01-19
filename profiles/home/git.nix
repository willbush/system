{ config, ... }:
{
  sops = {
    secrets."ssh/work/host" = { };
    secrets."ssh/work/user" = { };

    templates."ssh-work-config".content = ''
      Host ${config.sops.placeholder."ssh/work/host"}
        User ${config.sops.placeholder."ssh/work/user"}
        ForwardAgent yes
        ServerAliveInterval 120
        IdentityFile ~/.ssh/id_ed25519_github
    '';
  };

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

    lazygit = {
      enable = true;
      settings = {
        git.paging = {
          colorArg = "always";
          pager = "delta --dark --paging=never";
        };
      };
    };

    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";
        prompt = "enabled";
      };
    };

    ssh = {
      enable = true;
      addKeysToAgent = "yes";

      includes = [ config.sops.templates."ssh-work-config".path ];

      matchBlocks = {
        "github.com" = {
          identityFile = "~/.ssh/id_ed25519_github";
        };
        "gitlab.com" = {
          identityFile = "~/.ssh/id_ed25519_gitlab";
        };
      };
    };
  };
  services.ssh-agent.enable = true;
}
