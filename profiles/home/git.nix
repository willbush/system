{ config, ... }:
{
  sops = {
    secrets."work/ssh/host" = { };
    secrets."work/ssh/user" = { };

    templates."ssh-work-config".content = ''
      Host ${config.sops.placeholder."work/ssh/host"}
        User ${config.sops.placeholder."work/ssh/user"}
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
        disableStartupPopups = true;
        git.paging = {
          colorArg = "always";
          pager = "delta --dark --paging=never";
        };
        keybinding = {
          universal = {
            prevItem-alt = "e";
            nextItem-alt = "n";
            scrollLeft = "M";
            scrollRight = "I";
            prevBlock-alt = "m";
            nextBlock-alt = "i";

            nextMatch = "h";
            prevMatch = "H";

            new = "<c-n>";
            edit = "<c-e>";

            createRebaseOptionsMenu = "R";
          };
          files = {
            openMergeTool = "T";
            ignoreFile = "<c-x>";
          };
          branches = {
            viewGitFlowOptions = "G";
          };
          commits = {
            moveDownCommit = "<c-n>";
            moveUpCommit = "<c-e>";
            startInteractiveRebase = "<c-b>";
          };
          submodules = {
            init = "s";
          };
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
