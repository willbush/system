{
  config,
  lib,
  pkgs,
  ...
}:

let
  ## downgrade until next release: https://github.com/jesseduffield/lazygit/issues/4770
  lazygit-downgraded = pkgs.lazygit.overrideAttrs (oldAttrs: rec {
    pname = "lazygit";
    version = "0.52.0";
    src = pkgs.fetchFromGitHub {
      owner = "jesseduffield";
      repo = "lazygit";
      rev = "v0.52.0";
      sha256 = "sha256-tbFRovaB0f+0VyX34DEXvWYjV3fipc5kbRNhm7rVMlo=";
    };
    ldflags = [
      "-X main.version=${version}"
      "-X main.buildSource=nix"
    ];
  });
in
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

  # from `mergiraf languages --gitattributes`
  home.file.".config/git/attributes".text = ''
    *.java merge=mergiraf
    *.kt merge=mergiraf
    *.rs merge=mergiraf
    *.go merge=mergiraf
    *.js merge=mergiraf
    *.jsx merge=mergiraf
    *.mjs merge=mergiraf
    *.json merge=mergiraf
    *.yml merge=mergiraf
    *.yaml merge=mergiraf
    *.toml merge=mergiraf
    *.html merge=mergiraf
    *.htm merge=mergiraf
    *.xhtml merge=mergiraf
    *.xml merge=mergiraf
    *.c merge=mergiraf
    *.h merge=mergiraf
    *.cc merge=mergiraf
    *.cpp merge=mergiraf
    *.hpp merge=mergiraf
    *.cs merge=mergiraf
    *.dart merge=mergiraf
    *.dts merge=mergiraf
    *.scala merge=mergiraf
    *.sbt merge=mergiraf
    *.ts merge=mergiraf
    *.tsx merge=mergiraf
    *.py merge=mergiraf
    *.php merge=mergiraf
    *.phtml merge=mergiraf
    *.sol merge=mergiraf
    *.lua merge=mergiraf
    *.rb merge=mergiraf
    *.nix merge=mergiraf
    *.sv merge=mergiraf
    *.svh merge=mergiraf
  '';

  programs = {
    git = {
      enable = true;
      userName = "Will Bush";
      userEmail = "git@willbush.dev";

      extraConfig = {
        core.autocrlf = "input";
        init.defaultBranch = "main";
        merge.conflictstyle = "zdiff3";
        pull.rebase = false;
        push.autoSetupRemote = true;
        # https://mergiraf.org/usage.html#registration-as-a-git-merge-driver
        merge.mergiraf = {
          name = "mergiraf";
          driver = "${lib.getExe pkgs.mergiraf} merge --git %O %A %B -s %S -x %X -y %Y -p %P -l %L";
        };
      };

      delta.enable = true;
    };

    lazygit = {
      enable = true;
      package = lazygit-downgraded;
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
