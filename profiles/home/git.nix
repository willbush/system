{
  config,
  lib,
  pkgs,
  ...
}:

let
  my-git-user = {
    email = "git@willbush.dev";
    name = "Will Bush";
  };
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
    *.properties merge=mergiraf
    *.kt merge=mergiraf
    *.rs merge=mergiraf
    *.go merge=mergiraf
    go.mod merge=mergiraf
    go.sum merge=mergiraf
    *.ini merge=mergiraf
    *.js merge=mergiraf
    *.jsx merge=mergiraf
    *.mjs merge=mergiraf
    *.json merge=mergiraf
    *.yml merge=mergiraf
    *.yaml merge=mergiraf
    pyproject.toml merge=mergiraf
    *.toml merge=mergiraf
    *.html merge=mergiraf
    *.htm merge=mergiraf
    *.xhtml merge=mergiraf
    *.xml merge=mergiraf
    *.c merge=mergiraf
    *.h merge=mergiraf
    *.cc merge=mergiraf
    *.hh merge=mergiraf
    *.cpp merge=mergiraf
    *.hpp merge=mergiraf
    *.cxx merge=mergiraf
    *.hxx merge=mergiraf
    *.c++ merge=mergiraf
    *.h++ merge=mergiraf
    *.mpp merge=mergiraf
    *.cppm merge=mergiraf
    *.ixx merge=mergiraf
    *.tcc merge=mergiraf
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
    *.ex merge=mergiraf
    *.exs merge=mergiraf
    *.nix merge=mergiraf
    *.sv merge=mergiraf
    *.svh merge=mergiraf
    *.md merge=mergiraf
    *.hcl merge=mergiraf
    *.tf merge=mergiraf
    *.tfvars merge=mergiraf
    *.ml merge=mergiraf
    *.mli merge=mergiraf
    *.hs merge=mergiraf
    *.mk merge=mergiraf
    Makefile merge=mergiraf
    GNUmakefile merge=mergiraf
    *.bzl merge=mergiraf
    *.bazel merge=mergiraf
    BUILD merge=mergiraf
    WORKSPACE merge=mergiraf
    *.cmake merge=mergiraf
    CMakeLists.txt merge=mergiraf
  '';

  programs = {
    jujutsu = {
      enable = true;
      settings = {
        user = my-git-user;
        ui = {
          default-command = "log";
          diff-formatter = [
            "difft"
            "--color=always"
            "$left"
            "$right"
          ];
        };
      };
    };
    git = {
      enable = true;
      settings = {
        user = {
          name = my-git-user.name;
          email = my-git-user.email;
        };
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
        alias = {
          dl = "-c diff.external=difft log -p --ext-diff";
          ds = "-c diff.external=difft show --ext-diff";
          df = "-c diff.external=difft diff";
        };
      };
    };

    delta = {
      enable = true;
      # Use delta by default, but difft via alias
      enableGitIntegration = true;
      options = {
        line-numbers = true;
        side-by-side = true;
      };
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
      # disable the old, deprecated default behavior. (fixing a warning)
      enableDefaultConfig = false;
      includes = [ config.sops.templates."ssh-work-config".path ];

      matchBlocks = {
        "*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 120;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          # connection multiplexing
          controlMaster = "auto";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "10m";
        };
        "github.com" = {
          addKeysToAgent = "yes";
          identityFile = "~/.ssh/id_ed25519_github";
        };
        "gitlab.com" = {
          addKeysToAgent = "yes";
          identityFile = "~/.ssh/id_ed25519_gitlab";
        };
      };
    };
  };
  services.ssh-agent.enable = true;
}
