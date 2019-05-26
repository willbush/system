{ config, pkgs, ... }:

let home = builtins.getEnv "HOME";
in
{
  home.stateVersion = "19.03";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.overlays =
    let path = ./overlays; in with builtins;
    map (n: import (path + ("/" + n)))
        (filter (n: match ".*\\.nix" n != null ||
                    pathExists (path + ("/" + n + "/default.nix")))
                (attrNames (readDir path)));

  home.sessionVariables =  {
     EDITOR = "emacsclient --create-frame --alternate-editor emacs";
  };

  home.file = {
    ".emacs.d" = {
      source = ../emacs;
      recursive = true;
      # Since home-manager deploys my config and is immutable, I might as well
      # byte compile everything deployed this way. From my profiling with `esup`
      # I haven't been able to see much of any gain. The main advantage is an
      # opportunity to see compiler warnings.
      onChange = ''
        emacs -Q --load ~/.emacs.d/init.el --batch --eval '(byte-compile-file "~/.emacs.d/init.el")'
        emacs -Q --load ~/.emacs.d/init.el --batch --eval '(byte-recompile-directory "~/.emacs.d/src/" 0 t)'
      '';
    };

    ".config".source = ../config;
    ".config".recursive = true;
    ".xmonad/xmonad.hs".source = ../xmonad/xmonad.hs;
    # Outside of NixOS the dictionary directory needs to be set.
    # https://github.com/NixOS/nixpkgs/issues/4521
    ".aspell.conf".text = ''
       dict-dir ${home}/.nix-profile/lib/aspell
       master en_US
       extra-dicts en-computers.rws
       add-extra-dicts en_US-science.rws
    '';
  };

  xdg = {
    enable = true;
    # Home manager's emacs service doesn't provide a desktop entry for the emacs
    # client. Note the %F on the `Exec=` line passes any file name string to
    # tell emacs to open a file. I just use Albert to launch the emacs client so
    # I don't every really need that.
    dataFile."applications/emacsclient.desktop".text = ''
      [Desktop Entry]
      Name=Emacsclient
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
      Exec=emacsclient -c -a emacs %F
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupWMClass=Emacs
      Keywords=Text;Editor;
    '';
  };

  xresources.properties = {
    # Set some Emacs GUI properties in the .Xresources file because they are
    # expensive to set during initialization in Emacs lisp. This saves about
    # half a second on startup time. See the following link for more options:
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.Font" = "Hack:size=16";
  };

  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    # Compile with imagemagick support so I can resize images.
    package = pkgs.emacs.override { inherit (pkgs) imagemagick; };
    extraPackages = (epkgs: (with epkgs; [
      attrap
      avy
      company
      company-nixos-options
      counsel
      counsel-projectile
      csharp-mode
      dante
      dashboard
      deadgrep
      define-word
      dired-narrow
      direnv
      disk-usage
      doom-modeline
      doom-themes
      esup
      evil
      evil-collection
      evil-exchange
      evil-magit
      evil-matchit
      evil-numbers
      evil-surround
      evil-tutor
      evil-visualstar
      expand-region
      fd-dired
      fill-column-indicator
      flyspell-correct-ivy
      general
      git-timemachine
      golden-ratio
      haskell-mode
      hindent
      hlint-refactor
      hydra
      ivy
      magit
      markdown-mode
      markdown-toc
      nav-flash
      nix-mode
      nix-sandbox
      org-re-reveal
      pdf-tools
      poporg
      powershell
      projectile
      rainbow-delimiters
      ranger
      smex
      use-package
      visual-fill-column
      which-key
      winum
      wttrin
      yaml-mode
    ]));
  };

  # home packages that need no extra configuration
  home.packages = with pkgs; [
    # haskellPackages.hakyll
    albert
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    cabal-install
    cabal2nix
    chromium
    dotnet-sdk
    exa
    feh
    firefox
    fzf
    gcc
    gnome3.simple-scan
    gnupg
    haskellPackages.apply-refact
    haskellPackages.brittany # marked broken but fixed with an overlay
    haskellPackages.ghcid
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    keepassxc
    libreoffice
    nix-prefetch-git
    openconnect
    ranger
    remmina
    rustup
    stack
    tokei
    transmission-gtk
    vlc
  ];

  programs.git = {
    enable = true;
    userName = "willbush";
    userEmail = "will.g.bush@gmail.com";
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      l  = "exa";
      ll = "exa -l";
      la = "exa -lah";
      vim = "nvim";
      dropbox = "docker exec -it dropbox dropbox";
      dropbox-start = ''
      docker run -d --restart=always --name=dropbox \
        -v ${home}/Dropbox:/dbox/Dropbox \
        -v ${home}/.dropbox:/dbox/.dropbox \
        -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox
      '';
    };
    oh-my-zsh = {
      enable = true;
      plugins = ["vi-mode" "web-search"];
      theme = "lambda";
    };
  };

  programs.htop = {
    enable = true;
    meters = {
      left = [ "AllCPUs" "Memory" "Swap" "Battery" ];
      right = [ "Tasks" "LoadAverage" "Uptime" ];
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;
    serverAliveInterval = 60;
    matchBlocks."github.com".identityFile = "~/.ssh/id_rsa_01";
  };

  services.compton = {
    enable = true;
    fade = true;
    backend = "xrender";
    fadeDelta = 1;
    # I only want transparency for a couple of applications.
     opacityRule = [
       "90:class_g ?= 'emacs' && focused"
       "75:class_g ?= 'emacs' && !focused"
       "90:class_g ?= 'alacritty' && focused"
       "75:class_g ?= 'alacritty' && !focused"
     ];
  };

  services.redshift = {
    enable = true;
    latitude = "33";
    longitude = "-97";
    temperature.day = 6500;
    temperature.night = 2700;
  };
}
