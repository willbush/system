{ config, pkgs, ... }:

let nixos18_09 =
  import (builtins.fetchTarball
    https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz) { };
in
{
  home.stateVersion = "19.03";

  # nixpkgs.config = {
  #   packageOverrides = pkgs: {
  #     stable = nixos18_09;
  #   };
  # };

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
    ".stack/config.yaml".source = ../stack/config.yaml;
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
      hasky-stack
      hindent
      hlint-refactor
      hydra
      ivy
      magit
      markdown-mode
      markdown-toc
      nix-mode
      nix-sandbox
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
    albert
    aspell
    aspellDicts.en
    chromium
    exa
    feh
    firefox
    fzf
    gcc
    gnupg
    htop
    keepassxc
    libreoffice
    ranger
    rustup
    tokei
    vlc
    # Haskell packages:
    cabal-install
    cabal2nix
    haskellPackages.apply-refact
    haskellPackages.ghcid
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    nix-prefetch-git
    # haskellPackages.brittany
    # stable.haskellPackages.brittany
    stack
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "willbush";
    userEmail = "will.g.bush@gmail.com";
    extraConfig = ''
      [credential]
      helper = cache --timeout=3600
    '';
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
        -v /home/will/Dropbox:/dbox/Dropbox \
        -v /home/will/.dropbox:/dbox/.dropbox \
        -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox'';
    };
    oh-my-zsh = {
      enable = true;
      plugins = ["vi-mode" "web-search"];
      theme = "agnoster";
    };
  };

  services.compton = {
    enable = true;
    fade = true;
    backend = "xrender";
    fadeDelta = 1;
    inactiveOpacity = "0.75";
    activeOpacity = "0.90";
    opacityRule = [
      "99:name *= 'Firefox'"
      "99:name *= 'Chromium'"
      "99:name *= 'VLC'"
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
