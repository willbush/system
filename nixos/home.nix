{ config, pkgs, ... }:

let home = builtins.getEnv "HOME";
    nixos18_09 = import (builtins.fetchTarball
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
    aspellDicts.en-computers
    aspellDicts.en-science
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
    haskellPackages.hakyll
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
