{ pkgs, ... }: {
  home.packages = with pkgs; [
    # core
    curl
    exa
    fd
    ripgrep
    tree
    wget

    # dev
    android-tools
    clang
    clang-tools
    github-changelog-generator
    nixpkgs-fmt
    python3
    racket
    shfmt

    # node
    nodePackages.expo-cli
    nodePackages.markdown-link-check
    nodePackages.npm
    nodePackages.prettier
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodejs

    # security
    git-crypt
    gopass
    mkpasswd

    # compressor / archiver packages
    p7zip
    unar
    unzip
    zip

    # tui utils
    bottom
    btop
    glances
    procs
    zenith

    # other utils
    onefetch
    du-dust
    file
    hyperfine # benchmarking tool
    mutagen
    pdfgrep
    tealdeer # tldr in Rust
    trash-cli
    usbutils
    xorg.xkill
  ];
}
