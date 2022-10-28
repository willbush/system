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
    delta
    flutter
    git-imerge
    github-changelog-generator
    gitui
    jq # TODO try xq
    nixpkgs-fmt
    python3
    racket
    shfmt
    terraform

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
    openssl

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
