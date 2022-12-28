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
    gh
    github-changelog-generator
    gitui
    helix
    nixpkgs-fmt
    nodePackages.prettier
    python3
    racket
    rustup
    shfmt
    terraform
    xq # jq in rust

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
    onefetch # neofetch like tool
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
