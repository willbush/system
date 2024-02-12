{ compiler ? "ghc884"
, doBenchmark ? false
, doProfiling ? false
, doStrict ? false
}:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowUnfree = false;
    config.allowBroken = false;
    overlays = [
      (self: super: {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ${compiler} = super.haskell.packages.${compiler} // rec {
              ghc = super.haskell.packages.${compiler}.ghc // {
                withPackages =
                  super.haskell.packages.${compiler}.ghc.withHoogle;
              };
              ghcWithPackages = ghc.withPackages;
            };
          };
        };
      })
    ];
  };
  haskellPkgs = pkgs.haskell.packages.${compiler};

in haskellPkgs.developPackage rec {
  name = builtins.baseNameOf ./.;
  # Must filter root otherwise `src = ./.` and lorri will get its known issue
  # where it rebuilds on any file change
  # (https://github.com/target/lorri/issues/6). When the root is filtered we
  # must provide a name (above) because the default implementation uses:
  # `builtins.baseNameOf root` which results in a name that refers to a store
  # path and end up with the error: "is not allowed to refer to a store path".
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  modifier = drv:
    pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or [ ]) ++ [
        haskellPkgs.cabal-install
        haskellPkgs.ghc
        pkgs.xorg.libX11
        pkgs.xorg.libXinerama
        pkgs.xorg.libXrandr
      ];

      enableLibraryProfiling = doProfiling;
      enableExecutableProfiling = doProfiling;

      testHaskellDepends = (attrs.testHaskellDepends or [ ]) ++ [
        # avoid installing this as a system wide tool due to:
        # https://github.com/digital-asset/ghcide/issues/538
        haskellPkgs.haskell-language-server
        haskellPkgs.hlint
        sources.niv
      ];

      inherit doBenchmark;

      configureFlags =
        pkgs.stdenv.lib.optional doStrict "--ghc-options=-Werror";

      passthru = {
        nixpkgs = pkgs;
        inherit haskellPkgs;
      };
    });

  returnShellEnv = false;
}
