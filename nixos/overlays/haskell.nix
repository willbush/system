self: pkgs:

let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  overrideCabal = pkgs.haskell.lib.overrideCabal;
in {
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc864 = pkgs.haskell.packages.ghc864.override (oldArgs: {
        overrides = self.lib.composeExtensions (oldArgs.overrides or (_: _: {}))
          (self: super: {

            brittany = doJailbreak (self.callCabal2nix "brittany"
              (pkgs.fetchFromGitHub {
                owner  = "lspitzner";
                repo   = "brittany";
                rev    = "6c187da8f8166d595f36d6aaf419370283b3d1e9";
                sha256 = "0nmnxprbwws3w1sh63p80qj09rkrgn9888g7iim5p8611qyhdgky";
                }) {});

            multistate = doJailbreak (overrideCabal super.multistate (attrs: { broken = false; }));
            butcher = doJailbreak (overrideCabal super.butcher (attrs: { broken = false; }));
          });
      });
    };
  };

  haskellPackages = self.haskell.packages.ghc864;
}
