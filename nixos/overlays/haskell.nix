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
                rev    = "988d5b435390f2391583b09e79304814c35dfd2b";
                sha256 = "0j6zkz9pbs7hkjdlrwl4cisnyd4r1fjbxffx41cz5azri79ff0gl";
                # date = 2019-06-23T19:31:05-05:00;
                }) {});

            multistate = doJailbreak (overrideCabal super.multistate (attrs: { broken = false; }));
            butcher = doJailbreak (overrideCabal super.butcher (attrs: { broken = false; }));
          });
      });
    };
  };

  haskellPackages = self.haskell.packages.ghc864;
}
