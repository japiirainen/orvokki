{ compiler ? "ghc8107" }:

let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/21a3136d25e1652cb32197445e9799e6a5154588.tar.gz";
    sha256 = "145d474g6dngvaiwq2whqdvaq14ba9pc5pvvcz4x8l2bkwbyn3hg";
  };

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
          overrides =
            let
              oldOverrides = old.overrides or (_: _: {});

              manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                orvokki = pkgsNew.haskell.lib.dontCheck haskellPackagesOld.orvokki;

                haskeline = haskellPackagesNew.haskeline_0_8_2;

                prettyprinter-ansi-terminal =
                  pkgsNew.haskell.lib.dontCheck haskellPackagesOld.prettyprinter-ansi-terminal;

                vector =
                  pkgsNew.haskell.lib.dontCheck haskellPackagesOld.vector;
              };

              sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
                orvokki = ./.;
              };

            in
              pkgsNew.lib.fold pkgsNew.lib.composeExtensions oldOverrides
                (   [ sourceOverrides ]
                ++  pkgsNew.lib.optional (compiler == "ghcjs") manualOverrides
                );
        });
      };
    
  };

  config.allowBroken = true;

  pkgs = import nixpkgs { inherit config; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskell.packages."${compiler}") orvokki; }