{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
    let

      forAllSystems =
        function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
        ] (system: function nixpkgs.legacyPackages.${system});

    in
    {

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskell.packages.ghc912.shellFor {
          packages = hpkgs: [
            # reuse the nixpkgs for this package
            hpkgs.distribution-nixpkgs
            # call our generated Nix expression manually
            (hpkgs.callPackage ./nix/pkg.nix { })
          ];

          # development tools we use
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            pkgs.hpack
            pkgs.haskell.packages.ghc912.haskell-language-server
          ];

          # Extra arguments are added to mkDerivation's arguments as-is.
          # Since it adds all passed arguments to the shell environment,
          # we can use this to set the environment variable the `Paths_`
          # module of distribution-nixpkgs uses to search for bundled
          # files.
          # See also: https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code
          distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
        };

      });

    };

}
