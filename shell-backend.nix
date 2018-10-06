{ lib ? (import <nixpkgs> {}).pkgs.lib
}:
let 
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };
  # for elm 18.0
  # elmPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-03.json; };
  myPackages = (import ./release.nix { withHoogle = true; } );

  projectDrvEnv = myPackages.project1.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ 
      pinnedPkgs.haskellPackages.hlint
      pinnedPkgs.haskellPackages.cabal-install
      pinnedPkgs.haskellPackages.hsimport
      pinnedPkgs.elasticsearch5
      # elmPkgs.elmPackages.elm
      ];
    shellHook = ''
      export PGDATA="./pgsql/data"
    '';
  });
in 
  projectDrvEnv