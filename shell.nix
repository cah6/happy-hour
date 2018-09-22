{ lib ? (import <nixpkgs> {}).pkgs.lib
}:
let 
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };
  haskellPackages = (import ./release.nix { withHoogle = true; } );

  projectDrvEnv = haskellPackages.project1.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ 
      pinnedPkgs.haskellPackages.hlint
      pinnedPkgs.haskellPackages.cabal-install
      pinnedPkgs.haskellPackages.hsimport
      ];
  });
in 
  projectDrvEnv