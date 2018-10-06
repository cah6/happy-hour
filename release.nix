{ withHoogle ? false
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };

  pinnedHaskellPkgs = pinnedPkgs.haskellPackages;

  customHaskellPackages = pinnedHaskellPkgs.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      backend-servant = self.callCabal2nix "backend-servant" ./backend-servant { };
      common = self.callCabal2nix "common" ./common { };
    });
  });

  hoogleAugmentedPackages = import ./toggle-hoogle.nix { withHoogle = withHoogle; input = customHaskellPackages; };

in
  { project1 = hoogleAugmentedPackages.backend-servant;
  }