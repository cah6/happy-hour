{ withHoogle ? false
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };

  pinnedHaskellPkgs = pinnedPkgs.haskellPackages;

  customHaskellPackages = pinnedHaskellPkgs.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      backend-servant = self.callCabal2nix "backend-servant" ./backend-servant { };
      common-types = self.callCabal2nix "common-types" ./common-types { };
    });
  });

  hoogleAugmentedPackages = import ./toggle-hoogle.nix { withHoogle = withHoogle; input = customHaskellPackages; };

in
  { backend-servant = hoogleAugmentedPackages.backend-servant;
  }