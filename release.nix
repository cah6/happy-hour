{ withHoogle ? false
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };

  pinnedHaskellPkgs = pinnedPkgs.haskellPackages;

  customHaskellPackages = pinnedHaskellPkgs.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      happy-hour-backend = self.callCabal2nix "happy-hour-backend" ./. { };
    });
  });

  hoogleAugmentedPackages = import ./toggle-hoogle.nix { withHoogle = withHoogle; input = customHaskellPackages; };

in
  { project1 = hoogleAugmentedPackages.happy-hour-backend;
  }