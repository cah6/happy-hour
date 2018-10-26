{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super: {
    http-media = pkgs.haskell.lib.dontCheck super.http-media;
    servant = pkgs.haskell.lib.dontCheck super.servant;
    doctest = self.callPackage ./doctest.nix { };
    servant-reflex = self.callPackage ./servant-reflex.nix { };

    common-types = self.callCabal2nix "common-types" ./common-types { };
  };
})
