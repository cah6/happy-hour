{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  withHoogle = true;
  overrides = self: super: {
    http-media = pkgs.haskell.lib.dontCheck super.http-media;
    servant = pkgs.haskell.lib.dontCheck super.servant;
    doctest = self.callPackage ./doctest.nix { };
    servant-reflex = self.callPackage ./servant-reflex.nix { };

    common-types = self.callCabal2nix "common-types" ./common-types { };
  };

  shellToolOverrides = ghc: super: {
    hlint = pkgs.haskellPackages.hlint;
    # ghc-exactprint = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.ghc-exactprint;
    # apply-refact = pkgs.haskellPackages.apply-refact;
    
    hie = (import (nixpkgs.fetchFromGitHub {
                   owner="domenkozar";
                   repo="hie-nix";
                   rev="96af698f0cfefdb4c3375fc199374856b88978dc";
                   sha256="1ar0h12ysh9wnkgnvhz891lvis6x9s8w3shaakfdkamxvji868qa";
                 }) {}).hie84;
  };
})
