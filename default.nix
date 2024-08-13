{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc965" }:
let
  myNixPkgs = import <nixpkgs> {
    overlays = [myNixPkgsOverlay];
  };

  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  myHaskellPkgsOverlay;
    });
  });

  myHaskellPkgsOverlay = (hSelf: hSuper: {
    canvas = hSelf.callCabal2nix "canvas" ./. {};
    
    canvas-haskell = hSelf.callCabal2nix "canvas-haskell" (myNixPkgs.lib.cleanSourceWith {
      src = builtins.fetchGit {
        url = "https://gitlab.com/lauraschauer/canvas-haskell-library.git";
        ref = "refs/heads/with-pagination-support";
      };
    }) {};
  });
in
myNixPkgs.myHaskellPackages.canvas

