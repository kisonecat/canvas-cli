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
  
  myDevTools = with myNixPkgs; [
    cabal-install 
    haskellPackages.ghcid
    haskell-language-server
    #haskellPackages.haskell-language-server
    hlint
    haskellPackages.apply-refact
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.canvas.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
