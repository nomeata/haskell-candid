{ rev    ? "e7603eba51f2c7820c0a182c6bbb351181caa8e7" # release-22.11
, sha256 ? "sha256:0mwck8jyr74wh1b7g6nac1mxy6a0rkppz8n12andsffybsipz5jw"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "haskell-candid requires at least nix 2.0"
    else import (builtins.fetchTarball {
       url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
       inherit sha256;
      }) {
         config.allowUnfree = true;
         config.allowBroken = false;
      }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

let
drv = pkgs.haskellPackages.developPackage {
  name = "candid";
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
    crc = markUnbroken (dontCheck super.crc);
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    passthru = {
      nixpkgs = pkgs;
    };

  });

  inherit returnShellEnv;
};

in drv
