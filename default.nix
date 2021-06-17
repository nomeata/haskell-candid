{ rev    ? "0d40179fd4cd7252bd8f8763c02cc8991b891219" # release-21.05
, sha256 ? "0n2wm7n0aar2j2cjm22swm09dsmzgji34mb11nmr1ffs3vzhgr07"

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
