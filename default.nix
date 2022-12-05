{ rev    ? "8bc2d76794ab08f6b997cb4b64ab9bf014b743f2" # release-22.11
, sha256 ? "sha256:1vh83d6knnzn5m817km2a2i6byfd0dbfvbkdpb1wkl5miwcqicr2"

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
