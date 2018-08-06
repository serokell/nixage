{ pkgs, system, config, overrides, haskellOverrides }: proj:

let
  inherit (builtins) getAttr;
  inherit (pkgs.lib)
    any cleanSource composeExtensions foldr id mapAttrs mapAttrsToList warn;
  inherit (pkgs.haskell.lib) overrideCabal;

  inherit (import ./extraDeps { inherit pkgs; }) depNeedsPrefetch resolveExtraDep;
  inherit (import ./prefetch.nix { inherit pkgs; }) prefetchAllIncomplete;
  inherit (import ./stackage.nix) fixResolverName;
  inherit (import ./upstream.nix { inherit pkgs; }) callCabal2nix;

  stackageSrc =
    if proj ? nixpkgs-stackage
    then proj.nixpkgs-stackage
    else import ./nixpkgs-stackage.nix;

  nixpkgsSrc =
    if proj ? nixpkgs
    then proj.nixpkgs
    else import ./nixpkgs.nix;

  projPkgs = import (fetchTarball nixpkgsSrc) {
    inherit config system;
    overlays = [ overrides (import (fetchTarball stackageSrc)) ];
  };

  resolver = fixResolverName proj.resolver;

  stackagePackages = projPkgs.haskell.packages.stackage."${resolver}".override {
    overrides = foldr composeExtensions (_:_:{}) [
      speedupDeps
      extra-deps
      local-packages
      haskellOverrides
    ];
  };
  inherit (stackagePackages) callHackage;

  speedupDeps = self: super: {
    mkDerivation = drv: super.mkDerivation (drv // {
      doCheck = false;
      doHaddock = false;
    });
  };

  extra-deps = self: super:
    mapAttrs (resolveExtraDep self super) proj.extra-deps;

  projectSrc = cleanSource proj.root;
  mkLocalPackage = name: path:
    let
      drv = callCabal2nix stackagePackages name projectSrc {} ''--subpath="${path}"'';
      cabalOverrides = {
        doCheck = true;
        doHaddock = true;
        # HACK: make it easier to build packages without a license yet
        license = pkgs.lib.licenses.free;
      };
      derivationOverrides = {
        strictDeps = true;
      };
    in (overrideCabal drv (_: cabalOverrides)).overrideAttrs (_: derivationOverrides);
  local-packages = self: super:
    mapAttrs mkLocalPackage proj.packages;

in {
  inherit projPkgs;
  haskellPackages = stackagePackages;
  target =
    if any id (mapAttrsToList depNeedsPrefetch proj.extra-deps)
    then warn "Some dependencies are incomplete." (prefetchAllIncomplete proj)
    else mapAttrs (name: _: getAttr name stackagePackages) proj.packages;
}
