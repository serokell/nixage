{
  resolver = "lts-12.2";
  nixpkgs = {
    url = "https://github.com/kirelagin/nixpkgs/archive/14fe3f881faa373c228a092c222c1c7c3532c82c.tar.gz";
    sha256 = "0wx5srx0z3ixjfj7sg7xaw829mdmhk0hnr9q394fpmk1qh418av0";
  };
  packages = { nixage = "."; };
  extra-deps = {
    aeson-options = "0.0.0";
    cborg = "0.2.0.0";
    hashing = "0.1.0.1";
    hnix = "0.5.2";
    monadlist = "0.0.2";
    serialise = "0.2.0.0";
  };
  ghc-options = {
    "$locals" = "-Wall";
  };
}
