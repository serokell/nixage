{
  resolver = "lts-11.14";
  nixpkgs = {
    url = "https://github.com/nixos/nixpkgs/archive/d1ae60cbad7a49874310de91cd17708b042400c8.tar.gz";
    sha256 = "0a1w4702jlycg2ab87m7n8frjjngf0cis40lyxm3vdwn7p4fxikz";
  };
  packages = { nixage = "."; };
  extra-deps = {
    aeson-options = "0.0.0";
    cborg = "0.2.0.0";
    hashing = "0.1.0.1";
    hnix = "0.5.1";
    monadlist = "0.0.2";
    serialise = "0.2.0.0";
  };
  ghc-options = {
    "$locals" = "-Wall";
  };
}
