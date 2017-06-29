
# RRN: We must take ghc as an argument here:
{ pkgs ?
         import <nixpkgs> {},
         # import (sourcepkgs.fetchFromGitHub {
         #   owner = "NixOS";
         #   repo = "nixpkgs-channels";
         #   rev = "3ac20e92e8d187dea5315432f94e5109177386b2";
         #   sha256 = "11va3jhal5nk28kvqsqp2aarkzm71x687zxl03d0d0dvf7bw2pwi";
         # }) {},
  ghc ? pkgs.ghc
  }:

# https://github.com/NixOS/nixpkgs/archive/17.03.tar.gz
# ed031caf09356945884cb32459c6f32f1f627afd4e94eaa279b682106b3cf7b2
# let ghc = pkgs.ghc; in

pkgs.haskell.lib.buildStackProject {
  name = "ryan-test";
  inherit ghc;
  buildInputs = with pkgs; [ zlib ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
