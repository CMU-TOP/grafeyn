{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  mplSrc ?  pkgs.fetchgit {
          url = "https://github.com/MPLLang/mpl.git";
          rev = "5a983376cfdc2d2546c3f1a27783fd18333a018c";
          sha256 = "sha256-JTG0nUTakYrZNcon0j0HsVwwIFsAvRT8KIBH6TBjRsQ=";
	},
  rainey-pkgs ? pkgs.fetchgit {
          url = "https://github.com/mikerainey/nix-packages.git";
          rev = "61399409342c98a4b3ae9be8479b98aa79b4b7ff";
          sha256 = "sha256-cJUo5B23VXqo9qD1Ap3WL8aCyfivbvMKy4bL0O/Ojgc=";
	},
  mpl ? import "${rainey-pkgs}/pkgs/mpl/default.nix" { dfltSrc=mplSrc; },
  gmp ? pkgs.gmp
  }:

let
  customPython = pkgs.python310.buildEnv.override {
    extraLibs = [ pkgs.python310Packages.jsonschema
                  pkgs.python310Packages.simplejson
                  pkgs.python310Packages.psutil
                  pkgs.python310Packages.py-cpuinfo
                  pkgs.python310Packages.sqlalchemy
                  pkgs.python310Packages.python-sql
                  (with import <nixpkgs> {}; pkgs.python310Packages.callPackage ../../flexibench {})
                ];
  };
in

stdenv.mkDerivation rec {
  name = "feynsum";

  buildInputs = [ mpl gmp customPython pkgs.jq pkgs.dsq ];

  MPL_COMPILER="${mpl}/bin/mpl";

}
