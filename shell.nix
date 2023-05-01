let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
in
  nixpkgs.mkShell {
    # dependencies
    buildInputs = with nixpkgs; [ 
	(nixpkgs.rustChannelOf { date = "2023-04-26"; channel = "nightly"; }).rust
        cargo
        rustc
    ];
  }
