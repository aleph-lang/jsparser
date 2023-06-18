let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  # choose the ocaml version you want to use
  my-python = nixpkgs.python310;
  python-with-packages = my-python.withPackages ( p: with p; [
    aiohttp
  ]);
in
  nixpkgs.mkShell {
    # dependencies
    buildInputs = with nixpkgs; [ 
        #nixpkgs.latest.rustChannels.nightly.rust
	(nixpkgs.rustChannelOf { date = "2023-06-18"; channel = "nightly"; }).rust
        cargo
        rustc
        python-with-packages
        pkg-config
        openssl
    ];
  }
