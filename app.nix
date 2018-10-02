{ prod ? false
, prodWsURL ? "ws://127.0.0.1:8000" }:

let
  syspkgs = import <nixpkgs> { };
  pkgs = import (syspkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "7c826371c49";
    sha256 = "09wxm698cgx6sxbvzgdg3kxzrj828s5q17wpszdd9w86sncm9kq4";
  }) { inherit overlays; };

  inherit (pkgs) runCommand closurecompiler;

  # !!! PROTOTYPE NIXPKGS !!! e4ca48c224a10b906cd9689e4531b39741f5b8fd

  # recent 7c826371c49

  misoRepo = pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "0q0m105c7jj913vh4mc9qsnc9ddvldff1np364z6acw6fyslnn5r";
    rev = "5c3d01780bce2f803bf10d531c876250be5eebc6";
  };

  spheresAndPointsRepo = pkgs.fetchFromGitHub {
    owner = "bmabsout";
    repo = "spheres-and-points";
    sha256 = "0ip871dmw4ppbv1hrwp4x86ss3rjgl81wv0v06yml41c0wsd0wd7";
    rev = "281288954742a0f0efdeb47a176dd554deafef7e";
  };

  jsaddleRepo = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "b423436565fce7f69a65d843c71fc52dc455bf54";
    sha256 = "09plndkh5wnbqi34x3jpaz0kjdjgyf074faf5xk97rsm81vhz8kk";
  };

  ghcjsSrc = pkgs.fetchFromGitHub {
    owner = "sorki";
    repo = "ghcjs";
    rev = "5acb3a2b83663da308196dee0bacb2577d86db32";
    sha256 = "1gqa1ilh92f2rd0zkc4lj4z3ys924x18k3mwfik3lq690mcz9vx3";
    fetchSubmodules = true;
  };

  overlays = [
    (self: super:
      {
        haskell = super.haskell // {
          compiler = super.haskell.compiler // {
            ghcjs84 = super.haskell.compiler.ghcjs84.override { inherit ghcjsSrc; };
          };
        };
      }
    )
  ];

  makeOverrides = function: names: haskellPackagesNew: haskellPackagesOld:
    let toPackage = name: {
          inherit name;
          value = function haskellPackagesOld.${name};
        };
    in builtins.listToAttrs (map toPackage names);

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

  dontChecks = [
    "directory-tree"
    "comonad"
    "Glob"
    "semigroupoids"
    "mockery"
    "servant"
    "unix-time"
    "word8"
    "lens"
    "wai-extra"
    "jsaddle"
    "wai-app-static"
    "SHA"
    "http-media"
  ];

  doJailbreak = [
  ];

  ghcCallPackage = pkgs.haskell.packages.ghc843.callPackage;

  ghcJSCallPackage = let
    overrides = composeExtensionsList [
      (makeOverrides pkgs.haskell.lib.dontCheck dontChecks)
      (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreak)
      (self: super: {

      doctest = self.callPackage "${spheresAndPointsRepo}/modifiedDeps/doctest/doctest.nix" {};
      jsaddle = self.callPackage "${jsaddleRepo}/jsaddle" {};

      #jsaddle = super.jsaddle.overrideAttrs (old: rec { libraryHaskellDepends = [ super.ghcjs-base ]; });
      #jsaddle = self.callPackage ../jsaddle/jsaddle {};

      })
      ];
   in (pkgs.haskell.packages.ghcjs84.override { inherit overrides; }).callPackage;


  # deps
  mqtt-hs = ghcCallPackage ./packages/mqtt-hs.nix { };

  cayene-lpp =  ghcCallPackage ./packages/cayene-lpp.nix { };
  cayene-lpp-js =  ghcJSCallPackage ./packages/cayene-lpp.nix { };

  ttn =  ghcCallPackage ./packages/data-ttn.nix { };
  ttn-js =  ghcJSCallPackage ./packages/data-ttn.nix { };

  ttn-client =  ghcCallPackage ./packages/ttn-client.nix {
    inherit ttn cayene-lpp mqtt-hs;
  };

  miso-ghc = ghcCallPackage "${misoRepo}/miso-ghc.nix" { };
  miso-ghcjs = ghcJSCallPackage "${misoRepo}/miso-ghcjs.nix" { };

  # toplevel
  server = ghcCallPackage ./server {
    inherit ttn ttn-client cayene-lpp;
    miso = miso-ghc;
  };

  client = ghcJSCallPackage ./client {
    ttn = ttn-js;
    cayene-lpp = cayene-lpp-js;
    miso = miso-ghcjs;
    patchWsURL = if prod then prodWsURL else null;
  };

  animate = pkgs.fetchurl {
    url = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css";
    sha256 = "13c9dc2s5gqh0p66xasnzyy8abh9kqgk6y94q7hbbbamj88zmqwg";
  };
  fontawesome = pkgs.fetchzip {
    url = "https://use.fontawesome.com/releases/v5.2.0/fontawesome-free-5.2.0-web.zip";
    sha256 = "1an7dda6pag9lfa66gm2y25zr1mlv3yw1qkdp007kqd2kxnylll5";
  };
  bootstrap = pkgs.fetchzip {
    url = "https://github.com/twbs/bootstrap/releases/download/v4.1.3/bootstrap-4.1.3-dist.zip";
    sha256 = "084wsbs8zhgi6cb4kvfbm1kahzvlmkdqk0sm5kn1clzyr90qzpvr";
    stripRoot=false;
  };

  static = runCommand "static-files" { } ''
    mkdir $out
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/all.js

    mkdir $out/fa
    cp -R ${fontawesome}/{css,webfonts} $out/fa/

    cp -R ${bootstrap}/css/bootstrap.min.css $out/bootstrap.css

    cp ${animate} $out/animate.css

    cp ${./site.css} $out/site.css
  '';

  combined = runCommand "dashboard-combined" { } ''
    mkdir -p $out/bin

    ln -s ${static}       $out/static
    ln -s ${server}/bin/* $out/bin
  '';

  runner = pkgs.writeScript "runner" ''
    #!${pkgs.stdenv.shell}
    OLDPWD="$( pwd )"
    TMPDIR="$( mktemp -d )"
    trap "{ rm -rf $TMPDIR ; cd $OLDPWD; exit 255; }" EXIT
    pushd $TMPDIR
    ln -s ${combined} result
    pushd result
    ./bin/server
  '';
in { inherit client server static combined runner ttn-client; }

