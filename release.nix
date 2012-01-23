# Hydra build file for coccinelle

{ nixpkgs ? /etc/nixos/nixpkgs
, cocciSrc ? { outPath = ./.; revCount = 1234; gitTag = "abcdef"; }
, officialRelease ? false
}:

rec {
  tarball =
    let pkgs = import nixpkgs {};
        ml = pkgs.ocaml_3_12_1;
        mlPackages = pkgs.ocamlPackages_3_12_1;
    in with pkgs; releaseTools.sourceTarball {
      name = "coccinelle-tarball";
      src = cocciSrc;
      inherit officialRelease;
      version = builtins.readFile ./version;
      versionSuffix = if officialRelease then "" else "pre${toString cocciSrc.revCount}-${cocciSrc.gitTag}";
      
      buildInputs = [
        perl python texLiveFull
        ml mlPackages.findlib mlPackages.menhir
        mlPackages.ocaml_pcre mlPackages.ocaml_sexplib mlPackages.ocaml_extlib mlPackages.pycaml
      ];
      
      configurePhase = ''
        perl -w ./configure
        make depend
      '';
      
      preDist = ''
        local PREVHOME=$HOME
        export HOME=$TMPDIR
      '';
      
      postDist = ''
        export HOME=PREVHOME
      '';
    };

  build =
    { system ? builtins.currentSystem
    , ocamlVersion11 ? false
    }:
    
    let pkgs = import nixpkgs { inherit system; };
        ml = if ocamlVersion11 then pkgs.ocaml_3_11_1 else pkgs.ocaml_3_12_1;
        mlPackages = if ocamlVersion11 then pkgs.ocamlPackages_3_11_1 else pkgs.ocamlPackages_3_12_1;
    in with pkgs; releaseTools.nixBuild {
      name = "coccinelle";
      src = tarball;
    
      buildInputs = [
        perl python texLiveFull ncurses makeWrapper
        ml mlPackages.findlib mlPackages.menhir
        mlPackages.ocaml_pcre mlPackages.ocaml_sexplib mlPackages.ocaml_extlib mlPackages.pycaml
      ];

      preConfigure = ''
        sed -i "configure" -e's|/usr/bin/perl|${perl}/bin/perl|g'
        sed -i "globals/config.ml.in" \
            -e"s|/usr/local/share|$out/share|g"
      '';

      buildPhase = ''
        make depend
        make all
        make all.opt
      '';
      
      doCheck = false;
      postInstall = ''
        wrapProgram "$out/bin/spatch"                              \
         --prefix "LD_LIBRARY_PATH" ":" "$out/lib"                 \
         --prefix "PYTHONPATH" ":" "$out/share/coccinelle/python"

        yes | make test
      '';
    };
}
