# Hydra build file for coccinelle

{ nixpkgs ? /etc/nixos/nixpkgs
, cocciSrc ? { outPath = ./.; revCount = 1234; gitTag = "abcdef"; }
, officialRelease ? false
}:

let
  
  # The source tarball taken from the repository.
  # The tarball should actually be compilable using
  #   ./configure && make depend && make opt && make install
  # on systems other than nix.
  tarball =
    let
      pkgs = import nixpkgs {
        # use ocaml 3.12
        config.packageOverrides =
          pkgs:
          { ocaml = pkgs.ocaml_3_12_1;
            ocamlPackages = pkgs.ocamlPackages_3_12_1;
          };
      };
      version = builtins.readFile ./version;
      versionSuffix = if officialRelease then "" else "pre${toString cocciSrc.revCount}-${cocciSrc.gitTag}";
    in with pkgs; with ocamlPackages; releaseTools.sourceTarball {
      name = "coccinelle-tarball";
      src = cocciSrc;
      inherit officialRelease;
      inherit version;
      inherit versionSuffix;

      buildInputs = [
        perl python texLiveFull
        ocaml findlib menhir
        ocaml_pcre ocaml_sexplib
        ocaml_extlib pycaml
      ];
      
      configurePhase = ''
        # explicitly run perl because the configure script references a perl outside the nix store
        # substituting the path to perl is not a good idea as it would invalidate the tarball on
        # non-nix machines.
        perl -w ./configure
        
        make depend
      '';

      preDist = ''
        local PREVHOME=$HOME
        export HOME=$TMPDIR    # the latex installation needs to write to the $HOME directory, so rename it here
      '';
      
      postDist = ''
        export HOME=$PREVHOME  # restore the home directory

        # rename the tarball to give it a version-specific name
        mv coccinelle-*.tar.gz "coccinelle-${version}${versionSuffix}.tar.gz"
      '';
    };


  # builds coccinelle, given a ocaml selector function and an ocaml environment builder.
  # the build procedure itself is largely the same as the coccinelle expression in nixpkgs.
  # the result should be a usable nix-expression
  mkBuild = { name, ocamlVer, mkEnv }: { system ? builtins.currentSystem }:
    let pkgs = import nixpkgs {
          inherit system;
          config.packageOverrides = ocamlVer;
        };

        ocamlEnv = mkEnv pkgs;
    in with pkgs; releaseTools.nixBuild {
      inherit name;
      src = tarball;

      # ocamlEnv contains the ocaml libraries in scope.
      buildInputs = [ perl python texLiveFull ncurses makeWrapper ocamlEnv ];

      # patch the files for use with nix
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
      
      # run checking after installation.
      # also, the test phase may require a yes/no input.
      doCheck = false;
      postInstall = ''
        wrapProgram "$out/bin/spatch"                              \
         --prefix "LD_LIBRARY_PATH" ":" "$out/lib"                 \
         --prefix "PYTHONPATH" ":" "$out/share/coccinelle/python"

        yes | make test
      '';
    };


  # selects which version of ocaml and ocamlPackages to use in nixpkgs.
  selOcaml312 = pkgs:
    { ocaml = pkgs.ocaml_3_12_1;
      ocamlPackages = pkgs.ocamlPackages_3_12_1;
    };
  selOcaml311 = pkgs:
    { ocaml = pkgs.ocaml_3_11_1;
      ocamlPackages = pkgs.ocamlPackages_3_11_1;
    };


  # builds an environment with the ocaml packages needed to build coccinelle
  # the mkList function selects which additional packages to include
  mkOcamlEnv = mkList: pkgs:
    pkgs.buildEnv {
      name = "cocci-ocamlenv";
      paths = with pkgs.ocamlPackages; [ pkgs.ocaml findlib menhir ] ++ mkList pkgs.ocamlPackages;
    };

  # selections of ocaml libraries
  libs_full = mkOcamlEnv (libs: with libs; [ ocaml_pcre ocaml_sexplib ocaml_extlib pycaml ]);
  libs_rse  = mkOcamlEnv (libs: with libs; [ ocaml_pcre ocaml_sexplib ocaml_extlib ]);
  libs_se   = mkOcamlEnv (libs: with libs; [ ocaml_sexplib ocaml_extlib ]);
  libs_null = mkOcamlEnv (libs: []);

in # list of jobs
{ inherit tarball;

  # different configurations of coccinelle builds based on different ocamls/available libraries
  build = mkBuild { name = "coccinelle"; ocamlVer = selOcaml312; mkEnv = libs_full; };
  build_rse = mkBuild { name = "coccinelle_config1"; ocamlVer = selOcaml312; mkEnv = libs_rse; };
  build_se = mkBuild { name = "coccinelle_config2"; ocamlVer = selOcaml312; mkEnv = libs_se; };
  build_null_12 = mkBuild { name = "coccinelle_config3"; ocamlVer = selOcaml312; mkEnv = libs_null; };
  build_null_11 = mkBuild { name = "coccinelle_config4"; ocamlVer = selOcaml311; mkEnv = libs_null; };
}
