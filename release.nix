# Hydra build file for coccinelle

{ nixpkgs ? /etc/nixos/nixpkgs
, cocciSrc ? { outPath = ./.; revCount = 1234; gitTag = "abcdef"; }
, testsSrc ? { outPath = ../big-tests; rev = 1234; }
, officialRelease ? false
, performRegress ? false
}:


let
  
  # version information
  version = builtins.readFile ./version;
  versionSuffix = if officialRelease then "" else "pre${toString cocciSrc.revCount}-${cocciSrc.gitTag}";


  #
  # Source release (tarball)
  #

  # The source tarball taken from the repository.
  # The tarball should actually be compilable using
  #   ./configure && make depend && make opt && make install
  # on systems other than nix.
  tarball =
    let pkgs = import nixpkgs { };
    in with pkgs; with ocamlPackages; releaseTools.sourceTarball {
      name = "coccinelle-tarball";
      src = cocciSrc;
      inherit officialRelease;
      inherit version;
      inherit versionSuffix;

      buildInputs = [
        ocaml findlib menhir python
        texLiveFull # for building the documentation
	pkgconfig  # for the autoconf macros
      ];

      preDist = ''
        local PREVHOME=$HOME
        export HOME=$TMPDIR    # the latex installation needs to write to the $HOME directory, so rename it here
      '';
      
      dontCopyDist = 1; # we'll copy the tarball to the tarballs folder ourselves (and rename it)
      postDist = ''
        export HOME=$PREVHOME  # restore the home directory

        ensureDir "$out/tarballs"

        # rename the tarball to give it a version-specific name
        cp coccinelle-*.tar.gz "$out/tarballs/coccinelle-${version}${versionSuffix}.tar.gz"
      '';
    };


  #
  # Builds for specific configurations
  #

  # builds coccinelle, parameterized over the ocaml and python packages, and the configure flags.
  # the result should be a usable nix-expression

  # mkConfiguration is a function that takes the nix package collection of the build
  # (called 'pkgs') and results in a record containing:
  #  name of the configuration, python packages, ocaml packages selection function
  #  (which takes the original 'pkgs' as parameter), and ocaml packages. The selection
  #  function is used by 'mkConfiguration' to determine the appropriate ocamlPackages
  #  field in 'pkgs'.
  mkBuild = mkConfiguration: { system ? builtins.currentSystem }:
    let pkgs = import nixpkgs {
          inherit system;
          config.packageOverrides = orig : {
            ocamlPackages = cfg.selOcaml orig;
          };
        };
        cfg = mkConfiguration pkgs;
    in with pkgs; releaseTools.nixBuild {
      name = "cocci-build-${cfg.name}";
      src = tarball;
      buildInputs = [ pkgconfig ncurses ocaml ] ++ cfg.ocamls ++ cfg.pythons;
      configureFlagsArray = cfg.flags;
    };

  build = mkBuild defaultCfg;
  defaultCfg = pkgs: with pkgs; {
    name = "default";
    pythons = [ python3 ];
    ocamls = with ocamlPackages; [
      findlib # menhir ocaml_typeconv ocaml_sexplib ocaml_extlib ocaml_pcre pycaml
    ];
    flags = [];
    selOcaml = orig: orig.ocamlPackages;
  };


  /*
  # selects which version of ocaml and ocamlPackages to use in nixpkgs.
  selOcaml312 = pkgs:
    { ocaml = pkgs.ocaml_3_12_1;
      ocamlPackages = pkgs.ocamlPackages_3_12_1;
    };
  selOcaml310 = pkgs:
    { ocaml = pkgs.ocaml_3_10_0;
      ocamlPackages = pkgs.ocamlPackages_3_10_0;
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

  # different configurations of coccinelle builds based on different ocamls/available libraries
  build = mkBuild { name = "coccinelle"; ocamlVer = selOcaml312; mkEnv = libs_full; inclPython = true; };
  build_rse = mkBuild { name = "coccinelle_config1"; ocamlVer = selOcaml312; mkEnv = libs_rse; inclPython = true; };
  build_se = mkBuild { name = "coccinelle_config2"; ocamlVer = selOcaml312; mkEnv = libs_se; inclPython = true; };
  build_null_12 = mkBuild { name = "coccinelle_config3"; ocamlVer = selOcaml312; mkEnv = libs_null; inclPython = true; };
  # build_null_10 = mkBuild { name = "coccinelle_config4"; ocamlVer = selOcaml310; mkEnv = libs_null; inclPython = true; };
  build_null_12_np = mkBuild { name = "coccinelle_config5"; ocamlVer = selOcaml312; mkEnv = libs_null; inclPython = false; };
  # build_null_10_np = mkBuild { name = "coccinelle_config6"; ocamlVer = selOcaml310; mkEnv = libs_null; inclPython = false; };
  build_rse_np = mkBuild { name = "coccinelle_config7"; ocamlVer = selOcaml312; mkEnv = libs_rse; inclPython = false; };
  */


  #
  # Package builders
  #

  # package builder for Debian-based OS'ses
  makeDeb =
    system: diskImageFun:
    
    with import nixpkgs { inherit system; };
    releaseTools.debBuild {
      name = "coccinelle-deb";
      src = tarball;
      diskImage = diskImageFun vmTools.diskImageFuns {
        extraPackages = [ "python" "python-support" "ocaml-nox" "ocaml-findlib" ];
      };
      debRequires = [ "python" "python-support" "ocaml-nox" "ocaml-findlib" ];
      doCheck = false;

      buildPhase = ''
        make depend
        make all
        make all.opt
      '';
    };

  makeDeb_i686 = makeDeb "i686-linux";
  makeDeb_x86_64 = makeDeb "x86_64-linux";

  # different debian builds
  # deb_ubuntu1010_i386 = makeDeb_i686 (disk: disk.ubuntu1010i386);
  # deb_ubuntu1010_x86_64 = makeDeb_x86_64 (disk: disk.ubuntu1010x86_64);


  #
  # Testing tasks
  #

  mkTask =
    argsfun: { system ? builtins.currentSystem }:
    let pkgs = import nixpkgs { inherit system; };
        args = argsfun pkgs system;
        name = "${args.name}-${version}${versionSuffix}";
    in pkgs.stdenv.mkDerivation ({
      phases = [ "runPhase" ];

      runPhase = ''
        ensureDir "$out"
        ensureDir "$out/nix-support"
        touch "$TMPDIR/result.log"
        exec > >(tee -a "$TMPDIR/result.log") 2> >(tee -a "$TMPDIR/result.log" >&2)
        runHook execPhase
        cp "$TMPDIR/result.log" "$out/"
        echo "report log $out/result.log" >> "$out/nix-support/hydra-build-products"
        echo "$name" > "$out/nix-support/hydra-release-name"
      '';

      meta = {
        description = "Coccinelle post-build task";
        schedulingPriority = 8;
      };
    } // args // { inherit name; });

  mkReport = inputs: mkTask (pkgs: _: with pkgs; {
    name = "report";
    builds = map (i: i {}) inputs;

    execPhase = ''
      echo "collecting logs"
      for build in $builds; do
        echo "$build/nix-support/make.log"
        cat "$build/nix-support/make.log"
      done

      echo "grepping OCaml warnings"
      if grep -2 "Warning " "$TMPDIR/result.log"
      then
        echo "found warnings!"
        false
      else
        echo "there are apparently no significant warnings"
      fi
    '';

    meta = {
      description = "Analysis of the coccinelle build reports";
      schedulingPriority = 5;
    };
  });

  report = mkReport [ build ];
  # build_rse build_se build_null_12 build_null_12_np build_rse_np


  #
  # Regression tests
  #

  # Produces regression test results, which can be positive or
  # negative. The build should succeed regardless of the outcome
  # of individual tests unless coccinelle is horribly broken.
  # The resulting files are stored in a tarball so that it allows
  # manual inspection.
  mkRegress = cocciSelect: mkTask (pkgs: system: with pkgs;
    let coccinelle = cocciSelect { inherit system; };
    in {
      name = "regression-${toString testsSrc.rev}";
      buildInputs = [ coccinelle ];

      execPhase = ''
        # prepare a writeable tests directory
        # as this directory contains large
        # files, we'll create links to the
        # individual files.
        ensureDir "$TMPDIR/tests"
        cp -rs ${testsSrc}/* "$TMPDIR/tests/"
	chmod -R u+w "$TMPDIR/tests/"
        cd "$TMPDIR/tests"

	# initialize essential environment variables
        # for the makefile
        export COCCIDIR=$TMPDIR
        export SPATCH=${coccinelle}/bin/spatch.opt
        export ISO=${coccinelle}/share/coccinelle/standard.iso
        export DEFS=${coccinelle}/share/coccinelle/standard.h

	# generate the test outcomes
        make -e all

        # collect the results
	# note: the tarball is likely to contain useless
        # symbolic links to files in the nix store. So be it.
        cd "$TMPDIR"
        tar -czf "$out/results.tar.gz" ./tests
	echo "file binary-dist $out/results.tar.gz" >> "$out/nix-support/hydra-build-products"
      '';

      meta = {
        description = "Regression test of Coccinelle";
        schedulingPriority = 8;
      };
    });

  # Checks whether the regression tests meet our expectations.
  # If the set of failed tests is different than specified in
  # the tests repository, this check fails.
  checkRegress = regressSelect: mkTask (pkgs: system: with pkgs;
    let regress = regressSelect { inherit system; };
    in {
      name = "test-${toString testsSrc.rev}";

      execPhase = ''
        # prepare a writeable tests directory
        # as this directory contains large
        # files, we'll create links to the
        # individual files.
        ensureDir "$TMPDIR/tests"
        cp -rs ${testsSrc}/* "$TMPDIR/tests/"
	chmod -R u+w "$TMPDIR/tests/"

        # extract the outcome of the regression test over it
	echo "reconstructing regression directory"
        cd "$TMPDIR"
        tar xfz "${regress}/results.tar.gz"
        cd "$TMPDIR/tests"

	echo "analyzing results"
	make failedlog

	echo "verifying the outcome"
	make check
      '';

      meta = {
        description = "Regression test of Coccinelle";
        schedulingPriority = 8;
      };
    });

  regress = assert performRegress; mkRegress build;
  test = checkRegress regress;

  
  #
  # collections of build tasks
  #

  basicAttrs = {
    inherit tarball;
    inherit build;
# build_rse build_se build_null_12 build_null_12_np build_rse_np;
    inherit report;
  };

  testAttrs = {
    inherit regress;
    inherit test;
  };

in basicAttrs // (if performRegress then testAttrs else {})
