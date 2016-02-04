# Hydra build file for coccinelle

{ nixpkgs ? "/etc/nixos/nixpkgs"
, cocciSrc ? { outPath = ./.; revCount = 1234; gitTag = "abcdef"; }
, testsSrc ? { outPath = ../big-tests; rev = 1234; }
, officialRelease ? false
, performRegress ? true
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

        mkdir -p "$out/tarballs"

        # rename the tarball to give it a version-specific name
        cp coccinelle-*.tar.gz "$out/tarballs/coccinelle-${version}${versionSuffix}.tar.gz"
      '';
    };


  #
  # Helper functions for building configurations
  #

  selOcamlDefault = orig: orig.ocamlPackages;
  selOcaml400 = orig: orig.ocamlPackages_4_00_0;
  selOcaml312 = orig: orig.ocamlPackages_3_12_1;
  selOcaml311 = orig: orig.ocamlPackages_3_11_2;
  selOcaml310 = orig: orig.ocamlPackages_3_10_0;

  selCommonOcamlPkgs = ocamlPackages: with ocamlPackages; [
    findlib menhir ocaml_sexplib
  ];

  selMinimalOcamlPkgs = ocamlPackages: with ocamlPackages; [
    findlib menhir
  ];

  selAllOcamlPkgs = ocamlPackages: with ocamlPackages; [
    findlib menhir ocaml_sexplib ocaml_pcre pycaml
  ];

  selCommonInputs = pkgs: [ pkgs.pkgconfig pkgs.pcre ];

  selDefaultShell = pkgs: pkgs.stdenv.shell;

  selPythonNone = pkgs: [];
  selPythonDefault = pkgs: [ pkgs.python ];
  selPython2 = pkgs: [ pkgs.python27 ];
  selPython3 = pkgs: [ pkgs.python3 ];

  # creates a configuration for a given python version
  mkCfgPython = f: pkgs: with (f pkgs); {
    inherit name pythons flags;

    ocamls = selCommonOcamlPkgs pkgs.ocamlPackages ++ [ pkgs.ocamlPackages.pycaml ];
    selOcaml = selOcamlDefault;
    extras = selCommonInputs pkgs;
    shell = selDefaultShell pkgs;
    extraAttrs = { };
  };

  # creates a configuration for a given ocaml version
  mkCfgOcaml = { name, selOcaml, flags }: pkgs: {
      inherit flags selOcaml;

    name = "ocaml-${name}";
    pythons = selPythonDefault pkgs;
    ocamls = selMinimalOcamlPkgs pkgs.ocamlPackages;
    extras = selCommonInputs pkgs;
    shell = selDefaultShell pkgs;
    extraAttrs = { };
  };

  # creates a default configuration with additional flags
  mkCfgDefault = { name, flags, extra ? {} }: pkgs: {
    inherit name flags;
    pythons = selPythonDefault pkgs;
    ocamls = selAllOcamlPkgs pkgs.ocamlPackages;
    selOcaml = selOcamlDefault;
    extras = selCommonInputs pkgs;
    shell = selDefaultShell pkgs;
    extraAttrs = extra;
  };

  # creates a minimal configuration with additional flags
  mkCfgMinimal = { name, flags }: pkgs: {
    inherit name flags;
    pythons = [];
    ocamls = [];
    selOcaml = selOcamlDefault;
    extras = [];
    shell = selDefaultShell pkgs;
    extraAttrs = { };
  };

  # creates a configuration for the given ocaml packages
  mkCfgPackage = { name, ocamls, flags }: pkgs: {
    inherit name flags;
    pythons = selPythonDefault pkgs;
    ocamls = selMinimalOcamlPkgs pkgs.ocamlPackages ++ ocamls pkgs.ocamlPackages;
    selOcaml = selOcamlDefault;
    extras = selCommonInputs pkgs;
    shell = selDefaultShell pkgs;
    extraAttrs = { };
  };

  # build the project using the given shell
  # it takes a minimal configuration, but then with all the
  # libraries that trigger features of coccinelle to be enabled.
  mkCfgShell = { name, selShell }: pkgs: {
    inherit name;
    pythons = selPythonDefault pkgs;
    ocamls = selMinimalOcamlPkgs pkgs.ocamlPackages;
    selOcaml = selOcamlDefault;
    flags = [];
    extras = [ pkgs.pcre ];
    shell = selShell pkgs;
    extraAttrs = { };
  };

  # creates a configuration with multiple ocaml versions: this gives
  # conflicts. This is just a test to see whether our build system is
  # not too much confused in this case. It seems at least that ocamlfind
  # cannot be used in this setting.
  mkCfgManyOcaml =
    let
      selOcaml = pkgs: ocamlPkgSel: with (ocamlPkgSel pkgs); ocaml;
      selPkgs = pkgs: ocamlPkgSel: with (ocamlPkgSel pkgs); [ menhir ];
    in sels: pkgs: {
      name = "many-ocaml";
      pythons = [];
      ocamls = pkgs.lib.concatMap (selPkgs pkgs) sels;
      selOcaml = selOcamlDefault;
      flags = [];
      extras = selCommonInputs pkgs ++ map (selOcaml pkgs) sels;
      shell = selDefaultShell pkgs;
      extraAttrs = { };
    };


  #
  # Configurations
  #

  defaultCfg = mkCfgDefault { name = "default"; flags = []; };
  debugCfg = mkCfgDefault { name = "debug"; flags = [ "--enable-release=no" ]; };
  wrappersCfg = mkCfgDefault { name = "wrappers"; flags = [ "--enable-python" "--enable-ocaml" "--without-pkg-config" "--without-ocamlfind" ]; };
  manyOcamlCfg = mkCfgManyOcaml [ selOcaml400 selOcaml312 selOcaml311 selOcaml310 ];

  minimalCfgs = map mkCfgMinimal [
    { name = "minimal"; flags = []; }
    { name = "noocamlscripting"; flags = [ "--disable-ocaml" ]; }
  ];

  # Several configurations testing different python versions.
  # We exlicitly pass the "--enable-python" flag so that the
  # build should fail if no suitable python can be detected.
  pythonCfgs = 
    map mkCfgPython [
      ( _ : { name = "no-python"; pythons = []; flags = []; })

      (pkgs: {
        name = "python2-local";
        pythons = selPython2 pkgs;
        flags = [ "--enable-python" "--disable-pycaml" ];
      })

      (pkgs: {
        name = "python3-local";
        pythons = selPython3 pkgs;
        flags = [ "--enable-python" "--disable-pycaml" ];
      })

      (pkgs: {
        name = "python3-global";
        pythons = selPython3 pkgs;
        flags = [ "--enable-python" ];
      })

      (pkgs: {
        name = "python-nopkgconfig";
        pythons = selPython2 pkgs;
        flags = [ "--enable-python" "--without-pkg-config" ];
      })

#  disabled because this combination does not work in NixOS
#      (pkgs: {
#        name = "many-pythons";
#        pythons = selPython3 pkgs ++ selPython2 pkgs;
#        flags = [ "--with-python=python3" ];
#      })
    ];

  # Several configurations testing different OCaml versions.
  # These versions ship with minimal global packages in order
  # to thest the bundled packages with these ocaml versions.
  ocamlCfgs = map mkCfgOcaml [
    { name = "400nat"; selOcaml = selOcaml400; flags = [ "--enable-release=yes" ]; }
    { name = "400byt"; selOcaml = selOcaml400; flags = []; }
    { name = "312"; selOcaml = selOcaml312; flags = []; }
    { name = "311"; selOcaml = selOcaml311; flags = [ "--enable-release=yes" ]; }
    { name = "310"; selOcaml = selOcaml310; flags = []; }
  ];

  # Several configurations testing different available
  # ocaml packages.
  pkgCfgs = map mkCfgPackage [
    { name = "pcre"; ocamls = ps: [ ps.ocaml_pcre ]; flags = [ "--enable-pcre-syntax" ]; }
    { name = "sexplib"; ocamls = ps: [ ps.ocaml_sexplib ]; flags = [ "--enable-sexplib" ]; }
    { name = "pycaml"; ocamls = ps: [ ps.pycaml ]; flags = [ "--enable-pycaml" ]; }
  ];

  # Tests using several different types of shells.
  shellCfgs = map mkCfgShell [
    { name = "bash"; selShell = pkgs: "${pkgs.bash}/bin/bash"; }
    { name = "dash"; selShell = pkgs: "${pkgs.dash}/bin/dash"; }
    { name = "zsh"; selShell = pkgs: "${pkgs.zsh}/bin/zsh"; }

    # the configure script is not compatible with tcsh
    # { name = "tcsh"; selShell = pkgs: "${pkgs.tcsh}/bin/tcsh"; }
  ];

  #
  # Configurations for the compilation of coccinelle using ocamlbuild.
  #

  ocamlbuildZeroCfg = mkCfgMinimal {
    name  = "ocamlbuild-zero";
    flags = [ "--enable-ocamlbuild" "--enable-release" ];
  };

  ocamlbuildFullCfg = mkCfgDefault {
    name  = "ocamlbuild-full";
    flags = [ "--enable-ocamlbuild" "--enable-release" ];
  };

  ocamlbuildCfgs = map mkCfgOcaml [
    { name = "ocamlbuild-400nat"; selOcaml = selOcaml400;
      flags = [ "--enable-ocamlbuild" "--enable-release=yes" ]; }
    { name = "ocamlbuild-400byte"; selOcaml = selOcaml400;
      flags = [ "--enable-ocamlbuild" ]; }
    { name = "ocamlbuild-312"; selOcaml = selOcaml312;
      flags = [ "--enable-ocamlbuild" "--enable-release" ]; }
    { name = "ocamlbuild-311"; selOcaml = selOcaml311;
      flags = [ "--enable-ocamlbuild" ]; }
    { name = "ocamlbuild-310"; selOcaml = selOcaml310;
      flags = [ "--enable-ocamlbuild" "--enable-release" ]; }
  ] ++ [ ocamlbuildZeroCfg ocamlbuildFullCfg ];

  altCfgs =
    [ debugCfg manyOcamlCfg ]
    ++ minimalCfgs
    ++ ocamlCfgs ++ pythonCfgs
    ++ pkgCfgs ++ shellCfgs
    ++ ocamlbuildCfgs;


  #
  # Builds for specific configurations
  #

  # builds coccinelle, parameterized over the ocaml and python packages, and the configure flags.
  # the result should be a usable nix-expression

  # mkConfiguration is a function that takes the nix package collection of the build
  # (called 'pkgs') and results in a record containing:
  # name of the configuration, python packages, ocaml packages selection function
  # (which takes the original 'pkgs' as parameter), and ocaml packages. The selection
  # function is used by 'mkConfiguration' to determine the appropriate ocamlPackages
  # field in 'pkgs'.
  mkBuild = mkConfiguration: { system ? builtins.currentSystem }:
    let pkgs = import nixpkgs {
          inherit system;
          config.packageOverrides = orig : {
            ocamlPackages = cfg.selOcaml orig;
          };
        };
        cfg = mkConfiguration pkgs;
        flags = [ "--enable-release=world" ] ++ cfg.flags;
    in with pkgs; releaseTools.nixBuild ({
      inherit (cfg) shell;
      name = "cocci-build-${cfg.name}";
      src = tarball;
      enableParallelBuilding = true;
      buildInputs = cfg.extras ++ [ ncurses ocamlPackages.ocaml ] ++ cfg.ocamls ++ cfg.pythons;
      configureFlags = pkgs.lib.concatStringsSep " " flags; # hmm, flags are now not allowed to contain spaces
      doCheck = true;
      
      buildPhase = ''
        mkdir -p "$out/nix-support/"
        touch "$out/nix-support/make.log"
        echo "report log $out/nix-support/result.log" >> "$out/nix-support/hydra-build-products"

        make all 2> >(tee -a "$out/nix-support/make.log" >&2)
      '';

      # changes the shell in some of the scripts to the configured one
      prePatch = ''
        echo "patching the shell in scripts to: ${cfg.shell}"
        for script in configure scripts/spatch.sh.in scripts/genversion.sh \
          setup/fake-subst.sh setup/fake-menhir.sh setup/fake-pdflatex.sh; do
          substituteInPlace $script --replace '#! /bin/sh' '#! ${cfg.shell}'
        done
      '';
    } // cfg.extraAttrs);

  build = mkBuild defaultCfg;
  altBuilds = map mkBuild altCfgs;
  allBuilds = [ build ] ++ altBuilds;

  # compile with ocaml profiling turned on and then running the
  # test suite to collect results.
  profileCfg = mkCfgDefault {
    name = "profiling";
    flags = [ "--enable-release=profile" ];
    extra = {
      installPhase = ''
        mkdir -p "$out/nix-support"
        cp ocamlprof.dump "$out/ocamlprof.dump"
        echo "file binary $out/ocamlprof.dump" >> "$out/nix-support/hydra-build-products"
      '';
    };
  };
  profile = mkBuild profileCfg {};


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
        mkdir -p "$out"
        mkdir -p "$out/nix-support"
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

  mkReport = inputs: mkTask (pkgs: _: with pkgs;
    let builds = map (i: i { inherit (pkgs.stdenv) system; }) inputs; in {
      name = "report";

      execPhase = ''
        echo "collecting logs"
        for build in ${lib.concatStringsSep " " builds}; do
          echo "log: $build/nix-support/make.log"
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

  report = mkReport allBuilds;


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
        mkdir -p "$TMPDIR/tests"
        cp -rs ${testsSrc}/* "$TMPDIR/tests/"
	chmod -R u+w "$TMPDIR/tests/"
        cd "$TMPDIR/tests"

	# initialize essential environment variables
        # for the makefile
	export COCCINELLE_HOME=${coccinelle}/lib/coccinelle
        export COCCIDIR=$TMPDIR
        export SPATCH=${coccinelle}/bin/spatch.opt
        export ISO=${coccinelle}/lib/coccinelle/standard.iso
        export DEFS=${coccinelle}/lib/coccinelle/standard.h

	# generate the test outcomes using a parallel build
        make -e all -j$NIX_BUILD_CORES -l$NIX_BUILD_CORES

        # collect the results
	# note: the tarball is likely to contain useless
        # symbolic links to files in the nix store. We therefore
        # delete these symlinks. As a result, you should be able
        # to unpack the tarball in the tests directory.
        find "$TMPDIR/tests" -depth -type l -delete
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
        mkdir -p "$TMPDIR/tests"
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

  regress = mkRegress build;
  test = checkRegress regress;


  #
  # Performing release actions
  #

  dist =
    let pkgs = import nixpkgs { };
        name = "release-${version}${versionSuffix}";
    in with pkgs; releaseTools.nixBuild {
      inherit name;
      src = cocciSrc;
      buildInputs = with ocamlPackages; [
        pkgconfig ncurses texLiveFull
        ocaml findlib menhir
        python pcre patchelf
      ];
      configureFlags = "--enable-release";

      buildPhase = ''
        export TARGETDIR="$TMPDIR/dists"
        mkdir -p $TARGETDIR
        export HOME=$TMPDIR
	make prerelease GIT=echo TMP=$TARGETDIR
	make release GIT=echo TMP=$TARGETDIR
	make package TMP=$TARGETDIR
      '';

      installPhase = ''
        mkdir -p "$out/nix-support/"
	echo "cocci-dist-${version}" > "$out/nix-support/hydra-release-name"
	cp $TMPDIR/dists/*.tgz "$out/"
	for file in $out/*.tgz; do
          echo "file binary-dist $file" >> $out/nix-support/hydra-build-products
	done
      '';

      dontInstall = false;
      doCheck = false;
    };
  
  #
  # collections of build tasks
  #

  basicAttrs = {
    inherit tarball;
    inherit build;
    inherit report;
    inherit dist;
    inherit profile;
  };

  # artificial dependency on report to ensure that we are not going through
  # an expensive regression test when there is already something wrong with
  # the build process.
  reportFirst = x : if report == null then x else x;
  testAttrs = reportFirst {
    inherit regress;
    inherit test;
  };

in basicAttrs // (if performRegress then testAttrs else {})
