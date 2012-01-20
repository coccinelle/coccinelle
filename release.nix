# Hydra configuration file for coccinelle

{ nixpkgs ? /etc/nixos/nixpkgs
, cocciSrc ? { outPath = ./.; revCount = 1234; gitTag = "abcdef"; }
, officialRelease ? false
, ocamlVersion11 ? false
}:

let pkgs = import nixpkgs { };
    ml = if ocamlVersion11 then pkgs.ocaml_3_11_1 else pkgs.ocaml_3_12_1;
    mlPackages = if ocamlVersion11 then pkgs.ocamlPackages_3_11_1 else pkgs.ocamlPackages_3_12_1;

in rec {
  tarball =
    with pkgs;
    releaseTools.sourceTarball {
      name = "coccinelle-tarball";
      src = cocciSrc;
      inherit officialRelease;
      version = builtins.readFile ./version;
      versionSuffix = if officialRelease then "" else "pre${toString cocciSrc.revCount}-${cocciSrc.gitTag}";
      
      buildInputs =
        [ perl python ml mlPackages.findlib mlPackages.menhir texLiveFull ];
      
      configurePhase = ''
        perl -w ./configure
      '';
      
      preDist = ''
        local PREVHOME=$HOME
        export HOME=$TMPDIR
      '';
      
      postDist = ''
        export HOME=PREVHOME
      '';
    };
}
