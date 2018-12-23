open Ocamlbuild_plugin ;;

let _ = dispatch begin function
  | After_rules ->
      dep  ["compile"; "c"] ["config.h"];
      (* flag ["ocaml"; "compile"] & S[A"-ccopt"; A"-O9"]; *)
      flag ["compile"; "c"] & S[ A"-ccopt"; A"-D_GNU_SOURCE"; A"-ccopt"; A"-fPIC" ];

      flag ["link"; "library"; "ocaml"; "byte"; "use_libparmap"] &
        S[A"-dllib"; A"-lparmap_stubs"; ];
      flag ["link"; "library"; "ocaml"; "native"; "use_libparmap"] &
          S[A"-cclib"; A"-lparmap_stubs"; ];
      dep ["link"; "ocaml"; "use_libparmap"] ["libparmap_stubs.a"];
      flag ["link"; "ocaml"; "link_libparmap"] (A"libparmap_stubs.a");

  | _ -> ()
end
