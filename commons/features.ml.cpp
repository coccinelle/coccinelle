(* yes sometimes cpp is useful *)

(* old:
note: in addition to Makefile.config, globals/config.ml is also modified
by configure
features.ml: features.ml.cpp Makefile.config
	cpp -DFEATURE_GUI=$(FEATURE_GUI) \
            -DFEATURE_MPI=$(FEATURE_MPI) \
	    -DFEATURE_PCRE=$(FEATURE_PCRE) \
	   features.ml.cpp > features.ml

clean::
	rm -f features.ml

beforedepend:: features.ml

*)

#if FEATURE_MPI==1

module Distribution = struct
  let under_mpirun () = 
    Distribution.under_mpirun()
      
  let mpi_main2 ?debug_mpi map_ex reduce_ex fxs = 
    Distribution.mpi_main2 debug_mpi map_ex reduce_ex fxs

  let mpi_adjust_argv argv = 
    Distribution.mpi_adjust_argv argv
end

#else 

module Distribution = struct
  let under_mpirun () = 
    false
      
  let mpi_main2 ?debug_mpi map_ex reduce_ex fxs = 
    let res = List.map map_ex (fxs()) in
    reduce_ex res

  let mpi_adjust_argv argv = 
    argv
end

#endif


#if FEATURE_REGEXP_PCRE==1
#else
#endif

#if FEATURE_BACKTRACE==1
module Backtrace = struct
 let print () = 
   Backtrace.print ()
end
#else

module Backtrace = struct
 let print () = 
   print_string "no backtrace support, use configure --with-backtrace\n"
end

#endif
