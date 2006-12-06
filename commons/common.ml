open Commonop

(*****************************************************************************)
(* Todo *)
(*****************************************************************************)

(* 

 A tracer/logger/profiler. An assert/raise/... that print the backtrace.
 Would also like to see the value of the arguments as in Perl.
 Ocamldebug is not enough. When the program loops, ocamldebug loop, 
 and only some tracing information can help. Tracing often helps
 me to find the error faster than ocamldebug.
 
 debug, tywith, fix_caml tracing, 
 try and printexn and camlp4 => better trace of exception
 fix_caml  (use camlp4 ?)
 Sux to put let _ = pr Here in, ou les Timing.
 
 How avoid to put all those ugly printf in my code
  let action = Assisted_trajectory.assisted state t_state trajectory oracle in
  let _ = Printf.printf "\n XXX action = %s\n" (string_of_action action) in 
 Can do a trace (but will not have the "action =") => with a fix_caml tricks
 how infer good func to call (we have not a generic show)

 CIL seems to have good trace, profiling  utilities functions (in ocamlutil/).
 Unison has also good functions (in ubase/).


 Advanced invariant weaving.
 cf fixed_int => call the invariant func
 type filename = string   TODO could check that exist :) type sux  

 better pre/post/... sugar
  (ca sux c explicit inv_fixed, et c let sinx_aux, ...



 Try extract all the quite generic function from LFS
 execute_and_show_progress, timeout_func, ...
 Make a generic timeout function in caml (or call timeout(1))

 let _ = if _Update then try Trajectory.doit map_name with _ -> () in
 Do via sandbox (either special form of secure(fun() -> ...).
 Sandbox time fcts check_better.

 Take common.ml2
 Y, memo, ... cf dir hack and fun




 Lourd les
 when (((match take_safe 1 !passed_tok with [Tstruct _] -> false| _ -> true)) &&
       ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true))) 
  des que ajoute des infos au token.
 De meme quand veut extraire les infos, obliger de faire une fonction qui 
 traite chacun des cas (If _,info -> info | While _,_,_,info > info ...)

 return/when/unless/ a la Perl ... sux those ifthenelse
        if depth <= 0 then (print_string "fail but run"; [])
        else if t_state > (times.((Array.length times) - 2)) 
             then (print_string "no more spline"; [])
        else 
  Ca sux ca, why ? cos dont visually see what is algorithm from error/special
  case handling.
  For me if then else mean an algorithm whereas raise when ... mean handle 
  the fucking special case (that could be filtered after by pof).


 Special form via macro (as the || die that just do a try around the call).


 let  (run_to_states_run: state -> run -> states_run) = 
  let rec aux here = function
    | [] -> []
    | act::xs -> 
        let next = next_state here x act in
        next::(run_to_states_run next xs)
                in
  aux
 => Peut faire mieux, combinator general.


 less: y'a des binary methods now in ocaml (cf manual caml) so perhaps can 
 do inter/union/... of seti/set/.. cleanly


 read_bool.

 On lisp: graham   
 complement 
 !
 fif?

 Apply fcts in turn while having the time


pof:
  what is algo, error handling (cf below, need a raise ... when ...)
  what is debugging (either printf, or Graphics, ...)

visual:
 - as in perl, map, fold, ... in special color
 - look at all the open, then assign a color per module, then
   parse the file to extract the func, then print good color when func
 - or simply put each time explicit name
 - ref and := and {contents = } and mutable in a special color,  they are
   dangerous !!
 - the types in lib.ml should be in a special color (or special symbol)
 - use inference info to colorify more (=> give feedback, as for indentation)
   related: automatically infer code (use inference feedback).
 - fix comment diezedieze cos fout en l'air l'indentation, color => need 
   emacs mode fix.

*)

(* 
 solved:  style

 let (fixed_int_to_posmap: fixed_int -> posmap) = fun fixed -> 
  let v = ((fix_to_i fixed) / (power 2 16)) in
  let _ = Printf.printf "coord xy = %d\n" v in
  v
 The need for printf make me force to name stuff => :(
 How avoid ? use 'it' special keyword ?
 update: in fact dont have to name it, use +> (fun v -> ...)  so when want
  erase debug just have to erase one line.

 Un fichier option.ml qui contient toutes les constantes/... qui peuvent etre
 modifier a runtime et un main.ml avec un getopt qui les modifie.
 update: just need call the Arg. module and have a flag.ml file

*)

(*****************************************************************************)
(* We use *)
(*****************************************************************************)
(* functions:
    =, <=, max min,  ... 
    List.rev, List.mem, List.partition, List.fold*, List.concat, ...
*)

(* let gsubst = global_replace *)

(* 
   Format can be useful. Allow to hide passing an indent_level variable.
   You use as usual the print_string function except that there is this
   automatic indent_level variable handled for you (and maybe more services).
   src: julia in coccinelle unparse_cocci

   Other useful techniques:
    - ExprAt technique (src: norman ramsey), or unwrap/rewrap with tuples.
    - continuation visitor (src: douence),
    - aspect-like fonction via add-hook with continuation (src: pad)
    - forbid polymorphic  '='  by redefining it.
    - functor like function by using nested function.
    - use better Set/Map (binary tree or hashtbl).
    - hashsons, memoize (cache), lazyize.
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* only relevant in bytecode, in native the stacklimit is the os stacklimit
   (adjustable by ulimit -s) *)
let _ =    Gc.set {(Gc.get ()) with Gc.stack_limit = 100 * 1024 * 1024}

(* now in Commonop
let (+>) o f = f o
*)

let rec (do_n: int -> (unit -> unit) -> unit) = fun i f ->
  if i = 0 then () else (f(); do_n (i-1) f)
let rec (foldn: ('a -> int -> 'a) -> 'a -> int -> 'a) = fun f acc i ->
  if i = 0 then acc else foldn f (f acc i) (i-1)

let sum_int   = List.fold_left (+) 0

let fold_left_with_index f acc =
  let rec aux acc n = function
    | [] -> acc
    | x::xs -> aux (f acc x n) (n+1) xs 
  in aux acc 0


(* let rec enum x n = if x = n then [n] else x::enum (x+1)  n *)
let rec enum x n = 
  assert (x <= n);
  let rec aux acc x n = 
    if x = n then n::acc else aux (x::acc) (x+1) n 
  in
  List.rev (aux [] x n)

let (list_of_string: string -> char list) = fun s -> 
  (enum 0 ((String.length s) - 1) +> List.map (String.get s))

let push2 v l =
  l := v :: !l

let command2 s = ignore(Sys.command s)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

let pr s = (print_string s; print_string "\n"; flush stdout)
let pr2 s = (prerr_string s; prerr_string "\n"; flush stderr)

let pr2gen x = pr2 (Dumper.dump x)

include Printf

let _chan = ref stderr
let start_log_file () = 
  _chan := open_out ( "/tmp/debugml" ^  
    (string_of_int (Unix.getuid ())) ^  ":" ^ (string_of_int (Unix.getpid()))
   )

let dolog s = output_string !_chan (s ^ "\n"); flush !_chan

let verbose_level = ref 1
let log s =  if !verbose_level >= 1 then dolog s
let log2 s = if !verbose_level >= 2 then dolog s
let log3 s = if !verbose_level >= 3 then dolog s

let pause () = (pr2 "pause: type return"; ignore(read_line ()))

(*  used by fix_caml *)
let _trace_var = ref 0
let add_var() = incr _trace_var
let dec_var() = decr _trace_var
let get_var() = !_trace_var

let (print_n: int -> string -> unit) = fun i s -> 
  do_n i (fun () -> print_string s)
let (printerr_n: int -> string -> unit) = fun i s -> 
  do_n i (fun () -> prerr_string s)

let showCodeHex xs = List.iter (fun i -> printf "%02x" i) xs

let _debug = ref true
let debugon  () = _debug := true
let debugoff () = _debug := false
let debug f = if !_debug then f () else ()

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

let get_mem() =
  command2("grep VmData /proc/" ^ string_of_int (Unix.getpid()) ^ "/status")

let memory_stat () =
  let stat = Gc.stat() in
  let conv_mo x = x * 4 / 1000000 in
  Printf.sprintf "maximal = %d Mo\n" (conv_mo stat.Gc.top_heap_words) ^
  Printf.sprintf "current = %d Mo\n" (conv_mo stat.Gc.heap_words) ^
  Printf.sprintf "lives   = %d Mo\n" (conv_mo stat.Gc.live_words)
(*         Printf.printf "fragments = %d Mo\n" (conv_mo stat.Gc.fragments); *)

let timenow () = 
  "sys:" ^ (string_of_float (Sys.time ())) ^ " seconds" ^
  ":real:" ^ 
    (let tm = Unix.time () +> Unix.gmtime in
     tm.Unix.tm_min +> string_of_int ^ " min:" ^ 
     tm.Unix.tm_sec +> string_of_int ^ ".00 seconds")

let _count1 = ref 0  
let _count2 = ref 0  
let _count3 = ref 0  
let _count4 = ref 0  
let _count5 = ref 0  

let count1 () = incr _count1
let count2 () = incr _count2
let count3 () = incr _count3
let count4 () = incr _count4
let count5 () = incr _count5

let profiling_diagnostic () = 
  Printf.sprintf 
    "count1 = %d\ncount2 = %d\ncount3 = %d\ncount4 = %d\ncount5 = %d\n" 
    !_count1 !_count2 !_count3 !_count4 !_count5

let time_func f = 
(*   let _ = Timing () in *)
  let x = f () in
(*   let _ = Timing () in *)
  x

let profile = ref false

let _profile_table = ref (Hashtbl.create 100)
let profile_start category = failwith "todo"
let profile_end category = failwith "todo"
  
let profile_code category f = 
  if not !profile then f() else begin
  let t = Unix.gettimeofday () in
  let res = f () in
  let t' = Unix.gettimeofday () in
  let (xtime, xcount) = 
    (try Hashtbl.find !_profile_table category
    with Not_found -> 
      let xtime = ref 0.0 in
      let xcount = ref 0 in
      Hashtbl.add !_profile_table category (xtime, xcount);
      (xtime, xcount)
    ) in
  xtime := !xtime +. (t' -. t);
  xcount := !xcount + 1;
  res
  end

let profile_diagnostic () = 
  if not !profile then () else begin
  let xs = 
    Hashtbl.fold (fun k v acc -> (k,v)::acc) !_profile_table [] 
      +> List.sort (fun (k1, (t1,n1)) (k2, (t2,n2)) -> compare t2 t1)
  in
  pr2 "---------------------";
  pr2 "profiling result";
  pr2 "---------------------";
  xs +> List.iter (fun (k, (t,n)) -> 
    pr2 (sprintf "%-40s : %10.3f sec %10d count" k !t !n)
    )
  end


(*****************************************************************************)
(* Test *)
(*****************************************************************************)
let example b = assert b

let _ = example (enum 1 4 = [1;2;3;4])

open Dumper
let assert_equal a b = 
  if not (a = b) 
  then failwith ("assert_equal: those 2 values are not equal:\n\t" ^ 
                 (dump a) ^ "\n\t" ^ (dump b) ^ "\n")

(*-------------------------------------------------------------------*)
let _list_bool = ref []

(* introduce a fun () ??, otherwise the calculus is made at compile time and this can be long *)
(* let (example: string -> (unit -> bool) -> unit) = fun s func -> 
  _list_bool := (s,func):: (!_list_bool)
  would like to do as a func that take 2 terms, and make an = over it
  avoid to add this ugly fun (), but pb of type, cant do that :(
*)
let (example2: string -> bool -> unit) = fun s b -> 
(* _list_bool := (s,b)::(!_list_bool) *)
  try assert b with x -> failwith s

let (test_all: unit -> unit) = fun () -> 
  List.iter (fun (s, b) -> Printf.printf "%s: %s\n" s (if b then "passed" else "failed")
	    ) !_list_bool

let (test: string -> unit) = fun s -> 
  Printf.printf "%s: %s\n" s (if (List.assoc s (!_list_bool)) then "passed" else "failed")
(* ex: let _ = example "++" ([1;2]++[3;4;5] = [1;2;3;4;5]) *)



(*****************************************************************************)
(* Quickcheck like (sfl) *)
(*****************************************************************************)

(* Better than quickcheck, cos cant do a test_all_prop in haskell cos
 * prop were function, whereas here we have not prop_Unix x = ... but
 * laws "unit" ... 
 *
 * How do without overloading ? objet ? can pass a generator as a
 * parameter, mais lourd, prefer automatic inferring of the
 * generator? But at the same time quickcheck does not do better cos
 * we must explictly type the property. So between a 
 *    prop_unit:: [Int] -> [Int] -> bool ... 
 *    prop_unit x = reverse [x] == [x] 
 * and 
 *    let _ = laws "unit" (fun x -> reverse [x] = [x]) (listg intg) 
 * no real differences.  
 *
 * Yes I define typeg generator but quickcheck too, he must define
 * class instance. I emulate the context Gen a => Gen [a] by making
 * listg take as a param a type generator. Morover I have not the pb of
 * monad. I can do random independently, so my code is more simple 
 * I think than the haskell code of quickcheck.
 *)

(*---------------------------------------------------------------------------*)
(* generator *)
(*---------------------------------------------------------------------------*)
type 'a gen = unit -> 'a

let (ig: int gen) = fun () ->
  Random.int 10
let (lg: ('a gen) -> ('a list) gen) = fun gen () -> 
  foldn (fun acc i -> (gen ())::acc) [] (Random.int 10)
let (pg: ('a gen) -> ('b gen) -> ('a * 'b) gen) = fun gen1 gen2 () -> 
  (gen1 (), gen2 ())
let polyg = ig
let (ng: (string gen)) = fun () -> 
  "a" ^ (string_of_int (ig ()))

let (oneofl: ('a list) -> 'a gen) = fun xs () -> 
  List.nth xs (Random.int (List.length xs)) 
(* let oneofl l = oneof (List.map always l) *)

let (oneof: (('a gen) list) -> 'a gen) = fun xs -> 
  List.nth xs (Random.int (List.length xs)) 

let (always: 'a -> 'a gen) = fun e () -> e

let (frequency: ((int * ('a gen)) list) -> 'a gen) = fun xs -> 
  let sums = sum_int (List.map fst xs) in
  let i = Random.int sums in
  let rec aux acc = function ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs | _ -> failwith "frequency" in
  aux 0 xs
let frequencyl l = frequency (List.map (fun (i,e) -> (i,always e)) l)


(* 
let b = oneof [always true; always false] ()
let b = frequency [3, always true; 2, always false] ()

cant do this:
 let rec (lg: ('a gen) -> ('a list) gen) = fun gen -> oneofl [[]; lg gen ()] 
 nor
 let rec (lg: ('a gen) -> ('a list) gen) = fun gen -> oneof [always []; lg gen] 
 cos caml is not as lazy as haskell :(
 fix the pb by introducing a size limit
 take the bounds/size as parameter
 morover this is needed for more complex type, how make a bintreeg ??
  we need recursion 

let rec (bintreeg: ('a gen) -> ('a bintree) gen) = fun gen () -> 
  let rec aux n = 
    if n = 0 then (Leaf (gen ()))
    else frequencyl [1, Leaf (gen ()); 4, Branch ((aux (n / 2)), aux (n / 2))] ()
  in aux 20

*)


(*---------------------------------------------------------------------------*)
(* property *)
(*---------------------------------------------------------------------------*)

(* todo, a test_all_laws, better syntax (done already a little with ig in place of intg *)
(* en cas d'erreur, print the arg that not respect *)

(* return None kan cool, et Just the_case_that_produce_the_error kan pas cool *)
let (laws: string -> ('a -> bool) -> ('a gen) -> 'a option) = fun s func gen ->
  let res = foldn (fun acc i -> let n = gen() in (n, func n)::acc) [] 1000 in
  let res = List.filter (fun (x,b) -> not b) res in
  if res = [] then None else Some (fst (List.hd res))

(*
let b = laws "unit" (fun x       -> reverse [x]          = [x]                   )ig
let b = laws "app " (fun (xs,ys) -> reverse (xs++ys)     = reverse ys++reverse xs)(pg (lg ig)(lg ig))
let b = laws "rev " (fun xs      -> reverse (reverse xs) = xs                    )(lg ig)
let b = laws "appb" (fun (xs,ys) -> reverse (xs++ys)     = reverse xs++reverse ys)(pg (lg ig)(lg ig))
let b = laws "max"  (fun (x,y)   -> x <= y ==> (max x y  = y)                       )(pg ig ig)
*)

(* with monitoring, TODO as in haskell, laws = laws2, no need for 2 func, but hard i found *)
(* todo classify, collect,forall *)

let rec (statistic_number: ('a list) -> (int * 'a) list) = function
  | []    -> []
  | x::xs -> let (splitg, splitd) = List.partition (fun y -> y = x) xs in
    (1+(List.length splitg), x)::(statistic_number splitd)

(* in pourcentage *)
let (statistic: ('a list) -> (int * 'a) list) = fun xs ->
  let stat_num = statistic_number xs in
  let totals = sum_int (List.map fst stat_num) in
  List.map (fun (i, v) -> ((i * 100) / totals), v) stat_num 
  
let (laws2: string -> ('a -> (bool * 'b)) -> ('a gen) -> ('a option * ((int * 'b) list ))) = 
  fun s func gen ->
  let res = foldn (fun acc i -> let n = gen() in (n, func n)::acc) [] 1000 in
  let stat = statistic (List.map (fun (x,(b,v)) -> v) res) in
  let res = List.filter (fun (x,(b,v)) -> not b) res in
  if res = [] then (None, stat) else (Some (fst (List.hd res)), stat)

(* let b = laws2 "max"  (fun (x,y)   -> ((x <= y ==> (max x y  = y)), x <= y))(pg ig ig) *)



(* todo, do with coarbitrary ?? idea is that given a 'a, generate a 'b
   depending of 'a and gen 'b, that is modify gen 'b, what is important is
   that each time given the same 'a, we must get the same 'b !!!
let (fg: ('a gen) -> ('b gen) -> ('a -> 'b) gen) = fun gen1 gen2 () ->
let b = laws "funs" (fun (f,g,h) -> x <= y ==> (max x y  = y)       )(pg ig ig)
 *)
(*
let one_of xs = List.nth xs (Random.int (List.length xs)) 
let take_one xs =
  if empty xs then failwith "Take_one: empty list"
  else 
    let i = Random.int (List.length xs) in
    List.nth xs i, filter_index (fun j _ -> i <> j) xs
  *)    

(*****************************************************************************)
(* Persistence *)
(*****************************************************************************)

let get_value filename = 
  let chan = open_in filename in
  let x = input_value chan in (* <=> Marshal.from_channel  *)
  (close_in chan; x)

let write_value valu filename = 
  let chan = open_out filename in
  ((* output_value chan valu;*)  (* <=> Marshal.to_channel *)
   Marshal.to_channel chan valu [Marshal.Closures];
   close_out chan) 

let write_back func filename = 
  write_value (func (get_value filename)) filename



(*****************************************************************************)
(* Counter *)
(*****************************************************************************)
let _counter = ref 0
let counter () = (_counter := !_counter +1; !_counter)

let _counter2 = ref 0
let counter2 () = (_counter2 := !_counter2 +1; !_counter2)

let _counter3 = ref 0
let counter3 () = (_counter3 := !_counter3 +1; !_counter3)

type timestamp = int

(*****************************************************************************)
(* String_of *)
(*****************************************************************************)
(* To work with the macro system autogenerated string_of and print_ function
   (kind of deriving a la haskell) *)

(* int, bool, char, float, ref ?, string *) 

let string_of_string s = "\"" ^ s "\""

let string_of_list f xs = 
  "[" ^ (xs +> List.map f +> String.concat ";" ) ^ "]"

let string_of_unit () = "()"

let string_of_array f xs =
  "[|" ^ (xs +> Array.to_list +> List.map f +> String.concat ";") ^ "|]"

let string_of_option f = function
  | None   -> "None "
  | Some x -> "Some " ^ (f x)


let print_bool x = print_string (if x then  "True" else "False")

let print_option pr = function
  | None   -> print_string "None"
  | Some x -> print_string "Some ("; pr x; print_string ")"

let print_list pr xs = 
  begin
    print_string "["; 
    List.iter (fun x -> pr x; print_string ",") xs; 
    print_string "]";
  end

(* specialised 
let (string_of_list: char list -> string) = 
  List.fold_left (fun acc x -> acc^(Char.escaped x)) ""
*)


let rec print_between between fn = function
  | [] -> ()
  | [x] -> fn x
  | x::xs -> fn x; between(); print_between between fn xs

let pp_do_in_box f      = Format.open_box 1; f(); Format.close_box ()
let pp_do_in_zero_box f = Format.open_box 0; f(); Format.close_box ()


(* Format.mli says that behaviour is undefined when there is no open_box *)
let pp_init f = 
  begin 
    Format.open_box 0; 
    f(); 
    Format.close_box();
    Format.print_newline();
  end


let pp s = Format.print_string s

(* convert something printed using format to print into a string *)
let format_to_string f =
  let o = open_out "/tmp/out" in
  Format.set_formatter_out_channel o;
  let _ = f() in
  Format.print_flush();
  Format.set_formatter_out_channel stdout;
  close_out o;
  let i = open_in "/tmp/out" in
  let lines = ref [] in
  let rec loop _ =
    let cur = input_line i in
    lines := cur :: !lines;
    loop() in
  (try loop() with End_of_file -> ());
  String.concat "\n" (List.rev !lines)


let print_xxxxxxxxxxxxxxxxx () = 
  pr2 "-----------------------------------------------------------------------"


(*****************************************************************************)
(* Macro *)
(*****************************************************************************)

(* put your macro in macro.ml4, and you can test it interactivly as in lisp *)
let macro_expand s = 
  let c = open_out "ttttt.ml" in
  begin
    output_string c s; close_out c;
    command2 ("ocamlc -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo -impl' " ^
             "-I +camlp4 -impl macro.ml4");
    command2 "camlp4o ./macro.cmo pr_o.cmo ttttt.ml";
    command2 "rm -f ttttt.ml";
  end

(*
let t = macro_expand "{ x + y | (x,y) <- [(1,1);(2,2);(3,3)] and x > 2 and y < 3}"
let x = { x + y | (x,y) <- [(1,1);(2,2);(3,3)] and x > 2 and y < 3}
let t = macro_expand "{1 .. 10}"
let x = {1 .. 10} +> List.map (fun i -> i)
let t = macro_expand "[1;2] to append to [2;4]"
let t = macro_expand "{x = 2; x = 3}"

let t = macro_expand "type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree)"
*)

  

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(* I like the list@func notation, object reminescence *)
(* let ((@): 'a -> ('a -> 'b) -> 'b) = fun a b -> b a *)
(* let o f g x = f (g x) *)
let (+>) o f = f o
let (+!>) refo f = refo := f !refo 

let ($)     f g x = g (f x)
(* dont work :( let ( ° ) f g x = f(g(x)) *)
let compose f g x = f (g x)

(* trick to have something similar to the   1 `max` 4   haskell infix notation.
   by Keisuke Nakano on the caml mailing list.
>    let ( /* ) x y = y x
>    and ( */ ) x y = x y
or 
  let ( <| ) x y = y x
  and ( |> ) x y = x y

> Then we can make an infix operator <| f |> for a binary function f.
*)

let flip f = fun a b -> f b a

let curry f x y = f (x,y)
let uncurry f (a,b) = f a b

let id = fun x -> x

let rec applyn n f o = if n = 0 then o else applyn (n-1) f (f o)

(* now in prelude 
let rec (do_n: int -> (unit -> unit) -> unit) = fun i f ->
  if i = 0 then () else (f(); do_n (i-1) f)
*)


class ['a] shared_variable_hook (x:'a) = 
  object(self)
    val mutable data = x
    val mutable registered = []
    method set x = 
      begin
        data <- x;
        pr "refresh registered";
        registered +> List.iter (fun f -> f());
      end
    method get = data
    method modify f = self#set (f self#get)
    method register f = 
      registered <- f :: registered 
  end    

(* src: from aop project *)
let rec ptFix trans elem =
  let image = trans elem in
  if (image = elem) 
  then elem (* point fixe *)
  else ptFix trans image

(* le point fixe  pour les objets *)
let rec ptFixForObjetct trans elem =
  let image = trans elem in
  if (image#equal elem) then elem (* point fixe *)
  else ptFixForObjetct trans image

let (add_hook: ('a -> ('a -> 'b) -> 'b) ref  -> ('a -> ('a -> 'b) -> 'b) -> unit) = 
 fun var f ->
  let oldvar = !var in 
  var := fun arg k -> f arg (fun x -> oldvar x k)

let (add_hook_action: ('a -> unit) ->   ('a -> unit) list ref -> unit) = 
 fun f hooks -> 
  push2 f hooks

let (run_hooks_action: 'a -> ('a -> unit) list ref -> unit) = 
 fun obj hooks -> 
  !hooks +> List.iter (fun f -> try f obj with _ -> ())


type 'a mylazy = (unit -> 'a)

let save_excursion reference f = 
  let old = !reference in
  let res = f() in
  reference := old;
  res

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

exception Todo
exception Impossible
exception Here
exception ReturnExn

(* old: let _TODO () = failwith "TODO",  now via fix_caml with raise Todo *)

open Dumper
let internal_error s = failwith ("internal error: "^s)
let error_cant_have x = internal_error ("cant have this case" ^(Dumper.dump x))

let myassert cond = if cond then () else failwith "assert error"

let warning s v = (pr2 ("Warning: " ^ s ^ "; value = " ^ (Dumper.dump v)); v)



(* emacs/lisp inspiration, (vouillon does that too in unison I think) *)
let unwind_protect f cleanup =
  try f ()
  with e -> begin cleanup e; raise e end

(* want or of merd, but cant cos cant put die ... in b (strict call) *)
let (|||) a b = try a with _ -> b



(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

(* Using the generic (=) is tempting, but it backfires, so better avoid it *)

(* To infer all the code that use an equal, and that should be
 * transformed, is not that easy, because (=) is used by many
 * functions, such as List.find, List.mem, and so on, so the strategy
 * is to turn what you were previously using into a function, because
 * (=) return an exception when applied to a function, then you simply
 * use ocamldebug to infer where the code has to be transformed 
 *)

(* src: caml list ? *)
let (=|=) : int    -> int    -> bool = (=)
let (=<=) : char   -> char   -> bool = (=)
let (=$=) : string -> string -> bool = (=)
let (=:=) : bool   -> bool   -> bool = (=)

(* the evil generic (=). I define another symbol to more easily detect
 * it, cos the '=' sign is syntaxically overloaded in caml. It is also
 * used to define function. 
 *)
let (=*=) = (=)

(* If really want to forbid to use '='
let (=) = (=|=)
*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)
let (==>) b1 b2 = if b1 then b2 else true (* could use too => *)

let (<=>) a b = if a = b then 0 else if a < b then -1 else 1

let xor a b = not (a = b)


(*****************************************************************************)
(* Char *)
(*****************************************************************************)

let string_of_char c = String.make 1 c

let is_single  = String.contains ",;()[]{}_`"
let is_symbol  = String.contains "!@#$%&*+./<=>?\\^|:-~"
let is_space   = String.contains "\n\t "
let cbetween min max c = 
  (int_of_char c) <= (int_of_char max) && 
  (int_of_char c) >= (int_of_char min)
let is_upper = cbetween 'A' 'Z'
let is_lower = cbetween 'a' 'z'
let is_alpha c = is_upper c || is_lower c
let is_digit = cbetween '0' '9'

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

(* since 3.08, div by 0 raise Div_by_rezo, and not anymore a hardware trap :)*)
let (/!) x y = if y = 0 then (log "common.ml: div by 0"; 0) else x / y

let rec (do_n: int -> (unit -> unit) -> unit) = fun i f ->
  if i = 0 then () else (f(); do_n (i-1) f)

(* now in prelude
let rec (foldn: ('a -> int -> 'a) -> 'a -> int -> 'a) = fun f acc i ->
  if i = 0 then acc else foldn f (f acc i) (i-1)
*)

let sum_float = List.fold_left (+.) 0.0
let sum_int   = List.fold_left (+) 0

let pi  = 3.14159265358979323846
let pi2 = pi /. 2.0
let pi4 = pi /. 4.0

(* 180 = pi *)
let (deg_to_rad: float -> float) = fun deg ->
  (deg *. pi) /. 180.0

let clampf = function
  | n when n < 0.0 -> 0.0
  | n when n > 1.0 -> 1.0
  | n -> n

let square x = x *. x

let rec power x n = if n = 0 then 1 else x * power x (n-1)

let between i min max = i > min && i < max

let (between_strict: int -> int -> int -> bool) = fun a b c -> 
  a < b && b < c


let bitrange x p = let v = power 2 p in between x (-v) v

(* descendant *)
let (prime1: int -> int option)  = fun x -> 
  let rec aux n = 
    if n = 1 then None
    else 
      if (x / n) * n = x then Some n else aux (n-1)
  in if x = 1 then None else if x < 0 then failwith "negative" else aux (x-1)

(* montant, better *)
let (prime: int -> int option)  = fun x -> 
  let rec aux n = 
    if n = x then None
    else 
      if (x / n) * n = x then Some n else aux (n+1)
  in if x = 1 then None else if x < 0 then failwith "negative" else aux 2

let sum xs = List.fold_left (+) 0 xs
let product = List.fold_left ( * ) 1


let decompose x = 
  let rec decompose x = 
  if x = 1 then []
  else 
    (match prime x with
    | None -> [x]
    | Some n -> n::decompose (x / n)
    ) 
  in assert (product (decompose x) = x); decompose x

let mysquare x = x * x
let sqr a = a *. a


type compare = Equal | Inf | Sup
let (<=>) a b = if a = b then Equal else if a < b then Inf else Sup
let (<==>) a b = if a = b then 0 else if a < b then -1 else 1

type uint = int


let int_of_stringchar s = 
  fold_left_with_index (fun acc e i -> acc + (Char.code e*(power 8 i))) 0 (List.rev (list_of_string s))

let int_of_base s base = 
  fold_left_with_index (fun acc e i -> 
    let j = Char.code e - Char.code '0' in
    if j >= base then failwith "not in good base"
    else acc + (j*(power base i))
		       )
    0  (List.rev (list_of_string s))

let int_of_stringbits s = int_of_base s 2
let _ = example (int_of_stringbits "1011" = 1*8 + 1*2 + 1*1)

let int_of_octal s = int_of_base s 8
let _ = example (int_of_octal "017" = 15)

(* let int_of_hex s = int_of_base s 16, NONONONO cos 'A' - '0' does not give 10 !! *)

let int_of_all s = 
  if String.length s >= 2 && (String.get s 0 = '0') && is_digit (String.get s 1)
  then int_of_octal s else int_of_string s


let (+=) ref v = ref := !ref + v
let (-=) ref v = ref := !ref - v

(*****************************************************************************)
(* Numeric/overloading *)
(*****************************************************************************)

type 'a numdict = 
    NumDict of (('a-> 'a -> 'a) * 
		('a-> 'a -> 'a) * 
		('a-> 'a -> 'a) * 
		('a -> 'a));;

let add (NumDict(a, m, d, n)) = a;;
let mul (NumDict(a, m, d, n)) = m;;
let div (NumDict(a, m, d, n)) = d;;
let neg (NumDict(a, m, d, n)) = n;;

let numd_int   = NumDict(( + ),( * ),( / ),( ~- ));;
let numd_float = NumDict(( +. ),( *. ), ( /. ),( ~-. ));;
let testd dict n = 
  let ( * ) x y = mul dict x y in 
  let ( / ) x y = div dict x y in 
  let ( + ) x y = add dict x y in 
  (* Now you can define all sorts of things in terms of *, /, + *) 
  let f num = (num * num) / (num + num) in 
  f n;;




(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let thd3 (_,_,z) = z

let map_fst f (x, y) = f x, y
let map_snd f (x, y) = x, f y

let pair  f (x,y) = (f x, f y)

(* for my ocamlbeautify script *)
let snd = snd
let fst = fst

let double a = a,a
let swap (x,y) = (y,x)


(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

(* type 'a maybe  = Just of 'a | None *)

type ('a,'b) either = Left of 'a | Right of 'b
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c

let just = function
  | (Some x) -> x
  | _ -> failwith "just: pb"

let some = just


let fmap f = function
  | None -> None
  | Some x -> Some (f x)
let map_option = fmap

let do_option f = function
  | None -> ()
  | Some x -> f x

let optionise f = 
  try Some (f ()) with Not_found -> None



(* pix *)
let some_or = function
  | None -> id
  | Some e -> fun _ -> e


let partition_either f l =
  let rec part_either left right = function
  | [] -> (List.rev left, List.rev right)
  | x :: l -> 
      (match f x with
      | Left  e -> part_either (e :: left) right l
      | Right e -> part_either left (e :: right) l) in
  part_either [] [] l


(* pix *)
let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

let map_filter f xs = xs +> List.map f +> filter_some

let rec find_some p = function
  | [] -> raise Not_found
  | x :: l -> 
      match p x with
      |	Some v -> v
      |	None -> find_some p l

(* same
let map_find f xs = 
  xs +> List.map f +> List.find (function Some x -> true | None -> false)
    +> (function Some x -> x | None -> raise Impossible)
*)



(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

let slength = String.length
let concat = String.concat

let i_to_s = string_of_int
let s_to_i = int_of_string


(* strings take space in memory. Better when can share the space used by
   similar strings *)
let _shareds = Hashtbl.create 100
let (shared_string: string -> string) = fun s -> 
  try Hashtbl.find _shareds s 
  with Not_found -> (Hashtbl.add _shareds s s; s)

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)


let (<!!>) s (i,j) = 
  String.sub s i (if j < 0 then String.length s - i + j + 1 else j - i)
(* let _ = example  ( "tototati"<!!>(3,-2) = "otat" ) *)

let (<!>) s i = String.get s i 

(* pix *)
let rec split_on_char c s =
  try
    let sp = String.index s c in
    String.sub s 0 sp :: 
    split_on_char c (String.sub s (sp+1) (String.length s - sp - 1))
  with Not_found -> [s]


let lowercase = String.lowercase

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

(* Different from Perl a little. Must match the entire way. 
 *  So  "testBee" =~ "Bee" is wrong  
 *  but "testBee" =~ ".*Bee" is right
 *)

(*
-let (=~) s re = Str.string_match (Str.regexp re) s 0 
-let (==~) s re = Str.string_match re s 0 
*)

let (regexp_match: string -> string -> string) = fun s re -> 
  let _ = assert(s =~ re) in
  Str.matched_group 1 s

(* beurk, side effect code, but hey, it is convenient *)
let (matched: int -> string -> string) = fun i s -> 
  Str.matched_group i s

let matched1 = fun s -> matched 1 s
let matched2 = fun s -> (matched 1 s, matched 2 s)
let matched3 = fun s -> (matched 1 s, matched 2 s, matched 3 s)
let matched4 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
let matched5 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
let matched6 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)



let split sep s = Str.split (Str.regexp sep) s
let join  sep xs = String.concat sep xs
let _ = example (join "/" ["toto"; "titi"; "tata"] = "toto/titi/tata")
(*
let rec join str = function
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ str ^ (join str xs)
*)


let (split_list_regexp: string -> string list -> (string * string list) list) =
 fun re xs ->
  let rec aux (heading, accu) = function
    | [] -> [(heading, List.rev accu)]
    | x::xs -> 
        if x =~ re 
        then (heading, List.rev accu)::aux (x, []) xs
        else aux (heading, x::accu) xs
  in
  aux ("__noheading__", []) xs 
  +> (fun xs -> if (List.hd xs) = ("__noheading__",[]) then List.tl xs else xs)


(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

let dirname = Filename.dirname
let basename = Filename.basename

type filename = string (* TODO could check that exist :) type sux *)

let (filesuffix: filename -> string) = fun s -> 
  (try regexp_match s ".+\\.\\([a-zA-Z0-9_]+\\)$" with _ ->  "NOEXT")
let (fileprefix: filename -> string) = fun s -> 
  (try regexp_match s "\\(.+\\)\\.\\([a-zA-Z0-9_]+\\)?$" with _ ->  s)

let _ = example (filesuffix "toto.c" = "c")
let _ = example (fileprefix "toto.c" = "toto")

(*
assert (s = fileprefix s ^ filesuffix s)

let withoutExtension s = global_replace (regexp "\\..*$") "" s
let () = example "without"
    (withoutExtension "toto.s.toto" = "toto")
*)


(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

let int_to_month i = 
  assert (i <= 12 && i >= 1);
  match i with
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> raise Impossible


(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

let (list_of_string: string -> char list) = fun s -> 
  (enum 0 ((String.length s) - 1) +> List.map (String.get s))

let _ = example (list_of_string "abcd" = ['a';'b';'c';'d'])

(*
let rec (list_of_stream: ('a Stream.t) -> 'a list) = 
parser
  | [< 'c ; stream >]  -> c :: list_of_stream stream
  | [<>]               -> []

let (list_of_string: string -> char list) = 
  Stream.of_string $ list_of_stream
*)

let (lines: string -> string list) = fun s -> 
  let rec lines_aux = function
    | [] -> []
    | [x] -> if x = "" then [] else [x ^ "\n"] (* old: [x] *)
    | x::xs -> 
        let e = x ^ "\n" in
        e::lines_aux xs 
  in
  (time_func (fun () -> Str.split_delim (Str.regexp "\n") s)) +> lines_aux
(* in fact better make it return always complete lines, simplify *)
(*  Str.split, but lines "\n1\n2\n" dont return the \n and forget the first \n => split_delim better than split *)
(* +> List.map (fun s -> s ^ "\n") but add an \n even at the end => lines_aux *)
(* old: slow
  let chars = list_of_string s in
  chars +> List.fold_left (fun (acc, lines) char -> 
    let newacc = acc ^ (String.make 1 char) in
    if char = '\n' 
    then ("", newacc::lines)
    else (newacc, lines)
    ) ("", []) 
       +> (fun (s, lines) -> List.rev (s::lines))
*)

(*  CHECK: unlines (lines x) = x *)
let (unlines: string list -> string) = fun s -> 
  (String.concat "\n" s) ^ "\n"
let (words: string -> string list)   = fun s -> 
  Str.split (Str.regexp "[ \t()\";]+") s
let (unwords: string list -> string) = fun s -> 
  String.concat "" s

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
let cat_orig file = 
  let chan = open_in file in
  let rec aux ()  = 
    try 
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
      let l = input_line chan in
      l :: aux ()
    with End_of_file -> [] in
  aux()

(* tail recursive efficient version *)
let cat file = 
  let chan = open_in file in
  let rec aux acc ()  = 
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b 
    then aux (l::acc) ()
    else acc 
  in
  aux [] () +> List.rev +> (fun x -> close_in chan; x)

let interpolate str = 
  begin
    command2 ("printf \"%s\\n\" " ^ str ^ ">/tmp/caml");
    cat "/tmp/caml"
  end
(* could do a print_string but printf dont like print_string *)
let echo s = printf "%s" s; flush stdout; s 

let usleep s = for i = 1 to s do () done

(* now in prelude 
let command2 s = ignore(Sys.command s)
*)

let do_in_fork f = 
  let pid = Unix.fork () in
  if pid = 0
  then 
    begin 
      (* Unix.setsid(); *)
      Sys.set_signal Sys.sigint (Sys.Signal_handle   (fun _ -> 
        pr2 "being killed";
        Unix.kill 0 Sys.sigkill;
        ));
      f(); 
      exit 0;
    end
  else pid


let process_output_to_list = fun command -> 
  let chan = Unix.open_process_in command in
  let rec aux () =  
    try 
      let e = input_line chan in
      e::aux()
    with End_of_file -> begin ignore(Unix.close_process_in chan); [] end
  in aux ()

let read_file file = cat file +> unlines

let write_file file s = 
  let chan = open_out file in
  (output_string chan s; close_out chan)

let filesize file = 
  (Unix.stat file).Unix.st_size


let lfile_exists filename = 
  try 
    (match (Unix.lstat filename).Unix.st_kind with
    | (Unix.S_REG | Unix.S_LNK) -> true
    | _ -> false
    )
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false
    
      
(* src: from chailloux et al book *)
let capsule_unix f args = 
  try (f args) 
  with Unix.Unix_error (e, fm, argm) -> 
    log (Printf.sprintf "exn Unix_error: %s %s %s\n" (Unix.error_message e) fm argm)


let (readdir_to_kind_list: string -> Unix.file_kind -> string list) = 
 fun path kind -> 
  Sys.readdir path 
  +> Array.to_list 
  +> List.filter (fun s -> 
    try 
      let stat = Unix.lstat (path ^ "/" ^  s) in
      stat.Unix.st_kind = kind
    with e -> 
      pr2 ("EXN pb stating file: " ^ s);
      false
    )

let (readdir_to_dir_list: string -> string list) = fun path -> 
  readdir_to_kind_list path Unix.S_DIR

let (readdir_to_file_list: string -> string list) = fun path -> 
  readdir_to_kind_list path Unix.S_REG

let (readdir_to_link_list: string -> string list) = fun path -> 
  readdir_to_kind_list path Unix.S_LNK


let (readdir_to_dir_size_list: string -> (string * int) list) = fun path -> 
  Sys.readdir path 
  +> Array.to_list 
  +> map_filter (fun s -> 
    let stat = Unix.lstat (path ^ "/" ^  s) in
    if stat.Unix.st_kind = Unix.S_DIR 
    then Some (s, stat.Unix.st_size) 
    else None
    )
  
(* taken from mlfuse, the predecessor of ocamlfuse *)
type rwx = [`R|`W|`X] list
let file_perm_of : u:rwx -> g:rwx -> o:rwx -> Unix.file_perm = 
 fun ~u ~g ~o ->
  let to_oct l = 
    List.fold_left (fun acc p -> acc lor ((function `R -> 4 | `W -> 2 | `X -> 1) p)) 0 l in
  let perm = 
    ((to_oct u) lsl 6) lor
    ((to_oct g) lsl 3) lor
    (to_oct o)
  in
  perm


(* pix *)
let has_env var = 
  try 
    let _ = Sys.getenv var in true
  with Not_found -> false

(* emacs/lisp inspiration (eric cooper and yaron minsky use that too) *)
let (with_open_outfile: filename -> (((string -> unit) * out_channel) -> 'a) -> 'a) = 
 fun file f ->
  let chan = open_out file in
  let pr s = output_string chan s in
  unwind_protect (fun () -> 
    let res = f (pr, chan) in
    close_out chan;
    res)
    (fun e -> close_out chan)

let (with_open_infile: filename -> ((in_channel) -> 'a) -> 'a) = fun file f ->
  let chan = open_in file in
  unwind_protect (fun () -> 
    let res = f chan in
    close_in chan;
    res)
    (fun e -> close_in chan)


exception Timeout

(* it seems that the toplevel block such signals, even with this explicit
   command :( 
let _ = Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigalrm]
*)

(* subtil: have to make sure that timeout is not intercepted before here, so 
 * avoid exn handle such as try (...) with _ -> cos timeout will not bubble up
 * enough. In such case, add a case before such as  
 * with Timeout -> raise Timeout | _ -> ... 
 *)
let timeout_function timeoutval = fun f -> 
  try 
    begin
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout ));
      ignore(Unix.alarm timeoutval);
      let x = f() in
      ignore(Unix.alarm 0);
      x
    end
  with Timeout -> 
    begin 
      log "timeout (we abort)";
      raise Timeout;
    end
  | e -> 
     (* subtil: important to disable the alarm before relaunching the exn,
      * otherwise the alarm is still running.
      * robust?: and if alarm launch after the log (...) ? 
      *)
      begin 
        log ("exn while in transaction (we abort too, even if ...) = " ^ Printexc.to_string e);
        ignore(Unix.alarm 0);
        raise e
      end


(*****************************************************************************)
(* List *)
(*****************************************************************************)

(* pix *)
let uncons l = (List.hd l, List.tl l)

(* pix *)
let safe_tl l = try List.tl l with _ -> []

let push l v =
  l := v :: !l

let rec zip xs ys = 
  match (xs,ys) with
  | ([],[]) -> []
  | ([],_) -> failwith "zip: not same length"
  | (_,[]) -> failwith "zip: not same length"
  | (x::xs,y::ys) -> (x,y)::zip xs ys

let rec zip_safe xs ys = 
  match (xs,ys) with
  | ([],_) -> []
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::zip_safe xs ys

let rec unzip zs = 
  List.fold_right (fun e (xs, ys)    -> 
    (fst e::xs), (snd e::ys)) zs ([],[])


let rec take n xs = 
  match (n,xs) with
  | (0,_) -> []
  | (_,[]) -> failwith "take: not enough"
  | (n,x::xs) -> x::take (n-1) xs

let rec take_safe n xs =
  match (n,xs) with
  | (0,_) -> []
  | (_,[]) -> []
  | (n,x::xs) -> x::take_safe (n-1) xs

let rec take_until p = function
  | [] -> []
  | x::xs -> if p x then [] else x::(take_until p xs)

let take_while p = take_until (p $ not)


let rec drop n xs = 
  match (n,xs) with
  | (0,_) -> xs
  | (_,[]) -> failwith "drop: not enough"
  | (n,x::xs) -> drop (n-1) xs

let _ = example (drop 3 [1;2;3;4] = [4])

let rec drop_while p = function
  | [] -> []
  | x::xs -> if p x then drop_while p xs else x::xs	

let span p xs = (take_while p xs, drop_while p xs)


let rec (span: ('a -> bool) -> 'a list -> 'a list * 'a list) = 
 fun p -> function
  | []    -> ([], [])
  | x::xs -> 
      if p x then 
	let (l1, l2) = span p xs in
	(x::l1, l2)
      else ([], x::xs)
let _ = example ((span (fun x -> x <= 3) [1;2;3;4;1;2] = ([1;2;3],[4;1;2])))

(* generate exception (Failure "tl") if there is no element satisfying p *)
let rec (skip_until: ('a list -> bool) -> 'a list -> 'a list) = fun p xs ->
  if p xs then xs else skip_until p (List.tl xs)
let _ = example (skip_until (function 1::2::xs -> true | _ -> false) [1;3;4;1;2;4;5] = [1;2;4;5])

let rec skipfirst e = function
  | [] -> []
  | e'::l when e = e' -> skipfirst e l
  | l -> l


(* now in prelude 
let rec enum x n = if x = n then [n] else x::enum (x+1)  n
let rec enum x n = 
  let _ = Assert (x <= n) in
  let rec aux acc x n = 
    if x = n then n::acc else aux (x::acc) (x+1) n 
  in
  List.rev (aux [] x n)
*)


let index_list xs = zip xs (enum 0 ((List.length xs) -1))

let or_list  = List.fold_left (||) false
let and_list = List.fold_left (&&) true

let snoc x xs = xs @ [x]
let cons x xs = x::xs

let head_middle_tail xs = 
  match xs with
  | x::y::xs -> 
      let head = x in
      let reversed = List.rev (y::xs) in
      let tail = List.hd reversed in
      let middle = List.rev (List.tl reversed) in
      head, middle, tail
  | _ -> failwith "head_middle_tail, too small list"

let _ = assert_equal (head_middle_tail [1;2;3]) (1, [2], 3)

let (++) = (@)

(* let (++) = (@), could do that, but if load many times the common, then pb *)
(* let (++) l1 l2 = List.fold_right (fun x acc -> x::acc) l1 l2 *)

let remove x xs = 
  let newxs = List.filter (fun y -> y <> x) xs in
  let _ = assert (List.length newxs = List.length xs - 1) in
  newxs

let foldl1 p = function x::xs -> List.fold_left p x xs | _ -> failwith "foldl1"

let fold_k f lastk acc xs = 
  let rec aux acc = function
    | [] -> lastk acc
    | x::xs -> 
        f acc x (fun acc -> aux acc xs)
  in
  aux acc xs


let rec list_init = function
  | []       -> raise Not_found
  | [x]      -> []
  | x::y::xs -> x::(list_init (y::xs))

let rec list_last = function
  | [] -> raise Not_found
  | [x] -> x
  | x::y::xs -> list_last (y::xs)

(* pix *)
let last_n n l = List.rev (take n (List.rev l))
let last l = List.hd (last_n 1 l)

let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)


(* todo: foldl, foldr (a more consistent foldr) *)

(* start pix *)
let iter_index f l =
  let rec iter_ n = function
    | [] -> ()
    | e::l -> f e n ; iter_ (n+1) l
  in iter_ 0 l

let map_index f l =
  let rec map_ n = function
    | [] -> []
    | e::l -> f e n :: map_ (n+1) l
  in map_ 0 l


(* pix *)
let filter_index f l =
  let rec filt i = function
    | [] -> []
    | e::l -> if f i e then e :: filt (i+1) l else filt (i+1) l
  in
  filt 0 l

(* pix *)
let do_withenv doit f env l =
  let r_env = ref env in
  let l' = doit (fun e -> 
    let e', env' = f !r_env e in
    r_env := env' ; e'
  ) l in
  l', !r_env

(* could call it for *)
let fold_left_with_index f acc =
  let rec aux acc n = function
    | [] -> acc
    | x::xs -> aux (f acc x n) (n+1) xs 
  in aux acc 0
  
let map_withenv      f env e = do_withenv List.map f env e

let rec collect_accu f accu = function
  | [] -> accu
  | e::l -> collect_accu f (List.rev_append (f e) accu) l

let collect f l = List.rev (collect_accu f [] l)

let rec fpartition p l =
  let rec part yes no = function
  | [] -> (List.rev yes, List.rev no)
  | x :: l -> 
      (match p x with
      |	None -> part yes (x :: no) l
      |	Some v -> part (v :: yes) no l) in
  part [] [] l

(* end pix *)

let rec removelast = function
  | [] -> failwith "removelast"
  | [_] -> []
  | e::l -> e :: removelast l

let remove x = List.filter (fun y -> y != x)
let empty list = list = []


let rec inits = function
  | [] -> [[]]
  | e::l -> [] :: List.map (fun l -> e::l) (inits l)

let rec tails = function
  | [] -> [[]]
  | (_::xs) as xxs -> xxs :: tails xs


let reverse = List.rev
let rev = List.rev

let nth = List.nth
let fold_left = List.fold_left
let rev_map = List.rev_map

(* pix *)
let rec fold_right1 f = function
  | [] -> failwith "fold_right1"
  | [e] -> e
  | e::l -> f e (fold_right1 f l)

let maximum l = foldl1 max l
let minimum l = foldl1 min l

(* do a map tail recursive, and result is reversed, it is a tail recursive map => efficient *)
let map_eff_rev = fun f l ->
  let rec aux acc = 
    function 
      |	[]    -> acc
      |	x::xs -> aux ((f x)::acc) xs
  in
  aux [] l

let rec (generate: int -> 'a -> 'a list) = fun i el ->
  if i = 0 then []
  else el::(generate (i-1) el)

let rec uniq = function
  | [] -> []
  | e::l -> if List.mem e l then uniq l else e :: uniq l

let rec all_assoc e = function
  | [] -> []
  | (e',v) :: l when e=e' -> v :: all_assoc e l
  | _ :: l -> all_assoc e l

let prepare_want_all_assoc l =
  List.map (fun n -> n, uniq (all_assoc n l)) (uniq (List.map fst l))

let rotate list = List.tl list ++ [(List.hd list)]

let or_list  = List.fold_left (||) false
let and_list = List.fold_left (&&) true

let rec (return_when: ('a -> 'b option) -> 'a list -> 'b) = fun p -> function
  | [] -> raise Not_found
  | x::xs -> (match p x with None -> return_when p xs | Some b -> b)

let rec splitAt n xs = 
  if n = 0 then ([],xs)
  else 
    (match xs with
    | []      -> ([],[])
    | (x::xs) -> let (a,b) = splitAt (n-1) xs in (x::a, b)
    )

let pack n xs = 
  let rec aux l i = function
    | [] -> failwith "not on a boundary"
    | [x] -> if i = n then [l++[x]] else failwith "not on a boundary"
    | x::xs -> if i = n then (l++[x])::(aux [] 1 xs) else aux (l++[x]) (i+1) xs in
  aux [] 1 xs

let min_with f = function
  | [] -> raise Not_found
  | e :: l ->
      let rec min_with_ min_val min_elt = function
	| [] -> min_elt
	| e::l -> 
	    let val_ = f e in
	    if val_ < min_val 
	    then min_with_ val_ e l
	    else min_with_ min_val min_elt l
      in min_with_ (f e) e l

let two_mins_with f = function
  | e1 :: e2 :: l ->
      let rec min_with_ min_val min_elt min_val2 min_elt2 = function
	| [] -> min_elt, min_elt2
	| e::l -> 
	    let val_ = f e in
	    if val_ < min_val2 
	    then
	      if val_ < min_val
	      then min_with_ val_ e min_val min_elt l
	      else min_with_ min_val min_elt val_ e l
	    else min_with_ min_val min_elt min_val2 min_elt2 l
      in 
      let v1 = f e1 in
      let v2 = f e2 in
      if v1 < v2 then min_with_ v1 e1 v2 e2 l else min_with_ v2 e2 v1 e1 l 
  | _ -> raise Not_found

let grep_with_previous f = function
  | [] -> []
  | e::l ->
      let rec grep_with_previous_ previous = function
	| [] -> []
	| e::l -> if f previous e then e :: grep_with_previous_ e l else grep_with_previous_ previous l
      in e :: grep_with_previous_ e l

let iter_with_previous f = function
  | [] -> ()
  | e::l ->
      let rec iter_with_previous_ previous = function
	| [] -> ()
	| e::l -> f previous e ; iter_with_previous_ e l
      in iter_with_previous_ e l


(* kind of cartesian product of x*x  *)
let rec (get_pair: ('a list) -> (('a * 'a) list)) = function
  | [] -> []
  | x::xs -> (List.map (fun y -> (x,y)) xs) ++ (get_pair xs)


(* retourne le rang dans une liste d'un element *)
let rang elem liste =
  let rec rang_rec elem accu = function
    | []   -> raise Not_found
    | a::l -> if a = elem then accu
    else rang_rec elem (accu+1) l in
  rang_rec elem 1 liste

(* retourne vrai si une liste contient des doubles *)
let rec doublon = function
  | []   -> false
  | a::l -> if List.mem a l then true
  else doublon l

let rec (inseredans: 'a -> 'a list -> 'a list list) = fun x -> function
  | []    -> [[x]]
  | y::ys -> (x::y::ys)  :: (List.map (fun xs -> y::xs) (inseredans x ys))
(* [1;2;3] -> 3   [2;3] [3;2] *)

let rec (permutation: 'a list -> 'a list list) = function
  | [] -> []
  | [x] -> [[x]]
  | x::xs -> List.flatten (List.map (inseredans x) (permutation xs))


(* pix *)
let rec map_flatten f l =
  let rec aux accu = function    
    | [] -> accu
    | e :: l -> aux (List.rev (f e) ++ accu) l
  in List.rev (aux [] l)


let rec repeat e n = 
    let rec aux acc = function
      | 0 -> acc
      | n when n < 0 -> failwith "repeat"
      | n -> aux (e::acc) (n-1) in
    aux [] n

let rec map2 f = function 
  | [] -> []
  | x::xs -> let r = f x in r::map2 f xs

let rec map3 f l = 
  let rec aux acc = function
    | [] -> acc 
    | x::xs -> aux (f x::acc) xs in
  aux [] l

(*
let tails2 xs = map rev (inits (rev xs))
let res = tails2 [1;2;3;4]
let res = tails [1;2;3;4]
let id x = x 
*)

let pack_sorted same xs = 
    let rec aux acc xs = 
      match (acc,xs) with
      |	((cur,rest),[]) -> cur::rest
      |	((cur,rest), y::ys) -> 
	  if same (List.hd cur) y then aux (y::cur, rest) ys
	  else aux ([y], cur::rest) ys
    in aux ([List.hd xs],[]) (List.tl xs) +> List.rev
let test = pack_sorted (=) [1;1;1;2;2;3;4]

let rec uniq2 = function
  | [] -> []
  | e::l -> if List.mem e l then uniq2 l else e :: uniq2 l

let rec keep_best f = 
  let rec partition e = function
    | [] -> e, []
    | e' :: l ->
	match f(e,e') with
	| None -> let (e'', l') = partition e l in e'', e' :: l'
	| Some e'' -> partition e'' l
  in function
  | [] -> []
  | e::l -> 
      let (e', l') = partition e l in
      e' :: keep_best f l'

let rec sorted_keep_best f = function
  | [] -> []
  | [a] -> [a]
  | a :: b :: l -> 
      match f a b with
      |	None -> a :: sorted_keep_best f (b :: l)
      |	Some e -> sorted_keep_best f (e :: l)



let (cartesian_product: 'a list -> 'b list -> ('a * 'b) list) = fun xs ys -> 
  xs +> List.map (fun x ->  ys +> List.map (fun y -> (x,y)))
     +> List.flatten

let _ = assert_equal 
    (cartesian_product [1;2] ["3";"4";"5"]) 
    [1,"3";1,"4";1,"5";  2,"3";2,"4";2,"5"]

(*----------------------------------*)

(* sur surEnsemble [p1;p2] [[p1;p2;p3] [p1;p2] ....] -> [[p1;p2;p3] ...      *)
(* mais pas p2;p3                                                            *)
(* (aop) *)
let surEnsemble  liste_el liste_liste_el = 
  List.filter
    (function liste_elbis ->
      List.for_all (function el -> List.mem el liste_elbis) liste_el
    ) liste_liste_el;;



(*----------------------------------*)
(* combinaison/product/.... (aop) *)
(* 123 -> 123 12 13 23 1 2 3 *)
let rec realCombinaison = function
  | []  -> []
  | [a] -> [[a]]
  | a::l  -> 
      let res  = realCombinaison l in
      let res2 = List.map (function x -> a::x) res in
      res2 ++ res ++ [[a]]

(* genere toutes les combinaisons possible de paire      *)
(* par exemple combinaison [1;2;4] -> [1, 2; 1, 4; 2, 4] *)
let rec combinaison = function
  | [] -> []
  | [a] -> []
  | [a;b] -> [(a, b)]
  | a::b::l -> (List.map (function elem -> (a, elem)) (b::l)) ++
     (combinaison (b::l))

(*----------------------------------*)

(* list of list(aop) *)
(* insere elem dans la liste de liste (si elem est deja present dans une de  *)
(* ces listes, on ne fait rien                                               *)
let rec insere elem = function
  | []   -> [[elem]]
  | a::l -> 
      if (List.mem elem a) then a::l
      else a::(insere elem l)

let rec insereListeContenant lis el = function
  | []   -> [el::lis]
  | a::l -> 
      if List.mem el a then 
	(List.append lis a)::l
      else a::(insereListeContenant lis el l)

(* fusionne les listes contenant et1 et et2  dans la liste de liste*)
let rec fusionneListeContenant (et1, et2) = function
  | []   -> [[et1; et2]]
  | a::l -> 
      (* si les deux sont deja dedans alors rien faire *)
      if List.mem et1 a then
	if List.mem et2 a then a::l
	else 
	  insereListeContenant a et2 l
      else if List.mem et2 a then
	insereListeContenant a et1 l
      else a::(fusionneListeContenant (et1, et2) l)

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

let array_find_index f a =
  let rec array_find_index_ i =
    if f a.(i) then i else array_find_index_ (i+1)
  in
  try array_find_index_ 0 with _ -> raise Not_found

(*****************************************************************************)
(* Fast array *)
(*****************************************************************************)
(*
module B_Array = Bigarray.Array2
*)

(*
open B_Array
open Bigarray
*)

let b_array_string_of_t f a = "<>"
let bigarray_string_of_int16_unsigned_elt a = "<>"
let bigarray_string_of_c_layout a = "<>"

(*****************************************************************************)
(* Set. Have a look too at set*.mli  *)
(*****************************************************************************)
type 'a set = 'a list

let (empty_set: 'a set) = []
let (insert_set: 'a -> 'a set -> 'a set) = fun x xs -> 
  if List.mem x xs 
  then (* let _ = print_string "warning insert: already exist" in *) 
    xs 
  else x::xs

let (single_set: 'a -> 'a set) = fun x -> insert_set x empty_set
let (set: 'a list -> 'a set) = fun xs -> 
  xs +> List.fold_left (flip insert_set) empty_set 

let (exists_set: ('a -> bool) -> 'a set -> bool) = List.exists
let (forall_set: ('a -> bool) -> 'a set -> bool) = List.for_all
let (filter_set: ('a -> bool) -> 'a set -> 'a set) = List.filter
let (fold_set: ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a) = List.fold_left
let (map_set: ('a -> 'b) -> 'a set -> 'b set) = List.map
let (member_set: 'a -> 'a set -> bool) = List.mem

let find_set = List.find
let sort_set = List.sort
let iter_set = List.iter

let (top_set: 'a set -> 'a) = List.hd

let (inter_set: 'a set -> 'a set -> 'a set) = fun s1 s2 -> 
  s1 +> fold_set (fun acc x -> if member_set x s2 then insert_set x acc else acc) empty_set
let (union_set: 'a set -> 'a set -> 'a set) = fun s1 s2 -> 
  s2 +> fold_set (fun acc x -> if member_set x s1 then acc else insert_set x acc) s1
let (minus_set: 'a set -> 'a set -> 'a set) = fun s1 s2 -> 
  s1 +> filter_set  (fun x -> not (member_set x s2))

let big_union_set f xs = xs +> map_set f +> fold_set union_set empty_set

let (card_set: 'a set -> int) = List.length

let (include_set: 'a set -> 'a set -> bool) = fun s1 s2 -> 
  (s1 +> forall_set (fun p -> member_set p s2))

let equal_set s1 s2 = include_set s1 s2 && include_set s2 s1

let (include_set_strict: 'a set -> 'a set -> bool) = fun s1 s2 -> 
  (card_set s1 < card_set s2) && (include_set s1 s2)

let ($*$) = inter_set
let ($+$) = union_set
let ($-$) = minus_set
let ($?$) = member_set
let ($<$) = include_set_strict
let ($<=$) = include_set
let ($=$) = equal_set

(* as $+$ but do not check for memberness, allow to have set of func *)
let ($@$) = fun a b -> a @ b 

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)
(*
let (union: 'a list -> 'a list -> 'a list) = fun l1 l2 -> 
  List.fold_left (fun acc x -> if List.mem x l1 then acc else x::acc) l1 l2

let insert_normal x xs = union xs [x]

(* retourne lis1 - lis2 *)
let minus l1 l2 = List.filter    (fun x -> not (List.mem x l2)) l1

let inter l1 l2 = List.fold_left (fun acc x -> if List.mem x l2 then x::acc else acc) [] l1

let union_list =  List.fold_left union []

let uniq lis = 
  List.fold_left (function acc -> function el -> union [el] acc) [] lis

(* pix *)
let rec non_uniq = function
  | [] -> []
  | e::l -> if mem e l then e :: non_uniq l else non_uniq l

let rec inclu lis1 lis2 =
  List.for_all (function el -> List.mem el lis2) lis1

let equivalent lis1 lis2 = 
  (inclu lis1 lis2) && (inclu lis2 lis1)

*)


(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)
(* liste trie, cos we need to do intersection, and insertion (it is a set
   cos when introduce has, if we create a new has => must do a recurse_rep
   and another categ can have to this has => must do an union
 *)
(*
let rec insert x = function
  | [] -> [x]
  | y::ys -> 
      if x = y then y::ys
      else (if x < y then x::y::ys else y::(insert x ys))

(* same, suppose sorted list *)
let rec intersect x y =
  match(x,y) with
  | [], y -> []
  | x,  [] -> []
  | x::xs, y::ys -> 
      if x = y then x::(intersect xs ys)
      else 
	(if x < y then intersect xs (y::ys)
	else intersect (x::xs) ys
	)
(* intersect [1;3;7] [2;3;4;7;8];;   *)
*)

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
type ('a,'b) assoc  = ('a * 'b) list


let (assoc_to_function: ('a, 'b) assoc -> ('a -> 'b)) = fun xs ->
  xs +> List.fold_left (fun acc (k, v) -> 
    (fun k' -> 
      if k = k' then v else acc k'
    )) (fun k -> failwith "no key in this assoc")
(* simpler: 
let (assoc_to_function: ('a, 'b) assoc -> ('a -> 'b)) = fun xs ->
  fun k -> List.assoc k xs
*)

let (empty_assoc: ('a, 'b) assoc) = []
let fold_assoc = List.fold_left
let insert_assoc = fun x xs -> x::xs
let map_assoc = List.map
let filter_assoc = List.filter

let assoc = List.assoc
let keys xs = List.map fst xs

let lookup = assoc

(* assert unique key ?*)
let del_assoc key xs = xs +> List.filter (fun (k,v) -> k <> key)
let replace_assoc (key, v) xs = insert_assoc (key, v) (del_assoc key xs)

let apply_assoc key f xs = 
  let old = assoc key xs in
  replace_assoc (key, f old) xs

let big_union_assoc f xs = xs +> map_assoc f +> fold_assoc union_set empty_set

(* todo: pb normally can suppr fun l -> .... l but if do that, then strange type _a
 => assoc_map is strange too => equal dont work
*)
let (assoc_reverse: (('a * 'b) list) -> (('b * 'a) list)) = fun l -> 
  List.map (fun(x,y) -> (y,x)) l

let (assoc_map: (('a * 'b) list) -> (('a * 'b) list) -> (('a * 'a) list)) = 
 fun l1 l2 ->
  let (l1bis, l2bis) = (assoc_reverse l1, assoc_reverse l2) in
  List.map (fun (x,y) -> (y, List.assoc x l2bis )) l1bis

let rec (lookup_list: 'a -> ('a , 'b) assoc list -> 'b) = fun el -> function
  | [] -> raise Not_found
  | (xs::xxs) -> try List.assoc el xs with Not_found -> lookup_list el xxs

let (lookup_list2: 'a -> ('a , 'b) assoc list -> ('b * int)) = fun el xxs -> 
  let rec aux i = function
  | [] -> raise Not_found
  | (xs::xxs) -> 
      try let res = List.assoc el xs in (res,i) with Not_found -> aux (i+1) xxs
  in aux 0 xxs

let _ = example (lookup_list2 "c" [["a",1;"b",2];["a",1;"b",3];["a",1;"c",7]] = (7,2))

(*****************************************************************************)
(* Assoc int -> xxx with binary tree.  Have a look too at Mapb.mli *)
(*****************************************************************************)

(* ex: type robot_list = robot_info IntMap.t *)
module IntMap = Map.Make
    (struct
      type t = int
      let compare = compare 
    end)
let intmap_to_list m = IntMap.fold (fun id v acc -> (id, v) :: acc) m []
let intmap_string_of_t f a = "<Not Yet>"

module IntIntMap = Map.Make
    (struct
      type t = int * int
      let compare = compare 
end)

let intintmap_to_list m = IntIntMap.fold (fun id v acc -> (id, v) :: acc) m []
let intintmap_string_of_t f a = "<Not Yet>"


(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

let hcreate () = Hashtbl.create 401 (* il parait que better  when choose a prime *)
let hadd (k,v) h = Hashtbl.add h k v
let hmem k h = Hashtbl.mem h k
let hfind k h = Hashtbl.find h k
let hreplace (k,v) h = Hashtbl.replace h k v
let hiter = Hashtbl.iter
let hfold = Hashtbl.fold
let hremove k h = Hashtbl.remove h k

let find_hash_set key value_if_not_found h = 
  try Hashtbl.find h key
  with Not_found -> 
    (Hashtbl.add h key (value_if_not_found ()); Hashtbl.find h key)


let hash_to_list h = 
  Hashtbl.fold (fun k v acc -> (k,v)::acc) h [] 
  +> List.sort compare 

let hash_of_list xs = 
  let h = Hashtbl.create 101 in
  begin
    xs +> List.iter (fun (k, v) -> Hashtbl.add h k v);
    h
  end

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t 

let hash_hashset_add k e h = 
  match optionise (fun () -> Hashtbl.find h k) with
  | Some hset -> Hashtbl.replace hset e true
  | None -> 
      let hset = Hashtbl.create 100 in
      begin
        Hashtbl.add h k hset;
        Hashtbl.replace hset e true;
      end

let hashset_to_set baseset h = 
 h +> hash_to_list +> List.map fst +> (fun xs -> baseset#fromlist xs) 


(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
type 'a stack = 'a list

let (empty_stack: 'a stack) = []
let (push: 'a -> 'a stack -> 'a stack) = fun x xs -> x::xs
let (top: 'a stack -> 'a) = List.hd
let (pop: 'a stack -> 'a stack) = List.tl


(* now in prelude 
let push2 v l =
  l := v :: !l
*)

let pop2 l = 
  let v = List.hd !l in
  let _ = l := List.tl !l in
  v


(*****************************************************************************)
(* Binary tree *)
(*****************************************************************************)
type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree)

(*****************************************************************************)
(* Graph. Have a look too at Ograph_*.mli  *)
(*****************************************************************************)
(* todo: generalise to put in common (need 'edge (and 'c ?), 
 * and take in param a display func, cos caml sux, no overloading of show :( 
 * Simple impelemntation. Can do also matrix, or adjacent list, or pointer (ref)
 * todo: do some check (dont exist already, ...)
 *)

type 'node graph = ('node set) * (('node * 'node) set)

let (add_node: 'a -> 'a graph -> 'a graph) = fun node (nodes, arcs) -> 
  (node::nodes, arcs)

let (del_node: 'a -> 'a graph -> 'a graph) = fun node (nodes, arcs) -> 
  (nodes $-$ set [node], arcs) 
(* could do more job:   let _ = assert (successors node (nodes, arcs) = empty) in
   +> List.filter (fun (src, dst) -> dst != node))
*)
let (add_arc: ('a * 'a) -> 'a graph -> 'a graph) = fun arc (nodes, arcs) -> 
  (nodes, set [arc] $+$ arcs)

let (del_arc: ('a * 'a) -> 'a graph -> 'a graph) = fun arc (nodes, arcs) -> 
  (nodes, arcs +> List.filter (fun a -> not (arc = a)))

let (successors: 'a -> 'a graph -> 'a set) = fun x (nodes, arcs) -> 
  arcs +> List.filter (fun (src, dst) -> src = x) +> List.map snd

let (predecessors: 'a -> 'a graph -> 'a set) = fun x (nodes, arcs) -> 
  arcs +> List.filter (fun (src, dst) -> dst = x) +> List.map fst

let (nodes: 'a graph -> 'a set) = fun (nodes, arcs) -> nodes

(* pre: no cycle *)
let rec (fold_upward: ('b -> 'a -> 'b) -> 'a set -> 'b -> 'a graph  -> 'b) = 
 fun f xs acc graph -> 
  match xs with
  | [] -> acc
  | x::xs -> (f acc x) 
        +> (fun newacc -> fold_upward f (graph +> predecessors x) newacc graph)
        +> (fun newacc -> fold_upward f xs newacc graph) 
   (* TODO avoid already visited *)

let empty_graph = ([], [])



(*
let (add_arcs_toward: int -> (int list) -> 'a graph -> 'a graph) = fun i xs -> function
    (nodes, arcs) -> (nodes, (List.map (fun j -> (j,i) ) xs)++arcs)
let (del_arcs_toward: int -> (int list) -> 'a graph -> 'a graph) = fun i xs g ->
    List.fold_left (fun acc el -> del_arc (el, i) acc) g xs
let (add_arcs_from: int -> (int list) -> 'a graph -> 'a graph) = fun i xs -> function
    (nodes, arcs) -> (nodes, (List.map (fun j -> (i,j) ) xs)++arcs)


let (del_node: (int * 'node) -> 'node graph -> 'node graph) = fun node -> function
    (nodes, arcs) -> let newnodes = List.filter (fun a -> not (node = a)) nodes in
    if newnodes = nodes then (raise Not_found) else (newnodes, arcs)
let (replace_node: int -> 'node -> 'node graph -> 'node graph) = fun i n -> function
    (nodes, arcs) -> let newnodes = List.filter (fun (j,_) -> not (i = j)) nodes in
    ((i,n)::newnodes, arcs)
let (get_node: int -> 'node graph -> 'node) = fun i -> function
    (nodes, arcs) -> List.assoc i nodes

let (get_free: 'a graph -> int) = function
    (nodes, arcs) -> (maximum (List.map fst nodes))+1
(* require no cycle !! TODO if cycle check that we have already visited a node *)
let rec (succ_all: int -> 'a graph -> (int list)) = fun i -> function
    (nodes, arcs) as g -> 
      let direct = succ i g in
      union direct (union_list (List.map (fun i -> succ_all i g) direct))
let rec (pred_all: int -> 'a graph -> (int list)) = fun i -> function
    (nodes, arcs) as g -> 
      let direct = pred i g in
      union direct (union_list (List.map (fun i -> pred_all i g) direct))
(* require that the nodes are different !! *)
let rec (equal: 'a graph -> 'a graph -> bool) = fun g1 g2 ->
  let ((nodes1, arcs1),(nodes2, arcs2)) = (g1,g2) in
  try 
    let conv = assoc_map nodes1 nodes2 in (* do 2 things, check same length and to assoc *)
    List.for_all (fun (i1,i2) -> List.mem (List.assoc i1 conv, List.assoc i2 conv) arcs2) arcs1 
      && (List.length arcs1 = List.length arcs2)
    (* could think that only forall is needed, but need check same lenth too !!*)
  with _ -> false

let (display: 'a graph -> ('a -> unit) -> unit) = fun g display_func -> 
  let rec aux depth i = 
    print_n depth " ";
    print_int i; print_string "->"; display_func (get_node i g); print_string "\n";
    List.iter (aux (depth+2)) (succ i g)
  in aux 0 1

let (display_dot: 'a graph -> ('a -> string) -> unit) = fun (nodes, arcs) func ->
  let file = open_out "test.dot" in
  output_string file "digraph misc {\n" ;
  List.iter (fun (n, node) -> output_int file n; output_string file " [label=\"";
    output_string file (func node); output_string file " \"];\n"; ) nodes;
  List.iter (fun (i1,i2) ->  output_int file i1 ; output_string file " -> " ; 
    output_int file i2 ; output_string file " ;\n"; ) arcs;
  output_string file "}\n" ;
  close_out file;
  let status = Unix.system "viewdot test.dot" in
  ()
(* todo: faire = graphe (int can change !!! => cant make simply =)
   reassign number first !!
 *)

(* todo: mettre diff(modulo = !!) en rouge *)
let (display_dot2: 'a graph -> 'a graph -> ('a -> string) -> unit) = 
  fun (nodes1, arcs1) (nodes2, arcs2) func ->
  let file = open_out "test.dot" in
  output_string file "digraph misc {\n" ;
  output_string file "rotate = 90;\n";
  List.iter (fun (n, node) ->
    output_string file "100"; output_int file n; output_string file " [label=\"";
    output_string file (func node); output_string file " \"];\n"; ) nodes1;
  List.iter (fun (n, node) ->
    output_string file "200"; output_int file n; output_string file " [label=\"";
    output_string file (func node); output_string file " \"];\n"; ) nodes2;
  List.iter (fun (i1,i2) -> 
    output_string file "100"; output_int file i1 ; output_string file " -> " ; 
    output_string file "100"; output_int file i2 ; output_string file " ;\n"; ) arcs1;
  List.iter (fun (i1,i2) -> 
    output_string file "200"; output_int file i1 ; output_string file " -> " ;
    output_string file "200"; output_int file i2 ; output_string file " ;\n"; ) arcs2;
(*  output_string file "500 -> 1001; 500 -> 2001}\n" ; *)
  output_string file "}\n" ;
  close_out file;
  let status = Unix.system "viewdot test.dot" in
  ()


*)
(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)
(* overloading *)

let map = List.map (* note: really really slow, use rev_map if possible *)
let filter = List.filter
let fold = List.fold_left
let member = List.mem
let iter = List.iter
let find = List.find
let exists = List.exists
let forall = List.for_all
let big_union f xs = xs +> map f +> fold union_set empty_set
let empty = []
let sort = List.sort
let length = List.length
let null xs = match xs with [] -> true | _ -> false
let head = List.hd
let tail = List.tl
let is_singleton = fun xs -> List.length xs = 1

(*****************************************************************************)
(* Geometry (raytracer) *)
(*****************************************************************************)

type vector = (float * float * float)
type point = vector
type color = vector (* color(0-1) *)

(* todo: factorise *)
let (dotproduct: vector * vector -> float) = 
  fun ((x1,y1,z1),(x2,y2,z2)) -> (x1*.x2 +. y1*.y2 +. z1*.z2)
let (vector_length: vector -> float) = 
  fun (x,y,z) -> sqrt (square x +. square y +. square z)
let (minus_point: point * point -> vector) = 
  fun ((x1,y1,z1),(x2,y2,z2)) -> ((x1 -. x2),(y1 -. y2),(z1 -. z2))
let (distance: point * point -> float) = 
  fun (x1, x2) -> vector_length (minus_point (x2,x1))
let (normalise: vector -> vector) = 
  fun (x,y,z) -> 
    let len = vector_length (x,y,z) in (x /. len, y /. len, z /. len)
let (mult_coeff: vector -> float -> vector) = 
  fun (x,y,z) c -> (x *. c, y *. c, z *. c)
let (add_vector: vector -> vector -> vector) = 
  fun v1 v2 -> let ((x1,y1,z1),(x2,y2,z2)) = (v1,v2) in
  (x1+.x2, y1+.y2, z1+.z2)
let (mult_vector: vector -> vector -> vector) = 
  fun v1 v2 -> let ((x1,y1,z1),(x2,y2,z2)) = (v1,v2) in
  (x1*.x2, y1*.y2, z1*.z2)
let sum_vector = List.fold_left add_vector (0.0,0.0,0.0)

(*****************************************************************************)
(* Pics (raytracer) *)
(*****************************************************************************)

type pixel = (int * int * int) (* RGB *)

(* required pixel list in row major order, line after line *)
let (write_ppm: int -> int -> (pixel list) -> string -> unit) = fun
  width height xs filename ->
    let chan = open_out filename in
    begin
     output_string chan "P6\n";
     output_string chan ((string_of_int width)  ^ "\n");
     output_string chan ((string_of_int height) ^ "\n");
     output_string chan "255\n";
     List.iter (fun (r,g,b) -> 
       List.iter (fun byt -> output_byte chan byt) [r;g;b]
	       ) xs;
     close_out chan
    end
    
let test1 () = write_ppm 100 100 
    ((generate (50*100) (1,45,100)) ++ (generate (50*100) (1,1,100)))
    "img.ppm"

(*****************************************************************************)
(* Diff (lfs) *)
(*****************************************************************************)
type diff = Match | BnotinA | AnotinB

let (diff: (int -> int -> diff -> unit)-> (string list * string list) -> unit)=
  fun f (xs,ys) -> 
    let file1 = "/tmp/diff1-" ^ (string_of_int (Unix.getuid ())) in
    let file2 = "/tmp/diff2-" ^ (string_of_int (Unix.getuid ())) in
    let fileresult = "/tmp/diffresult-" ^ (string_of_int (Unix.getuid ())) in
    write_file file1 (unwords xs);
    write_file file2 (unwords ys);
    command2 
      ("diff --side-by-side -W 1 " ^ file1 ^ " " ^ file2 ^ " > " ^ fileresult);
    let res = cat fileresult in
    let a = ref 0 in
    let b = ref 0 in
    res +> List.iter (fun s -> 
      match s with
      | ("" | " ") -> f !a !b Match; incr a; incr b;
      | ">" -> f !a !b BnotinA; incr b;
      | ("|" | "/" | "\\" ) -> 
          f !a !b BnotinA; f !a !b AnotinB; incr a; incr b;
      | "<" -> f !a !b AnotinB; incr a;
      | _ -> raise Impossible
    )
(*    
let _ = 
  diff 
    ["0";"a";"b";"c";"d";    "f";"g";"h";"j";"q";            "z"]
    [    "a";"b";"c";"d";"e";"f";"g";"i";"j";"k";"r";"x";"y";"z"]  
   (fun x y -> pr "match") (fun x y -> pr "a_not_in_b") (fun x y -> pr "b_not_in_a") 
*)

let (diff2: (int -> int -> diff -> unit) -> (string * string) -> unit) = 
 fun f (xs,ys) -> 
    write_file "/tmp/diff1" xs;
    write_file "/tmp/diff2" ys;
    command2 
     ("diff --side-by-side --left-column -W 1 " ^ 
      "/tmp/diff1 /tmp/diff2 > /tmp/diffresult");
    let res = cat "/tmp/diffresult" in
    let a = ref 0 in
    let b = ref 0 in
    res +> List.iter (fun s -> 
      match s with
      | "(" -> f !a !b Match; incr a; incr b;
      | ">" -> f !a !b BnotinA; incr b;
      | "|" -> f !a !b BnotinA; f !a !b AnotinB; incr a; incr b;
      | "<" -> f !a !b AnotinB; incr a;
      | _ -> raise Impossible
    )


(*****************************************************************************)
(* Parsers (aop-colcombet)                                                 *)
(*****************************************************************************)

let parserCommon lexbuf parserer lexer =
  try 
    let result = parserer lexer lexbuf in
    result
  with Parsing.Parse_error ->
    print_string "buf: "; print_string lexbuf.Lexing.lex_buffer;
    print_string "\n";
    print_string "current: "; print_int lexbuf.Lexing.lex_curr_pos;
    print_string "\n";
    raise Parsing.Parse_error


(* marche pas ca neuneu *)
(*
let getDoubleParser parserer lexer string = 
  let lexbuf1 = Lexing.from_string string in
  let chan = open_in string in
  let lexbuf2 = Lexing.from_channel chan in
  (parserCommon lexbuf1 parserer lexer  , parserCommon lexbuf2 parserer lexer )
*)

let getDoubleParser parserer lexer = 
  (
   (function string ->
     let lexbuf1 = Lexing.from_string string in
     parserCommon lexbuf1 parserer lexer
   ),
   (function string ->
     let chan = open_in string in
     let lexbuf2 = Lexing.from_channel chan in
     parserCommon lexbuf2 parserer lexer
   ))



(*****************************************************************************)
(* parser related (cocci) *)
(*****************************************************************************)

type pos_file = ((int * int) * int) (* (line * column), charpos) *)

type parse_info = {
    str: string;
    charpos: int;
  } 
let fake_parse_info = { charpos = -1; str = "" }

let (charpos_to_pos2: int -> filename -> (filename * int * int * string)) = 
 fun charpos filename ->

  (* Currently lexing.ml does not handle the line number position,  
   * even if there is some fields in the lexing structure, they are not 
   * maintained by the lexing engine :( So the following code does not work:
   *   let pos = Lexing.lexeme_end_p lexbuf in 
   *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum  
   *      (pos.pos_cnum - pos.pos_bol) in 
   * Hence this function to overcome the previous limitation.
   *)
  let chan = open_in filename in
  let linen  = ref 0 in
  let posl   = ref 0 in
  let rec aux () =
    let s = (input_line chan) in
    let _ = incr linen in
    let s = s ^ "\n" in
    if (!posl + slength s > charpos)
    then
      let _ = close_in chan in
      (filename, !linen, charpos - !posl, s)
    else 
      begin
        posl := !posl + slength s;
        aux ();
      end
  in aux ()

let charpos_to_pos a b = 
  profile_code "Common.charpos_to_pos" (fun () -> charpos_to_pos2 a b)

(*---------------------------------------------------------------------------*)
let (full_charpos_to_pos: filename -> (int * int) array ) = fun filename ->

    let arr = Array.create (filesize filename + 2) (0,0) in

    let chan = open_in filename in

    let charpos   = ref 0 in
    let line  = ref 0 in

    let rec aux () =
     try
       let s = (input_line chan) in
       let _ = incr line in

       for i = 0 to (slength s - 1) + 1 do (*  +1 cos input_line dont return the trailing \n *)
         arr.(!charpos + i) <- (!line, i);
       done;
       charpos := !charpos + slength s + 1;
       aux();
       
     with End_of_file -> 
       for i = !charpos to Array.length arr - 1 do
         arr.(i) <- (!line, 0);
       done;
       ();
    in 
    begin 
      aux ();
      close_in chan;
      arr
    end
    
let test_charpos file = 
  full_charpos_to_pos file +> Dumper.dump +> pr2

(*---------------------------------------------------------------------------*)
(* decalage is here to handle stuff such as cpp which include file and who 
   can make decalage *)
let (error_messagebis: filename -> (string * int (* *int *)) -> int -> string)=
 fun filename (lexeme, lexstart(*, lexend*)) decalage ->

  (* let posdiff = lexend   - lexstart in *)
  let charpos = lexstart      + decalage in
  let tok = lexeme in 
  let (file, line, pos, linecontent) =  charpos_to_pos charpos filename in
  sprintf "File \"%s\", line %d, characters %d
    around = '%s', whole content = %s charpos = %d"
    filename line pos (* (pos+posdiff) *) tok linecontent charpos
 (* characters %d-%d *)

let error_message = fun filename (lexeme, lexstart) -> 
  try 
    error_messagebis filename (lexeme, lexstart) 0    
  with End_of_file -> 
    pr2 ("PB in Common.error_message, position given out of file" ^ filename);
    raise End_of_file

(*****************************************************************************)
(* Misc/test *)
(*****************************************************************************)

let size_mo_ko i = 
  let ko = (i / 1024) mod 1024 in
  let mo = (i / 1024) / 1024 in
  (if mo > 0 
  then sprintf "%dMo%dKo" mo ko
  else sprintf "%dKo" ko
  )

let plural i s = 
  if i=1 
  then Printf.sprintf "%d %s" i s 
  else Printf.sprintf "%d %ss" i s

let sec_to_days sec = 
  let minfactor = 60 in
  let hourfactor = 60 * 60 in
  let dayfactor = 60 * 60 * 24 in

  let days  =  sec / dayfactor in
  let hours = (sec mod dayfactor) / hourfactor in
  let mins  = (sec mod hourfactor) / minfactor in
  (* old:   Printf.sprintf "%d days, %d hours, %d minutes" days hours mins *)
  (if days > 0  then plural days "day" ^ " "    else "") ^ 
  (if hours > 0 then plural hours "hour" ^ " " else "") ^
  (if mins > 0  then plural mins "min"   ^ " " else "") ^
  ""

let _ = Random.self_init ()

(*
let random_insert i l = 
    let p = Random.int (length l +1)
    in let rec insert i p l = 
      if (p = 0) then i::l else (hd l)::insert i (p-1) (tl l)
    in insert i p l

let rec randomize_list = function 
  []  -> []
  | a::l -> random_insert a (randomize_list l)
*)
let random_list xs = 
  List.nth xs (Random.int (length xs))                                           

let randomize_list xs = 
  let permut = permutation xs in
  random_list permut

(* let (test: 'a osetb -> 'a ocollection) = fun o -> (o :> 'a ocollection) *)
(* let _ = test (new osetb (Setb.empty)) *)

(*------------------------*)
let (generic_print: 'a -> string -> string) = fun v typ -> 
  write_value v "/tmp/generic_print";
  command2 
   ("printf 'let (v:" ^ typ ^ ")= Common.get_value \"/tmp/generic_print\" " ^
     " in v;;' " ^
     " | calc.top > /tmp/result_generic_print"); 
   cat "/tmp/result_generic_print" 
   +> drop_while (fun e -> not (e =~ "^#.*")) +> tail
   +> unlines
   +> (fun s -> 
       if (s =~ ".*= \\(.+\\)") 
       then matched1 s 
       else "error in generic_print, not good format:" ^ s)
   
(* let main () = pr (generic_print [1;2;3;4] "int list") *)

class ['a] olist (ys: 'a list) =
  object(o)
    val xs = ys
    method view = xs
(*    method fold f a = List.fold_left f a xs *)
    method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =                                              
      fun f accu -> List.fold_left f accu xs                                                        
  end


(* let _ = write_value ((new setb[])#add 1) "/tmp/test" *)
let typing_sux_test () = 
   let x = Obj.magic [1;2;3] in
   let f1 xs = List.iter print_int xs in
   let f2 xs = List.iter print_string xs in
   (f1 x; f2 x)

(* let _ = if not !Sys.interactive then (main ()) *)

