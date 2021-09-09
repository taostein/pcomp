
module L = List

type compressor_t = float * float;;
let compress c x = 
  let knee, ratio = c in
  if (x < knee) then x
  else (knee +. ((x -. knee) /. ratio))
;;

let makeup c x = x -. (compress c 0.0);;

let compress_makeup c y x =
  (makeup c y) +. (compress c x);;

let db_of_amp x = 20.0 *. log10 x;;
let amp_of_db u = 10.0 ** (u /. 20.);;
let add_db u1 u2 = db_of_amp ((amp_of_db u1) +. (amp_of_db u2));;

(* this simulates inputs to generate compressor outputs 
 * result is a list of (input, output) pairs
 * *)
let render c mopt =
  let incr = 0.5 in
  let start, end0 = -60.0, 0.0 in
  let rec f zz i =
    if i > end0 then zz
    else (
      let j = match mopt with
        | None -> compress c i
        | Some m -> compress_makeup c m i in
      f ((i, j) :: zz) (i +. incr)
    ) in
  f [] start
  |> L.rev
;;

(* this just prints a series of compressor outputs in
 * sep-separated format *)
let rec print sep xss =
  let sof = string_of_float in
  let rec f xss =
    if L.hd xss = [] then ()
    else (
      let _ = List.fold_left (
        fun zz x -> (print_string 
          ((if zz then (sof (fst (L.hd x))) else "") ^ sep ^
          (sof (snd (L.hd x)))); 
          false))
        true xss in 
      print_string "\n";
      f (L.map L.tl xss)
    ) in
  f xss
;;

(* adds together the simulated results of two compressors
 * results is the same format as the inputs
 *)
let addys =
  List.map2 (fun (x1, y1) (x2, y2) -> 
      if (x1 <> x2) then raise (Invalid_argument "x vals not equal");
      (x1, add_db y1 y2))
;;

(* done definitions. Now execute *)

let xys1 = render (-20.0, 4.0) (Some (-6.0));;
let xys2 = render (-58.0, 100.0) (Some (-24.0));;
let xys3 = addys xys1 xys2;;

(* output in tab-delimited format *)
print "\t" [xys1; xys2; xys3]; print_string "\n";

