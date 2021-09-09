
module L = List

type compressor_t = float * float * float option

(* k is knee, r is ratio, mo is an optional makeup
 * x is input (dB). results is compressed value (dB) *)
let compress (k, r, mo) x =
  let c x = 
    if (x < k) then x
    else (k +. ((x -. k) /. r)) in
  let m = match mo with 
        | Some m -> m -. (c 0.0)
        | None -> 0.0 in
  m +. (c x)

let db_of_amp x = 20.0 *. log10 x
let amp_of_db u = 10.0 ** (u /. 20.)
let add_db u1 u2 = db_of_amp ((amp_of_db u1) +. (amp_of_db u2))

(* this simulates inputs to generate compressor outputs 
 * result is a list of (input, output) pairs *)
let render c =
  let incr = 0.5 in
  let start, end0 = -60.0, 0.0 in
  let rec f zz i =
    if i > end0 then zz
    else (f ((i, c i) :: zz) (i +. incr)) in
  f [] start
  |> L.rev

(* prints compressor outputs in sep-separated format *)
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

let lightc = (-20.0, 4.0, Some (-6.0)) (* light compressor, high knee *)
let heavyc = (-58.0, 100.0, Some (-24.0)) (* heavy compressor, low knee *)

(* sequential compression, light then heavy *)
let compressor_seq_light_heavy x =
  compress lightc x |> amp_of_db |> compress heavyc

let compressor_seq_heavy_light x =
  compress heavyc x |> amp_of_db |> compress lightc 

(* parallel compression *)
let compressor_par x =
  add_db (compress lightc x) (compress heavyc x)
;;

(* output in tab-delimited format:
  * columns: input light heavy seq1 seq2 parallel *)
print "\t" [
  render (compress lightc);
  render (compress heavyc);
  render compressor_seq_light_heavy;
  render compressor_seq_heavy_light;
  render compressor_par
]; print_string "\n";

