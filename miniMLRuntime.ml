let print_byte x = print_char (char_of_int x) in
let prerr_byte x = prerr_char (char_of_int x) in

let buf = Buffer.create 16 in

(* let chan = open_in_bin "piero1.sld" *) (* for ocamldebug *)

let rec read_token in_token =
  try
    let c = input_char stdin (* chan *) in
    match c with
      ' ' | '\t' | '\r' | '\n' ->
        if in_token then ()
        else read_token false
    | _ ->
        Buffer.add_char buf c;
        read_token true
  with
    End_of_file ->
      if in_token then () else raise End_of_file
 in
let read_float () =
  Buffer.clear buf;
  read_token false;
  try
    float_of_string (Buffer.contents buf)
  with
    Failure _ -> failwith ((Buffer.contents buf) ^ ": float conversion failed.")
 in
let read_int () =
  Buffer.clear buf;
  read_token false;
  try
    int_of_string (Buffer.contents buf)
  with
    Failure _ -> failwith ((Buffer.contents buf) ^ ": int conversion failed.")
 in
