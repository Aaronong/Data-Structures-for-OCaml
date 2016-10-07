(* These ideas are taken (and debugged) from "More OCaml" by John Whitington *)

(* Input/output, from chapter 4 *)
type input =
    {pos_in : unit -> int;
     seek_in : int -> unit;
     input_char : unit -> char;
     in_channel_length : int}

  let input_of_channel ch =
   {pos_in = (fun () -> pos_in ch);
   seek_in = seek_in ch;
   input_char = (fun () -> input_char ch);
  in_channel_length = in_channel_length ch}

    let input_of_string s = 
     let pos = ref 0 in
                 {pos_in = (fun () -> !pos);
                 seek_in = (fun p -> 
     (if p < 0 then raise (Invalid_argument "seek before beginning")); 
                    pos := p);
                    input_char = (fun () -> 
     if !pos > String.length s - 1 then raise End_of_file
    else (let c = s.[!pos] in pos := !pos + 1; c));
in_channel_length = String.length s}

     type output =
    {output_char : char -> unit;
output_written : unit -> int;
  out_channel_length : unit -> int}

     let output_of_channel ch =
    let wrtn = ref 0 in
{output_char = (fun c -> output_byte ch (int_of_char c); wrtn := !wrtn + 1);
  output_written = (fun () -> !wrtn);
  out_channel_length = (fun () -> out_channel_length ch)}

                      let output_of_string s =
                      let wrtn = ref 0 in
     let pos = ref 0 in
     {output_char = (fun c -> 
    if !pos < String.length s 
then (s.[!pos] <- c; pos := !pos + 1; wrtn := !wrtn + 1) 
else raise End_of_file);
    output_written = (fun () -> !wrtn);
     out_channel_length = (fun () -> String.length s)}

(* Bitstream input/output, from chapter 5 *)
  type input_bits =
   {input : input;
   mutable byte : int;
  mutable bit : int}

    let input_bits_of_input i =
      {input = i;
      byte = 0;
      bit = 0}

    let rec getbit b =
      if b.bit = 0 then
      begin
      b.byte <- int_of_char (b.input.input_char ());
b.bit <- 128;
  getbit b
    end
else
  let r = b.byte land b.bit > 0 in
    b.bit <- b.bit / 2;
  r

        let align b =
      b.bit <- 0

let getval b n =
    if n <= 0 || n > 31 then
     raise (Invalid_argument "getval")
     else
     let r = ref 0 in
    for x = n - 1 downto 0 do
r := !r lor ((if getbit b then 1 else 0) lsl x)
  done;
   !r

  type output_bits =
{output : output;
  mutable wrtn : int;
  mutable obyte : int;
  mutable obit : int}

  let output_bits_of_output o =
    {output = o;
      wrtn = 0;
      obyte = 0;
    obit = 7}

    let flush o =
  (if o.obit < 7 then o.output.output_char (char_of_int o.obyte));
  o.obyte <- 0;
o.obit <- 7

let rec putbit' o b =
  if o.obit = (-1) then
    begin
  flush o;
  putbit' o b
end else begin
  if b <> 0 then o.obyte <- o.obyte lor (1 lsl o.obit);
    o.obit <- o.obit - 1;
o.wrtn <- o.wrtn + 1
end

      let putbit o b =
        if b then putbit' o 1 else putbit' o 0

      let putval o v l =
for x = l - 1 downto 0 do
putbit' o (v land (1 lsl x))
  done

        let ob_written ob =
        ob.wrtn

(* File I/O, my custom blend *)
let safe_open_in i f =
  let inf = open_in_bin i in
try
let r = f inf in
  close_in inf;
  r
  with
    | e -> close_in inf; raise e;;
  
let safe_open_out o f =
  let ouf = open_out_bin o in
    try
      let r = f ouf in
        close_out ouf;
        r
    with
      | e -> close_out ouf; raise e;;

  
let safe_open_inout i o f = 
  safe_open_in i (fun ic -> safe_open_out o (fun oc -> f ic oc));;

  
let input_string ic =
  let length = in_channel_length ic in
  let s = String.make length (char_of_int 0) in
  let _ = input ic s 0 length in
    s;;
