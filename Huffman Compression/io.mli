(* Input/output, from chapter 4 *)
type input =
    {pos_in : unit -> int;
     seek_in : int -> unit;
     input_char : unit -> char;
     in_channel_length : int}

val input_of_channel : in_channel -> input
val input_of_string : string -> input

type output =
    {output_char : char -> unit;
     output_written : unit -> int;
     out_channel_length : unit -> int}

val output_of_channel : out_channel -> output
val output_of_string : string -> output

(* Bitstream input/output, from chapter 5 *)
type input_bits
val input_bits_of_input : input -> input_bits
val getbit : input_bits -> bool
val align : input_bits -> unit
val getval : input_bits -> int -> int

type output_bits
val output_bits_of_output : output -> output_bits
val flush : output_bits -> unit
val putbit : output_bits -> bool -> unit
val putbit' : output_bits -> int -> unit
val putval : output_bits -> int -> int -> unit
val ob_written : output_bits -> int

(* Safe file I/O, my own custom blend *)
val safe_open_in : string -> (in_channel -> 'a) -> 'a
val safe_open_out : string -> (out_channel -> 'a) -> 'a
val safe_open_inout : string -> string -> (in_channel -> out_channel -> 'a) -> 'a

val input_string : in_channel -> string
