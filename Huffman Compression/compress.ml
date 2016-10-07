open Heap
open Io

(*
  #warnings "-3";;
  #use "heap.ml";;
  #use "io.ml";;
*)

(*Reads a text file and returns a list of paragraphs.*)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines;;

(*Stores a character at a given index into the frequency array*)
let rec line_freq char_freq line index =
  if index < String.length line then
    let char_ascii = Char.code (String.get line index) in
      Array.set char_freq char_ascii (char_freq.(char_ascii) + 1);
      line_freq char_freq line (index+1)
;;

(*Given a list of paragraphs, and an array, stores the ascii freqency of each characted into the array*)

let freq_of_char text char_freq=
  let rec freq_builder text =
    match text with
      |line::t -> line_freq char_freq line 0;
          let char_ascii = Char.code '\n' in
            Array.set char_freq char_ascii (char_freq.(char_ascii) + 1);
            freq_builder t
      |[] -> ()
  in
    freq_builder text;;


(*Part2 Huffman Tree*)
type tree = Leaf of int * char
          | Branch of int * tree * tree;;

(*weight/frequency associated to a node*)
let huffman_node_weight n =
  match n with
      Leaf(i, _) -> i
    | Branch (i, _, _) -> i;;

(*node comparer*)
let huffman_node_comparer n1 n2 =
  if (huffman_node_weight n1) > (huffman_node_weight n2) then
    false else true;;

(* returns number of non-zero weight leaves in an array of leaves*)
let count_elements arr =
  let rec helper index count =
    if index < Array.length arr then
      begin
        match arr.(index) with
          |Leaf(0,_) -> helper (index+1) count
          |_ -> helper (index+1) (count+1)
      end
    else count
  in
    helper 0 0;;

(* Given an array  of leaves, Returns an array of leaves with non-zero weight.*)
let make_tree_arr arr =
  let tree_arr1 = Array.init 256 (fun i -> Leaf(arr.(i), Char.chr i)) in
  let count = count_elements tree_arr1 in
  let tree_arr2 = Array.make count (Leaf(-1,'^')) in
  let rec helper count1 count2 =
    if count1 = Array.length tree_arr1 then () else
      match tree_arr1.(count1) with
        |Leaf(0,_) -> helper (count1 + 1) count2
        |x -> (Array.set tree_arr2 count2 x;
               helper (count1 + 1)(count2 + 1))
  in helper 0 0;
    tree_arr2;;

(* Given an array of leaves, returns a trie *)
let make_trie tree_arr =
  let tree_heap = heap_of_array (Array.length tree_arr) huffman_node_comparer tree_arr in
  let rec helper dum =
    let min1 = remove_min tree_heap in
      try
        (let min2 = remove_min tree_heap in
           insert tree_heap (Branch((huffman_node_weight min1 + huffman_node_weight min2),min1,min2));
           helper dum)
      with 
          Empty -> min1
  in
    helper ();;

(* Checks to see if code only contains zeros*)
let is_all_zero code =
  let rec helper index =
    if index = String.length code then true else
      match code.[index] with 
        |'0' -> helper (index+1)
        |_ -> false
  in
    helper 0;;

(* Given a trie and a list, stores characters and their codes in list*)
let make_list_of_trie trie lis =
  let rec helper branch code=
    match branch with
      |Branch(_,l,r) -> (helper l (code^"0");
                         helper r (code^"1"))
      |Leaf(_,k) -> if is_all_zero code then lis:= (code^"1",k)::!lis else 
            lis:= (code,k)::!lis
  in
    helper trie "";;

(* Add a suffix to list to demarcate the end of trie*)
let suffix_list ref_lis =
  let lis = !ref_lis in
  let rec helper lis =
    match lis with 
      |(k,v)::[] -> ref_lis:= !ref_lis@[(k^"1",'#')]
      |h::t -> helper t
      |[] -> ()
  in helper lis;;

(* Compress trie_list as code*)
let make_code_of_trie lis =
  let rec helper lis code =
    match lis with
      |(k,'1')::t -> helper t (code^k^"##1")
      |(k,'0')::t -> helper t (code^k^"##0")
      |(k,v)::t -> helper t (code^k^(String.make 1 v))
      |[] -> code
  in
    helper lis "";;

let process_trie trie =
  let rec helper oldT newT =
    match oldT with
      |("",v)::t -> helper t newT
      |(k,'\r')::t -> helper t ((k,'\n')::newT)
      |h::t -> helper t (h::newT)
      |[] -> newT
  in
    helper trie [];;

(* Uncompress code to trie_list*)
let make_trie_of_code code =
  let rec helper lis curr index =
    if index = String.length code then (process_trie lis) else
      begin
        match code.[index] with
          |'0' -> helper lis (curr^"0") (index+1)
          |'1' -> helper lis (curr^"1") (index+1)
          |'#' -> (if code.[index+1] == '#' 
                   then
                     helper ((curr,code.[index+2])::lis) "" (index+3)
                   else
                     helper ((curr,'#')::lis) "" (index+1))
          |k -> helper ((curr,k)::lis) "" (index+1)
      end
  in
    helper [] "" 0;;


(*Part3 Compressing text*)

(* Maps a given character to its code based on a trie_list*)
let char_to_code chr trie_list=
  let rec helper lis =
    match lis with
      |(code,x)::t -> if chr == x then code else helper t
      |[] -> "111111111111"
  in
    helper trie_list;;

(* Maps a given string to its code based on a trie_list*)
let string_to_code str trie =
  let rec helper code index =
    if index == String.length str then code else
      helper (code^(char_to_code str.[index] trie)) (index+1)
  in
    helper "" 0;;


(* Maps a given list of strings to a list of code based on a trie_list*)
let code_text text trie =
  let rec helper old_txt new_txt =
    match old_txt with
      |h::t -> helper t ((string_to_code h trie)::new_txt)
      |[] -> List.rev new_txt
  in
    helper text [];;

(* Maps a given code to its respective string using a trie_list*)
let code_to_str trie channel_in channel_out=
  let buf = Buffer.create 30 in
    try
      for i = 0 to max_int
      do
        Buffer.add_char buf (input_char channel_in);
        let rec helper trie_list buf =
          match trie_list with
            |(bin,chr)::t -> if Buffer.contents buf = bin then
                  (output_char channel_out chr;
                   Buffer.clear buf) else
                  (helper t buf)
            |[] -> ()

        in
          helper trie buf ;
      done;
    with End_of_file ->
      close_in channel_in;
      close_out channel_out;;

(* Maps a list of codes to a list of strings using a trie_list*)
let decode_text text trie =
  let rec helper old_txt new_txt =
    match old_txt with
      |h::t -> helper t ((code_to_str h trie)::new_txt)
      |[] -> List.rev new_txt
  in
    helper text [];;


(* Given a string and an output_bit writes the string to the output_bit*)
let write_bin_as_bits line output_bit =
  let rec helper index =
    if index = String.length line then 
      ()
    else
      match line.[index] with
        |'0' -> (putbit output_bit false;
                 helper (index+1))
        |_ -> (putbit output_bit true;
               helper (index+1))
  in
    helper 0;;


(*Given a list of binary strings and output file writes bits to file *)
let savebits trie huff_code coded_txt outfile = 
  let channel_out = open_out outfile in 
    output_string channel_out huff_code;
    output_string channel_out "\n\n";
    let output = output_of_channel channel_out in
    let output_bit = output_bits_of_output output in
    let rec helper txt =
      match txt with
        |h::[] -> write_bin_as_bits h output_bit;
            flush output_bit;
            close_out channel_out
        |h::t -> write_bin_as_bits h output_bit;
            write_bin_as_bits (char_to_code '\n' trie) output_bit;
            helper t
        |[] -> flush output_bit;
            close_out channel_out
    in
      helper coded_txt;;

let get_trie channel_in =
  let rec helper trie curr =
    if curr = "\r" then String.concat "\n" (List.rev trie) else
      helper (curr::trie) (input_line channel_in) 
  in
    helper [] (input_line channel_in);;

let loadbit infile outfile trie_ref =
  let channel_out = open_out outfile in
  let channel_in = open_in_bin infile in
    trie_ref := make_trie_of_code (get_trie channel_in);
    let input = input_of_channel channel_in in
    let input_bit = input_bits_of_input input in
      try
        for i = 0 to max_int
        do
          match getbit input_bit with
            |true -> output_char channel_out '1'
            |false -> output_char channel_out '0'
        done;
      with End_of_file ->
        close_in channel_in;
        close_out channel_out;;

let write_file text outfile =
  let channel_out = open_out outfile in
    output_string channel_out text;
    close_out channel_out;;

read_file "empty.txt";;


(* Compression*)
let compress fileIn fileOut =
  (* Reading the file*)
  let text = read_file fileIn in
    (* Dealing with empty file*)
    if text = [] then (
      let tmp = open_out fileOut in
        output_string tmp "\n\n";
        close_out tmp) else
      (* Extracting character frequency in file*)
      let char_freq = Array.make 256 0 in
        freq_of_char text char_freq;
        (* Making a Trie*)
        let tree_arr = make_tree_arr char_freq in
        let trie = make_trie tree_arr in
        (* Storing Huffman Code of each character in a list*)
        let lis = ref [] in
          make_list_of_trie trie lis;
          suffix_list lis;
          (* Storing Huffman Code in String form*)
          let trie_code = make_code_of_trie !lis in
          (* Compress file using Huffman Code*)
          let coded_txt = code_text text !lis in
            (* Saving Huffman Code and compressed file into a output file*)
            savebits !lis trie_code coded_txt fileOut;;

let () =
  compress Sys.argv.(1) Sys.argv.(2);;
