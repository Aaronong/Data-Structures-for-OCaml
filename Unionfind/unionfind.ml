

(* Exceptions*)
exception AlreadyThere (* if we are already in the structure *)
exception NotValid (* if we are out of bounds *)
exception NotFound


(*Code*)
(* uf_node consists of (Rank, Parent) *)
type uf_node = int * int;;

type uf_t = uf_node array;;


(* Make functions*)

let make_func num :uf_node =
  (-1,-1);;

let make (size : int) : uf_t = 
  Array.init size make_func;;


(* make the rank 1 and root itself *)
let singleton (tree: uf_t) (node:int) = 
  tree.(node) <- (1,node);;

let make_full_func num :uf_node = 
  (1, num);;

let make_full (size: int) :uf_t = 
  Array.init size make_full_func;;


(*UnionFind functions*)

let is_valid (tree: uf_t) (node:int) =
  if (node >= Array.length tree) then raise NotValid;; 

let rec find_nocompress (tree:uf_t) (node: int) = 
  is_valid tree node;
  match tree.(node) with
    |rank, parent -> if parent == node then parent else
          find_nocompress tree parent;;

let find (tree:uf_t) (node: int) =
  let root = find_nocompress tree node in
  let rec compress tree node root =
    match tree.(node) with
      |rank, parent -> if parent == node then parent else
            (tree.(node) <- (rank,root);
             compress tree parent root)
  in
    compress tree node root;;

let union (tree:uf_t) (a:int) (b:int) =
  match (find tree a), (find tree b) with 
    |root_a , root_b ->
        match tree.(root_a), tree.(root_b) with
          |(rank_a, _ ), (rank_b, _) ->
              if rank_a >= rank_b then
                (tree.(root_b) <- (rank_b, root_a);
                 tree.(root_a) <- ((rank_a + rank_b), root_a)) else
                (tree.(root_a) <- (rank_a, root_b);
                 tree.(root_b) <- ((rank_b + rank_a), root_b));;

let union_norank (tree:uf_t) (a:int) (b:int) =
  match (find tree a), (find tree b) with
    |root_a, root_b ->
        tree.(root_b) <- (1, root_a);;

let union_nocompress (tree:uf_t) (a:int) (b:int) =
  match (find_nocompress tree a), (find_nocompress tree b) with 
    |root_a , root_b ->
        match tree.(root_a), tree.(root_b) with
          |(rank_a, _ ), (rank_b, _) ->
              if rank_a >= rank_b then
                (tree.(root_b) <- (rank_b, root_a);
                 tree.(root_a) <- ((rank_a + rank_b), root_a)) else
                (tree.(root_a) <- (rank_a, root_b);
                 tree.(root_b) <- ((rank_b + rank_a), root_b));;

let union_norank_nocompress (tree:uf_t) (a:int) (b:int) =
  match (find_nocompress tree a), (find_nocompress tree b) with
    |root_a, root_b ->
        tree.(root_b) <- (1, root_a);;


let count (tree:uf_t) =
  let length = Array.length tree in
  let rec count_helper index counter (tree:uf_t) = 
    if index >= length then counter else
      (match tree.(index) with
        |(rank,parent) -> if index = parent then 
              count_helper (index+1) (counter+1) tree else
              count_helper (index+1) counter tree)
  in
    count_helper 0 0 tree;;

(* Run Times

   aaron_linux@Aaron-GS70-Stealth-Pro-LinuxMint ~/Desktop/AaronCode/FOP/Ocaml_ASS_1 $ ./tinyUF tinyUF.txt
   Loading 10 elements and 11 instructions took 0.00 seconds; sorting took 0.00 seconds.
   UF1: test not run (use "all" after the file name)
   UF2: 2 components in 0.00 seconds using compression only.
   UF3: 2 components in 0.00 seconds using ranks only.
   UF4: 2 components in 0.00 seconds using ranks and compression.
   UF1: 0.00 seconds for finds (no compression).
   UF2: 0.00 seconds for finds.
   UF3: 0.00 seconds for finds (no compression).
   UF4: 0.00 seconds for finds.

   aaron_linux@Aaron-GS70-Stealth-Pro-LinuxMint ~/Desktop/AaronCode/FOP/Ocaml_ASS_1 $ ./tinyUF mediumUF.txt
   Loading 625 elements and 900 instructions took 0.01 seconds; sorting took 0.00 seconds.
   UF1: test not run (use "all" after the file name)
   UF2: 3 components in 0.00 seconds using compression only.
   UF3: 3 components in 0.00 seconds using ranks only.
   UF4: 3 components in 0.00 seconds using ranks and compression.
   UF1: 0.00 seconds for finds (no compression).
   UF2: 0.00 seconds for finds.
   UF3: 0.00 seconds for finds (no compression).
   UF4: 0.00 seconds for finds.


*)
