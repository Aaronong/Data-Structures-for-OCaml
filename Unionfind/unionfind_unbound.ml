

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

let make () : uf_t = 
  Array.init 500 make_func;;

let make_full_func num :uf_node = 
  (1, num);;


let arr_cpy (tree_old:uf_t) (size:int) : uf_t =
  let arr_length = Array.length tree_old in
  let tree_new = Array.init (size) make_full_func in
  let rec helper counter =
    if counter >= arr_length then tree_new else 
      (tree_new.(counter) <- tree_old.(counter);
       helper (counter+1))
  in
    helper 0;;



(* make the rank 1 and root itself *)
let singleton (tree: uf_t) (node:int) = 
  let size = Array.length tree in
    if node > size then 
      (let tree = arr_cpy tree (node+1) in
         tree.(node) <- (1,node)) else
      tree.(node) <- (1,node);;



let make_full () :uf_t = 
  Array.init 500 make_full_func;;


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

let find_components (tree:uf_t) (node: int) : (int list) =
  let root = find_nocompress tree node in
  let rec compress tree node root (ans:int list) counter =
    match tree.(node) with
      |rank, parent -> if parent == node then 
            (node::ans) else
            (tree.(node) <- (rank,root);
             compress tree parent root (node::ans) (counter+1))
  in
    compress tree node root ([]) 0;;

let pre_components (tree:uf_t) = 
  let length = Array.length tree in
  let rec comp_helper index (tree:uf_t) (ans: (int list) list) =
    if index >= length then ans else
      comp_helper (index + 1) (tree) ((find_components tree index)::ans)
  in
    comp_helper 0 tree ([[]]);;

let sort_fun1 (l1: int list) (l2: int list) : int=
  match l1, l2 with
    | (h1::t1), (h2::t2) -> if h1 > h2 then 1 else (-1)
    | _,_ -> raise NotValid;;

let components (tree:uf_t) = 
  let bulk_list = pre_components tree in
  let sorted_bulk = List.sort (sort_fun1) bulk_list in
    sorted_bulk;;


