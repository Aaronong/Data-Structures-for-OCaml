(* This assignment is out of 10 marks, as follows:

   7 marks: implement the remove function, probably as explained in
   http://matt.might.net/articles/red-black-delete/

   3 marks: develop a solid testing mechanism for your code (including 
   a good "check" function)

   Good testing frameworks:
   a) are substantial in size (e.g. largeUF had 2mm links...)
   b) attempt to be exhaustive
   c) carefully cover corner cases
   d) examine potential performance bottlenecks and provide benchmark results

   Please note that since the testing framework is 3 marks, I expect you to
   do a careful job.
*)

(* Please put your name here: Aaron Ong*)
#load "unix.cma"
open Unix
open Printf

module type FPM =
sig
  type ('a, 'b) t
  val empty : ('a, 'b) t

  (* if a |-> b' is already in the map then modify it *)
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

  (* if a |-> b is not in the map then don't delete anything *)
  val remove : ('a, 'b) t -> 'a -> ('a, 'b) t

  exception Notfound
  (* if a |-> b is not in the map then raise Notfound *)

  exception RedOnRedError
  (* if parent and a child are both red then raise error*)

  exception BlackHeightError
  (* if black height of tree is not balanced then raise error*)

  exception UnexpectedPatternError
  exception UnexpectedPatternError2
  exception UnexpectedPatternError3
  exception UnexpectedPatternError4
  exception UnexpectedPatternError5

  exception DBlkUBlkError
  val lookup : ('a, 'b) t -> 'a -> 'b
  val lookup_option : ('a, 'b) t -> 'a -> 'b option

  (* check should make sure that the red-black invariants are being followed:
     1) black-height of each leaf is the same, 
     2) no two red nodes in a row, and
     3) no double black or unblack nodes

     You may find it useful to also check other properties (e.g., that
     items are sorted left-to-right).
  *)
  val check : ('a, 'b) t -> bool
end

module Fpm : FPM = 
struct
  (* Your module here *)
  type color = Red | Blk | DBlk | UBlk
  type ('a, 'b) tree = Lf | DLf
                     | Br of color * ('a,'b) tree * 'a * 'b * ('a,'b) tree

  type ('a, 'b) t = ('a, 'b) tree

  let empty = Lf

  exception Notfound
  exception BlackHeightError
  exception RedOnRedError
  exception DBlkUBlkError
  exception UnexpectedPatternError
  exception UnexpectedPatternError2
  exception UnexpectedPatternError3
  exception UnexpectedPatternError4
  exception UnexpectedPatternError5
  let rec lookup tr k =
    match tr with
      | Lf -> raise Notfound
      | Br (_, lft, k', v', rgt) ->
          if k = k' then v'
          else if k < k' then lookup lft k
          else lookup rgt k
      |_ -> raise UnexpectedPatternError

  let rec lookup_option tr k =
    match tr with
      | Lf -> None
      | Br (_, lft, k', v', rgt) ->
          if k = k' then Some v'
          else if k < k' then lookup_option lft k
          else lookup_option rgt k
      |_ -> raise UnexpectedPatternError

  let balance t = 
    match t with
      | (Blk, Br (Red, Br(Red, alpha, kx, vx, beta), ky, vy, gamma) ,kz, vz, delta)
      | (Blk, Br (Red, alpha, kx, vx, Br (Red, beta, ky, vy, gamma)), kz, vz, delta)
      | (Blk, alpha, kx, vx, Br (Red, beta, ky, vy, Br (Red, gamma, kz, vz, delta)))
      | (Blk, alpha, kx, vx, Br (Red, Br (Red, beta, ky, vy, gamma), kz, vz, delta))
        -> Br (Red, Br (Blk, alpha, kx, vx, beta), ky, vy, Br (Blk, gamma, kz, vz, delta))
      | (a,b,c,d,e) -> Br (a,b,c,d,e)

  let rec insert_helper tr k v =
    match tr with
      | Lf -> Br (Red, Lf, k, v, Lf)
      | Br (clr, lft, k', v', rgt) ->
          if k = k' then Br (clr, lft, k, v, rgt) else
          if k < k' then balance (clr, insert_helper lft k v, k', v', rgt)
          else balance (clr, lft, k', v', insert_helper rgt k v)
      |_ -> raise UnexpectedPatternError

  let insert tr k v =
    match insert_helper tr k v with
      | Br (_, lft, k, v, rgt) -> Br (Blk, lft, k, v, rgt)
      |Lf -> Lf  
      |_ -> raise UnexpectedPatternError

  let rebalance t = 
    match t with
      | (DBlk, Br (Red, Br(Red, alpha, kx, vx, beta), ky, vy, gamma) ,kz, vz, delta)
      | (DBlk, Br (Red, alpha, kx, vx, Br (Red, beta, ky, vy, gamma)), kz, vz, delta)
      | (DBlk, alpha, kx, vx, Br (Red, beta, ky, vy, Br (Red, gamma, kz, vz, delta)))
      | (DBlk, alpha, kx, vx, Br (Red, Br (Red, beta, ky, vy, gamma), kz, vz, delta))
        -> Br (Blk, Br (Blk, alpha, kx, vx, beta), ky, vy, Br (Blk, gamma, kz, vz, delta))
      |(DBlk, Br(UBlk, Br(Blk, a, kw, vw, b), kx, vx, Br(Blk, c, ky, vy, d)), kz,vz, e)
        -> Br (Blk, balance(Blk, Br(Red, a, kw, vw, b), kx, vx, c), ky, vy, Br (Blk, d, kz,vz, e))
      | (DBlk, a, kw, vw, Br(UBlk, Br(Blk, b, kx, vx, c), ky, vy, Br(Blk, d, kz, vz, e)))
        -> Br (Blk, Br(Blk, a, kw, vw, b), kx, vx, balance(Blk, c, ky, vy, Br(Red, d, kz, vz, e)))
      | (a,b,c,d,e) -> Br (a,b,c,d,e)

  let delBalance t = 
    match t with
      |(Blk, Br(Blk, a, kx, vx, b), ky, vy, Br(DBlk, c, kz, vz, d)) -> 
          rebalance(DBlk, Br(Red, a, kx, vx, b), ky, vy, Br(Blk, c, kz, vz, d))
      |(Blk, Br(DBlk, a, kx, vx, b), ky, vy, Br(Blk, c, kz, vz, d)) -> 
          rebalance(DBlk, Br(Blk, a, kx, vx, b), ky, vy, Br(Red, c, kz, vz, d))
      |(Red, Br(Blk, a, kx, vx, b), ky, vy, Br(DBlk, c, kz, vz, d)) -> 
          balance (Blk, Br(Red, a, kx, vx, b), ky, vy, Br(Blk, c, kz, vz, d))
      |(Red, Br(DBlk, a, kx, vx, b), ky, vy, Br(Blk, c, kz, vz, d)) -> 
          balance (Blk, Br(Blk, a, kx, vx, b), ky, vy, Br(Red, c, kz, vz, d))
      |(Blk, Br(Red, a, kx, vx, b), ky, vy, Br(DBlk, c, kz, vz, d)) -> 
          rebalance(DBlk, Br(UBlk, a, kx, vx, b), ky, vy, Br(Blk, c, kz, vz, d))
      |(Blk, Br(DBlk, a, kx, vx, b), ky, vy, Br(Red, c, kz, vz, d)) -> 
          rebalance(DBlk, Br(Blk, a, kx, vx, b), ky, vy, Br(UBlk, c, kz, vz, d))
      |(Blk, Br(Blk, a, kx, vx, b), ky, vy, DLf) -> 
          rebalance(DBlk, Br(Red, a, kx, vx, b), ky, vy, Lf)
      |(Blk, DLf, ky, vy, Br(Blk, c, kz, vz, d)) -> 
          rebalance(DBlk, Lf, ky, vy, Br(Red, c, kz, vz, d))
      |(Red, Br(Blk, a, kx, vx, b), ky, vy, DLf) -> 
          balance (Blk, Br(Red, a, kx, vx, b), ky, vy, Lf)
      |(Red, DLf, ky, vy, Br(Blk, c, kz, vz, d)) -> 
          balance (Blk, Lf, ky, vy, Br(Red, c, kz, vz, d))
      |(Blk, Br(Red, a, kx, vx, b), ky, vy, DLf) -> 
          rebalance(DBlk, Br(UBlk, a, kx, vx, b), ky, vy, Lf)
      |(Blk, DLf, ky, vy, Br(Red, c, kz, vz, d)) -> 
          rebalance(DBlk, Lf, ky, vy, Br(UBlk, c, kz, vz, d))
      |(a,b,c,d,e) -> Br(a,b,c,d,e)


  let rec find_rightmost tr =
    match tr with
      |Br (_, _, _, _, Lf) -> tr
      |Br (_, _, _, _, rgt) -> find_rightmost rgt
      |_ -> raise UnexpectedPatternError


  let rec remove_helper tr k =
    match tr with
      |Lf -> raise Notfound
      |Br (clr, lft, k', v', rgt) ->
          if k = k' then replace_parent tr else
          if k < k' then delBalance (clr, remove_helper lft k, k', v', rgt) else 
            delBalance (clr, lft, k', v', remove_helper rgt k)
      |_ -> raise UnexpectedPatternError

  and

    replace_parent tr = 
    match tr with
      |Br (Red, Lf, _, _, Lf) -> Lf
      |Br (Blk, Lf, _, _, Lf) -> DLf
      |Br (Blk, Br(Red, lft, k'', v'', rgt), k', v', Lf) ->
          Br (Blk, lft, k'', v'', rgt)
      |Br (Blk, Lf, k', v', Br(Red, lft, k'', v'', rgt)) ->
          Br (Blk, lft, k'', v'', rgt)
      |Br (clr, lft, k, v, rgt) -> 
          (let predecessor = find_rightmost lft in
             match predecessor with
               |Br (_, _, k', v', _) ->
                   delBalance (clr, (remove_helper lft k'), k', v', rgt)
               |_ -> raise UnexpectedPatternError)
      |_ -> raise UnexpectedPatternError




  (* You need to change these two functions *)
  let remove tr k =
    match remove_helper tr k with
      |Br (clr, lft, k, v, rgt) -> Br(Blk, lft, k, v, rgt)
      |Lf -> Lf
      |DLf -> Lf


  (* This assignment is out of 10 marks, as follows:

     7 marks: implement the remove function, probably as explained in
     http://matt.might.net/articles/red-black-delete/

     3 marks: develop a solid testing mechanism for your code (including 
     a good "check" function)

     Good testing frameworks:
     a) are substantial in size (e.g. largeUF had 2mm links...)
     b) attempt to be exhaustive
     c) carefully cover corner cases
     d) examine potential performance bottlenecks and provide benchmark results

     Please note that since the testing framework is 3 marks, I expect you to
     do a careful job.

     exception RedOnRedError
     (* if parent and a child are both red then raise error*)

     exception BlackHeightError
     (* if black height of tree is not balanced then raise error*)

     exception MultipleKeyError

     exception DBlkUBlkError
  *)

  let blackHeight lft rgt =
    if lft = rgt then lft else raise BlackHeightError


  let check tr =
    let rec check_helper tr = 
      match tr with
        |DLf
        |Br(DBlk, _,_,_,_)
        |Br(UBlk, _,_,_,_)
          -> raise DBlkUBlkError
        |Br (Red, Br(Red,_,_,_,_),_,_,_)
        |Br (Red,_,_,_, Br(Red,_,_,_,_))
          -> raise RedOnRedError
        |Lf -> 1
        |Br (Red,lft,_,_,rgt) -> blackHeight 
                                   (check_helper lft) (check_helper rgt)
        |Br (Blk,lft,_,_,rgt) -> (blackHeight 
                                    (check_helper lft) (check_helper rgt))+1
    in 
      match check_helper tr with
        (*|DBlkUBlkError
          |RedOnRedError
          |BlackHeightError
          -> false*)
        |_ -> true

end

(* Here is a little testing code to get you started... *)
open Fpm;;

let m1 = insert empty 5 10;;
let m2 = insert m1 6 12;;
let m3 = insert m2 7 15;;
let m4 = insert m3 8 16;;
let m5 = insert m4 9 19;;
let m6 = remove m5 6;;
let m7 = insert m6 22 23;;
let m8 = remove m7 8;;
let m9 = remove m8 9;;

(* These should all be true... *)
check m1;;
check m2;;
check m3;;
check m4;;
check m5;;
check m6;;
check m7;;
check m8;;
check m9;;

(* Try a couple of lookups... *)
lookup m3 6;;

lookup m8 9;;

(* Timing analysis *)
let time f x =
  let s = Unix.gettimeofday () in
  let a = f x in
  let e = Unix.gettimeofday () in
    (a, e -. s);;

let sequentialInsert tr = 
  let rec singleinsert tr counter =
    if counter > 1000000 then tr else
      singleinsert (insert tr counter (counter+10)) (counter+1)
  in
    singleinsert tr 0;;

let sequentialDelete tr = 
  let rec singledelete tr counter = 
    if counter > 1000000 then tr else
      singledelete (remove tr counter) (counter+1)
  in
    singledelete tr 500000;;


let sequentialTest () = 
  let (tr, inserttime) = time sequentialInsert empty in
  let checkInsert = check tr in
  let (tr, deletetime) = time sequentialDelete tr in
  let checkDelete = check tr in
    printf "1000000 sequential inserts took %.2f seconds.\n" inserttime;
    printf "Check function returned %B for sequentially inserted tree\n" checkInsert;
    printf "500000 sequential deletes took %.2f seconds\n" deletetime;
    printf "Check function returned %B for sequentially deleted tree\n" checkDelete;
    print_newline();;

let randomizedTest () =
  Random.self_init ();
  let rec singletest tr counter =
    if counter > 1000000 then tr else
      let rand = Random.int 100000 in
        match lookup_option tr rand with
          |None -> singletest (insert tr rand (rand+10)) (counter+1)
          |_ -> singletest (remove tr rand) (counter+1)
  in
    singletest empty 0;;

let testingFramework () =
  sequentialTest ();
  let (tree1, test1time) = time randomizedTest () in
  let check1 = check tree1 in
    printf "First randomized test of 1000000 insertions/deletions took %.2f seconds and check function returned %B\n" test1time check1; print_newline ();
    let (tree2, test2time) = time randomizedTest () in
    let check2 = check tree2 in
      printf "Second randomized test of 1000000 insertions/deletions took %.2f seconds and check function returned %B\n" test2time check2; print_newline ();
      let (tree3, test3time) = time randomizedTest () in
      let check3 = check tree3 in
        printf "Third randomized test of 1000000 insertions/deletions took %.2f seconds and check function returned %B\n" test3time check3; print_newline ();
        let (tree4, test4time) = time randomizedTest () in
        let check4 = check tree4 in
          printf "Fourth randomized test of 1000000 insertions/deletions took %.2f seconds and check function returned %B\n" test4time check4; print_newline ();
          let (tree5, test5time) = time randomizedTest () in
          let check5 = check tree5 in
            printf "Fifth randomized test of 1000000 insertions/deletions took %.2f seconds and check function returned %B\n" test5time check5; print_newline ();; 

testingFramework ();;


(*
Benchmark results

1000000 sequential inserts took 16.46 seconds.
Check function returned true for sequentially inserted tree
500000 sequential deletes took 5.94 seconds
Check function returned true for sequentially deleted tree

First randomized test of 1000000 insertions/deletions took 17.92 seconds and check function returned true

Second randomized test of 1000000 insertions/deletions took 18.03 seconds and check function returned true

Third randomized test of 1000000 insertions/deletions took 18.10 seconds and check function returned true

Fourth randomized test of 1000000 insertions/deletions took 18.19 seconds and check function returned true

Fifth randomized test of 1000000 insertions/deletions took 18.55 seconds and check function returned true

*)
