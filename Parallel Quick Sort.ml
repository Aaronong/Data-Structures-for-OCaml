(* Your name: *)

(* This homework is out of 10 marks.  
   It is due Monday, April 11th, at 11:59pm.
*)

(* Part 1 (3 marks): please write an implementation for heaps
   satisfying the "heap.mli" interface.  Put your code in
   a file "heap.ml".  We'll need heaps next week for compression.

   Time bounds: insert & remove_min should take O(log n) time.
   Heap_of_array can take up to O(n * log n) time, although if you
   use binary heaps you can do it in O(n) time.
*)

(* size * tail * func * array *)
exception Empty;;
exception Full;;
open Printf;;

type 'a heap = int * (int ref) * ('a -> 'a -> bool) * 'a array;;


let heap_of_array (s: int) (func: 'a -> 'a -> bool) (arr: 'a array) =
  if s <= Array.length arr then raise Full else
    begin
      Array.sort (fun x y -> if (func x y) then -1 else 1) arr;
      (((s), ref(Array.length arr), func, (Array.init (1 + s) (fun i -> if i == 0 then arr.(0) else (if i <= (Array.length arr) then arr.(i - 1) else arr.(0))))): 'a heap) 
    end;;

let remove_min (heap: 'a heap) =
  match heap with
    |(size,tail,func, arr) -> if !tail == 0 then
          raise Empty else
          let min = Array.get arr 1 in
          let last = Array.get arr (!tail) in
            Array.set arr 1 last;
            tail := !tail - 1;

            let rec sink count =
              if (count*2 + 1) <= !tail then
                begin
                  let parent = Array.get arr count in
                  let child1 = Array.get arr (count*2) in
                  let child2 = Array.get arr (count*2 +1) in
                    if func child1 child2 then
                      (if func child1 parent then
                         (Array.set arr (count*2) parent;
                          Array.set arr count child1;
                          sink(count*2))) else
                      (if func child2 parent then
                         (Array.set arr (count*2 + 1) parent;
                          Array.set arr count child2;
                          sink(count*2+1)))
                end
              else if count*2 <= !tail then
                begin
                  let parent = Array.get arr count in
                  let child = Array.get arr (count*2) in
                    (if func child parent then
                       (Array.set arr (count*2) parent;
                        Array.set arr count child))
                end

            in
              sink 1;
              min;;


let insert (heap: 'a heap) (a: 'a) = 
  match heap with
    |(size, tail, func, arr) -> if size == !tail then
          raise Full else
          (Array.set arr (!tail+1) a;

           let rec swim count =
             if count != 1 then
               begin
                 let child = Array.get arr count in
                 let parent = Array.get arr (count/2) in
                   if func child parent then
                     begin
                       Array.set arr count parent;
                       Array.set arr (count/2) child;
                       swim(count/2)
                     end
               end
           in
             swim (!tail + 1);
             tail := !tail + 1
          );;


(* Part 2 (1 mark): Please write some basic tests for your code 
   (e.g. write a sorting function) so that you are confident that 
   things work.
*)

exception Different;;


let min x y =
  if x < y then true else false;;

let randomizedTest size =
  Random.self_init();
  let arr = Array.init (size/2) (fun x -> Random.int 1000000) in
  let arr2 = Array.init size (fun i -> if i < (Array.length arr) then arr.(i) else 0)  in
  let heap = heap_of_array size min arr in
  let rec inserts count =
    if count < (size) then
      (let x = Random.int 1000000 in
         insert heap x;
         Array.set arr2 count x;
         inserts (count+1))
  in
    inserts (size/2);

    Array.sort (fun x y -> if (min x y) then -1 else 1) arr2;
    let rec deletes count =

      if count == (size) then true else
        begin

          let x = remove_min heap in
          let y = arr2.(count) in
            if x == y then 
              deletes (count + 1) else
              raise Different
        end

    in
      deletes 0
;;


randomizedTest 100000;;






(* Part 3 (5 marks): please write a parallel version of quicksort
   as discussed in class.  As before, you may assume the following
   parallel "library":
*)

(* Takes time O(max {|f x|,|g x|}) *)
let plet (f, x) (g, y) = (f x, g y);;  

(* Takes time proportional to the longest single operation on 
   an element of the array, i.e. if all of them are O(1) then
   the whole operation is O(1) *)
let pmap = Array.map;; 
let pmapi = Array.mapi;;
let pinit = Array.init;;
let piteri = Array.iteri;;

(* I'll also give you code for reduce and prefix-sum; both take 
   O(log n) time when given on O(1) associative function. *)
let preduce f a =
  let rec reduce' (i, j) =
    if i + 1 = j then a.(i) else
      (* These two recursive calls can be done in parallel. *)
      let (r1, r2) = plet (reduce', (i, (i+j)/2)) (reduce', ((i + j) / 2, j)) in
        f r1 r2
  in
    reduce' (0, (Array.length a));;

(* Here I'm cheating with a sequential version.  But you can assume in your
   analysis that it works in the correct time bound, i.e. O(log n). *)
let pprefix_sum f a = 
  let a' = Array.init (Array.length a) (fun i -> a.(i)) in
    for i = 1 to Array.length a - 1 do
      a'.(i) <- f (a'.(i)) (a'.(i - 1))
    done;
    a';;


(* Your runtime should be O(log^2 n), given enough processors and reasonable
   luck for pivot selection.  For your pivot, you can choose any element;
   probably the first element will be convenient. *)

(* the binaryPosition function encodes in binary whether an array of elements (array a) at a given position is larger than or smaller than the pivot. small and big are lists storing the binary positions of elements smaller than or larger than the pivot. start and stop denote the position of the array a that are of concern to encode.*)
let binaryPosition a small big start stop =
  let rec reduce' (i, j) =
    if i + 1 = j then 
      begin
        let pivot = a.(start) in
          if a.(i) < pivot then
            Array.set small (i-start) 1 else
            Array.set big (i-start) 1
      end
    else
      (* These two recursive calls can be done in parallel. *)
      let _ = plet (reduce', (i, (i+j)/2)) (reduce', ((i + j) / 2, j)) in () 
  in
    reduce' (start, (stop));;

(* the pcopy function copies the unpivoted elements in array a to its pivoted form in array a'. small and big are prefix sums used to assist in the process. start and stop denote the position of the array a that are of concern to encode. *)
let pcopy a a' small big start stop=

  let split = small.( (Array.length small) - 1) in
  let rec reduce' (i, j) =
    if i + 1 = j then 
      begin

        if i ==start then
          (if small.(0) == 0 then
             Array.set a' split a.(i) else
             Array.set a' 0 a.(i))

        else
          begin

            if small.(i-start) > small.(i-1-start) then
              Array.set a' (small.(i-start) - 1) a.(i) else
              Array.set a' (big.(i-start) - 1 + split) a.(i)

          end

      end
    else
      (* These two recursive calls can be done in parallel. *)
      let _ = plet (reduce', (i, (i+j)/2)) (reduce', ((i + j) / 2, j)) in () 
  in
    reduce' (start, (stop));;

(* pquicksort_helper takes in an array, a start index and stop index. It pivots all elements in the array between the start and stop index and returns two start and stop indexes denoting the start and stop indexes of the smaller than portion and the larger than portion. *)

let pquicksort_helper arr start stop=

  let smallArr = pinit (stop - start) (fun i -> 0) in
  let bigArr = pinit (stop - start) (fun i -> 0) in
    binaryPosition arr smallArr bigArr start stop;

    let smallSum = pprefix_sum (+) smallArr in
    let bigSum = pprefix_sum (+) bigArr in
    let a' = pinit (stop - start) (fun i -> 0) in

      pcopy arr a' smallSum bigSum start stop;

      piteri (fun i x -> Array.set arr (start + i) a'.(i)) a';
      (start, start + smallSum.(stop - start - 1)), (start + 1 + smallSum.(stop - start - 1), stop)
;;


(* like plet but takes in more parameters. *)
let plet' (f, x1, x2) (g, y1, y2) = (f x1 x2, g y1 y2);; 


(* Main function for pquicksort. Takes in an array and sorts it using the pquicksort helper function. It controls the parameters to feed to pquicksort_helper. *)
let pquicksort arr =
  let rec helper start stop =
    (*printf "start = %d stop = %d \n" start stop;
      print_newline (); *)
    let (start1, stop1), (start2, stop2) = pquicksort_helper arr start stop in
      if ( (start1  < stop1) && (start2  < stop2)) then 
        let _ = plet' (helper, start1, stop1) (helper, start2,stop2) in ()
      else if start1  < stop1 then
        let _ = helper start1 stop1 in ()
      else if start2  < stop2 then
        let _ = helper start2 stop2 in ()

  in
    helper 0 (Array.length arr)
;;



(* Please comment your code extensively so that I can follow it. *)

(* Part 4 (1 mark): please write some tests to make sure that 
   everything is working fine. *)

let arr = [|7;13;18;2;17;1;14;20;6;10;15;9;3;16;19;4;11;12;5;8|];;
pquicksort arr;;
arr;;

let arr2 = [|2;1;1;3;7;4;3|];;
pquicksort arr2;;
arr2;;

let arr3 = [|7;13;18;2;17;1;14;20;6;23;1;2;4;7;66;453;342;234;23;6;57;4;53;45;245;23;423;423;4;55;6;7;8;9;87;6;5;4;3;5;7;90;98;7;6;10;15;9;3;16;19;4;11;12;5;8|];;
pquicksort arr3;;
arr3;;

let arr4 = [|7;13;18;2;-17;1;-14;20;-6;10;-15;9;3;-16;19;-4;-11;12;5;8|];;
pquicksort arr4;;
arr4;;
