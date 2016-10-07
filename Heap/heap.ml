type 'a heap = {
  comp : 'a -> 'a -> bool;
  arr : 'a array;
  size : int ref
};;

let swap arr i j =
  let t = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- t;;

let isroot n = n = 0;;
let left n = ((n + 1) * 2) - 1;;
let right n = (n + 1) * 2;;
let parent n = ((n + 1) / 2) - 1;;

let rec swim comp arr i =
  if isroot i then () else
  if comp arr.(i) arr.(parent i) then 
    begin
      swap arr i (parent i);
      swim comp arr (parent i)
    end

let rec sink comp size arr i = 
  if left i >= size then () else
  if right i >= size then
    begin
      if comp arr.(left i) arr.(i) then swap arr (left i) i
    end else 
    begin
      if comp arr.(left i) arr.(i) ||
         comp arr.(right i) arr.(i) then
        begin
          if comp arr.(left i) arr.(right i) then 
            begin
              swap arr i (left i);
              sink comp size arr (left i)
            end else 
            begin
              swap arr i (right i);
              sink comp size arr (right i)
            end
        end
    end

exception Full;;
let insert {comp; arr; size} item =
  let sz = ! size in
    if sz = Array.length arr then raise Full else
      begin
        arr.(sz) <- item;
        swim comp arr sz;
        size := sz + 1
      end;;

exception Empty;;
let remove_min {comp; arr; size} =
  let sz = !size - 1 in
    if sz = -1 then raise Empty else
      begin
        size := sz;
        swap arr 0 sz;
        sink comp sz arr 0;
        arr.(sz)
      end;;

let heap_of_array size comp arr =
  if size > Array.length arr then raise Full else 
    let h = { comp = comp;
              arr = Array.copy arr;
              size = ref size } in
      for i = (size / 2) + 1 downto 0 do
        sink comp size h.arr i
      done;
      h;;
