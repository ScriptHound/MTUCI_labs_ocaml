open Sierpinski_triangle;;


let shell_sort arr = 
  let len = Array.length arr in 
  let gap = ref (len / 2) in
  while !gap > 0 do
    for idx = !gap to len - 1 do
      let temp = arr.(idx) in
      let subidx = ref idx in
      while !subidx >= !gap && arr.(!subidx - !gap) > temp do
        arr.(!subidx) <- arr.(!subidx - !gap);
        subidx := !subidx - !gap;
      done;
      arr.(!subidx) <- temp;
    done;
    gap := !gap / 2;
  done;
;;

Random.self_init ();;
let my_array = Array.init 10 (fun _ -> Random.int 100 + 1);;
shell_sort my_array;;
print_string "Array sorted with a Shellsort \n";;
print_string (String.concat " " (Array.to_list (Array.map string_of_int my_array)));;

let rec quicksort arr = 
  match arr with
  | [] -> []
  | pivot :: tail ->
    let left, right = List.partition (fun x -> x < pivot) tail in
      quicksort left @ [pivot] @ quicksort right;;

(* due to Shellsort is an in-place algorhitm new array is initialized*)
print_string "\nArray sorted with quicksort\n";;

let my_array = List.init 10 (fun _ -> Random.int 100 + 1);;
let sorted_array = quicksort my_array;;
print_string (String.concat " " (List.map string_of_int sorted_array));;
print_string "\n";;

let () = (sierpinski ());;
