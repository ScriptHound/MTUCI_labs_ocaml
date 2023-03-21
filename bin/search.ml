let rec interpolation_search arr x = 
  let n = Array.length arr in
  let low = 0 in
  let high = n - 1 in
  let rec loop low high = 
    if low <= high && x >= arr.(low) && x <= arr.(high) then
       let pos = low + ((x - arr.(low)) * (high - low)) / (arr.(high) - arr.(low)) in
       if arr.(pos) = x then
        Some pos
       else if arr.(pos) < x then
        loop (pos + 1) high
       else
        loop low (pos - 1)
    else 
      None
  in
  loop low high;;


Random.self_init ();;
let my_array = Array.init 10 (fun _ -> Random.int 100 + 1);;
Array.sort compare my_array;;
let int_to_look_for = Random.int 100 + 1;;
print_string (String.concat " " ["Int to look for"; (Int.to_string int_to_look_for); "\n"]);;
print_string (String.concat " " (Array.to_list (Array.map Int.to_string my_array)));;
print_string "\n";;
let search_result = interpolation_search my_array int_to_look_for;;

let is_found =
  match search_result with 
  | Some search_result -> String.cat "Found at " (Int.to_string search_result)
  | None -> "Not found";;

print_string (String.cat is_found "\n");;
