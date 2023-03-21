let kmp_search text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let pi = Array.make m 0 in
  let rec compute_prefix i j =
    if j < m then
      if pattern.[i] = pattern.[j] then begin
        pi.(j) <- i + 1;
        compute_prefix (i + 1) (j + 1)
      end else if i > 0 then
        compute_prefix pi.(i - 1) j
      else
        compute_prefix i (j + 1)
  in
  let rec search i j =
    if j = m then
      i - m
    else if i = n then
      -1
    else if text.[i] = pattern.[j] then
      search (i + 1) (j + 1)
    else if j > 0 then
      search i pi.(j - 1)
    else
      search (i + 1) 0
  in
  compute_prefix 0 1;
  search 0 0
;;

let test_string = "My test String";;
let my_pattern_string = "est Str";;
let my_another_pattern_string = "My te";;
let index_at_which_pattern_found = kmp_search test_string my_pattern_string;;
print_int index_at_which_pattern_found;;

print_string "\n";;
let index_at_which_pattern_found = kmp_search test_string my_another_pattern_string;;
print_int index_at_which_pattern_found;;
