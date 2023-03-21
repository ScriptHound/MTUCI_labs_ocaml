open Graphics;;

let rec draw_sierpinski x y size n =
  if n = 0 then
    fill_poly [|(x, y); (x + size, y); (x + size/2, y + size)|]
  else
    let size' = size / 2 in
    draw_sierpinski x y size' (n-1);
    draw_sierpinski (x + size') y size' (n-1);
    draw_sierpinski (x + size'/2) (y + size') size' (n-1)

let sierpinski () =
  open_graph " 1000x1000";
  set_window_title "Sierpinski Triangle";
  set_color black;
  draw_sierpinski 100 100 700 7;
  ignore (read_key ());
  close_graph ()

