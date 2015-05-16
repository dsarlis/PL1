let main = 
  Random.init (int_of_string Sys.argv.(1));
  let n = int_of_string Sys.argv.(2) in
  for i = 1 to n do
    Printf.printf "%d %d " (Random.int 10000 + 1) ((Random.int 300 / 10 + 1) * 10);
  done;
  Printf.printf "0 0";
