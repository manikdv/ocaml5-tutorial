module T = Domainslib.Task

let (num_domains,x,y,z) =
  try
    let num_domains = int_of_string Sys.argv.(1) in
    let x = int_of_string Sys.argv.(2) in
    let y = int_of_string Sys.argv.(3) in
    let z = int_of_string Sys.argv.(4) in
    (num_domains,x,y,z)
  with _ -> (1,18,12,6)

let rec tak x y z =
  if x > y then
    tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let rec tak_par pool x y z =
  if x < 20 && y < 20 then
    tak x y z
  else if x > y then
    let p1 = T.async pool (fun _ -> tak_par pool (x-1) y z) in
    let p2 = T.async pool (fun _ -> tak_par pool (y-1) z x) in
    let p3 = T.async pool (fun _ -> tak_par pool (z-1) x y) in
    tak_par pool (T.await pool p1) (T.await pool p2) (T.await pool p3)
  else
    z
    
let main () =
  let p = T.setup_pool ~num_domains:(num_domains - 1) () in
  let r = T.run p (fun _ -> tak_par p x y z) in
  T.teardown_pool p;
  Printf.printf "tak %d %d %d = %d\n" x y z r

let _ = main ()
