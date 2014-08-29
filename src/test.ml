open Core

let switch_test =
  let control_stream =
    let t0 = Time.now () in
    Frp.Stream.(map (ticks 100.) ~f:(fun d ->
      let x = Time.Span.to_ms (Time.(d - t0)) in
      if x < 1000. then `A else 
      if x < 2000. then `B else `C)
    |> skip_duplicates)
  in
  Frp.Stream.(switch (
    map control_stream ~f:(fun lbl ->
      let s = match lbl with `A -> "A" | `B -> "B" | `C -> "C" in
      map (ticks 20.) ~f:(fun _ -> s))))

let () = Frp.Stream.iter switch_test ~f:print_endline |> ignore
