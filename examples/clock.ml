let _ =
  Frp.Stream.iter (Frp.Stream.ticks 100.) ~f:(fun t ->
    print_endline ("The time is: " ^ Time.to_string t)
  );

