open Core

module Subscription : sig
  type t

  val empty : t

  val cancel : t -> unit

  val make : (unit -> unit) -> t

  val merge : t -> t -> t

  val concat : t array -> t
end = struct
  type t = unit -> unit

  let empty = fun () -> ()

  let cancel t = t ()

  let merge t1 t2 = fun () -> cancel t1; cancel t2

  let concat ts = fun () -> Array.iter ~f:cancel ts

  let make x = x
end

module Stream = struct
  let notify listeners x =
    Inttbl.iter listeners ~f:(fun ~key:_ ~data -> data x)

  let notify_all ls x = Array.iter ls ~f:(fun l -> notify l x)

  module Prim = struct
    type 'a t =
      { start        : unit -> (unit -> unit) (* returns a new stop function *)
      (* (t.start ()) () should be observationally equivalent to (fun () -> ()) () *)
      ; mutable stop : unit -> unit
      ; on_listeners      : ('a -> unit) Inttbl.t
      ; off_listeners     : ('a -> unit) Inttbl.t
      ; passive_listeners : ('a -> unit) Inttbl.t
      (* A passive listener will be called if and only if on_listeners is
       * nonempty *)
      ; mutable uid       : int
      }

    let create ?(start=(fun () () -> ())) () =
      { start
      ; stop              = (fun () -> ())
      ; on_listeners      = Inttbl.create ()
      ; off_listeners     = Inttbl.create ()
      ; passive_listeners = Inttbl.create ()
      ; uid               = 0
      }

    let stop t = t.stop ()

    let start t =
      stop t; (* TODO: This might be an error *)
      let stop' = t.start () in
      t.stop <- stop'
    ;;

    let on_interval ms ~f =
      let on_listeners  = Inttbl.create () in
      let start () =
        let interval = set_interval ms ~f:(fun () ->
          notify on_listeners (f (Time.now ()))) in
        fun () -> clear_interval interval
      in
      { start; stop = (fun () -> ()); uid = 0; on_listeners
      ; off_listeners = Inttbl.create (); passive_listeners = Inttbl.create ()
      }

    let turn_on key t =
      match Inttbl.find t.off_listeners key with
      | None   -> failwith "Stream.Prim.turn_on: Listener was not off"
      | Some f ->
        begin
          Inttbl.add t.on_listeners ~key ~data:f;
          Inttbl.remove t.off_listeners key;
          if Inttbl.length t.on_listeners = 1 then start t;
        end

    let turn_off key t =
      match Inttbl.find t.on_listeners key with
      | None   -> failwith "Stream.Prim.turn_off: Listener was not on"
      | Some f ->
        begin
          Inttbl.add t.off_listeners ~key ~data:f;
          Inttbl.remove t.on_listeners key;
          if Inttbl.length t.on_listeners = 0 then stop t;
        end

    let add_off_listener t f =
      let key = t.uid in
      t.uid <- t.uid + 1;
      Inttbl.add t.off_listeners ~key ~data:f;
      key
    ;;

    let add_on_listener t f =
      let key = add_off_listener t f in
      turn_on key t;
      key

    let add_passive_listener t f =
      let key = t.uid in
      t.uid <- t.uid + 1;
      Inttbl.add t.passive_listeners ~key ~data:f;
      key
  end

  (* Almost every feature of the OCaml type system
   * (with the exception of polymorphic variants)
   * can be seen here *)
  type 'a derived =
    { mutable uid   : int
    (* TODO: Consider just having one Inttbl with a tag for whether
     * a listener is on or off *)
    ; on_listeners      : ('a -> unit) Inttbl.t
    ; off_listeners     : ('a -> unit) Inttbl.t
    ; passive_listeners : ('a -> unit) Inttbl.t
    ; parents           : (int * ext) array
    }
  and 'a t =
    | Prim of 'a Prim.t
    | Derived of 'a derived
  and ext = In : 'a t -> ext

  let rec turn_on_derived : 'a. int -> 'a derived -> unit = fun key t ->
    match Inttbl.find t.off_listeners key with
    | None -> failwith "Stream.turn_on_derived: Listener was not off";
    | Some f ->
      begin
        Inttbl.add t.on_listeners ~key ~data:f;
        Inttbl.remove t.off_listeners key;
        if Inttbl.length t.on_listeners = 1
        then Array.iter t.parents ~f:(fun (k, In p) -> turn_on k p)
      end
  and turn_on : 'a. int -> 'a t -> unit = fun key t -> match t with
    | Prim p -> Prim.turn_on key p
    | Derived d -> turn_on_derived key d
  ;;

  let rec turn_off_derived : 'a. int -> 'a derived -> unit = fun key t ->
    match Inttbl.find t.on_listeners key with
    | None -> failwith "Stream.turn_off_derived: Listener was not on"
    | Some f ->
      begin
        Inttbl.add t.off_listeners ~key ~data:f;
        Inttbl.remove t.on_listeners key;
        if Inttbl.length t.on_listeners = 0
        then Array.iter t.parents ~f:(fun (k, In p) -> turn_off k p);
      end
  and turn_off : 'a. int -> 'a t -> unit = fun key t -> match t with
    | Prim p -> Prim.turn_off key p
    | Derived d -> turn_off_derived key d

  let add_listener' tbl t f =
    let key = t.uid in
    t.uid <- t.uid + 1;
    Inttbl.add tbl ~key ~data:f;
    key

  let add_off_listener t f =
    match t with
    | Prim p -> Prim.add_off_listener p f
    | Derived d -> add_listener' d.off_listeners d f
  ;;

  let add_passive_listener t f =
    match t with
    | Prim p -> Prim.add_passive_listener p f
    | Derived d -> add_listener' d.passive_listeners d f

  let add_on_listener t f =
    let key = add_off_listener t f in
    turn_on key t;
    key
  ;;

  let never () =
    Derived 
    { uid = 0; parents = [||]
    ; on_listeners = Inttbl.create (); off_listeners = Inttbl.create ()
    ; passive_listeners = Inttbl.create ()
    }

  let create parent ~update =
    let on_listeners = Inttbl.create () in
    let passive_listeners = Inttbl.create () in
    let trigger x =
      notify on_listeners x;
      notify passive_listeners x
    in
    let key = add_off_listener parent (fun x -> update trigger x)
    in
    Derived
    { uid = 0
    ; parents = [| key, In parent |]
    ; on_listeners
    ; passive_listeners
    ; off_listeners = Inttbl.create ()
    }

  let map t ~f =
    let update trigger x = trigger (f x) in
    create t ~update
  ;;

  let iter t ~f =
    let key = add_on_listener t f in
    Subscription.make (fun () -> turn_off key t)

  let skip_duplicates ?(eq=(=)) t =
    let prev = ref None in
    let update trigger x =
      let is_new = match !prev with
        | None -> true
        | Some y -> not (eq x y)
      in
      if is_new
      then begin
        prev := Some x;
        trigger x
      end
    in
    create t ~update

  let filter t ~f =
    let update trigger x =
      if f x then trigger x
    in
    create t ~update

  (* Should trigger initial value *)
  let fold t ~init ~f =
    let last = ref init in
    let update trigger x =
      let y = f !last x in
      last := y;
      trigger y
    in
    create t ~update

  (* TODO: Consider switching to a new listener after [n] events if
   * this is too inefficient *)
  let drop t n =
    let seen = ref 0 in
    let update trigger x =
      if !seen >= n then trigger x else incr seen
    in
    create t ~update

  let tail t = drop t 1

  let zip_with t1 t2 ~f =
    let on_listeners = Inttbl.create () in
    let passive_listeners = Inttbl.create () in
    let on_value q q' g = fun x ->
      match Queue.dequeue q' with
      | None -> Queue.enqueue q x
      | Some y -> notify_all [|on_listeners; passive_listeners|] (g x y)
    in
    let q1, q2 = Queue.create (), Queue.create () in
    let key1 = add_off_listener t1 (on_value q1 q2 f) in
    let key2 = add_off_listener t2 (on_value q2 q1 (fun x y -> f y x)) in
    Derived
    { uid = 0
    ; on_listeners
    ; passive_listeners
    ; off_listeners = Inttbl.create ()
    ; parents = [| (key1, In t1); (key2, In t2) |]
    }

  let ap tf tx = zip_with tf tx ~f:(fun f x -> f x)

  let zip = zip_with ~f:(fun x y -> (x, y))

  let merge t1 t2 =
    let on_listeners = Inttbl.create () in
    let passive_listeners = Inttbl.create () in
    let update x = notify_all [|on_listeners; passive_listeners|] x in
    let key1 = add_off_listener t1 update in
    let key2 = add_off_listener t2 update in
    Derived
    { uid = 0
    ; on_listeners
    ; passive_listeners
    ; off_listeners = Inttbl.create ()
    ; parents = [| (key1, In t1); (key2, In t2) |]
    }

  let join t =
    let on_listeners = Inttbl.create () in
    let passive_listeners = Inttbl.create () in
    let ls = [|on_listeners; passive_listeners|] in
    let parents      = [||] in
    let update s =
      let key = add_off_listener s (notify_all ls)
      in Array.push (key, In s) parents 
    in
    let key = add_off_listener t update in
    Derived
    { uid = 0
    ; on_listeners
    ; passive_listeners
    ; off_listeners = Inttbl.create ()
    ; parents
    }
    
  let switch t =
    let on_listeners = Inttbl.create () in
    let passive_listeners = Inttbl.create () in
    let ls = [|on_listeners; passive_listeners|] in
    let prev = ref None in
    let parents = [||] in
    let update s =
      Option.iter !prev ~f:(fun (k, s') -> turn_off k s');
      let key = add_off_listener s (notify_all ls) 
      in
      prev := Some (key, s);
      Array.unsafe_set parents 1 (key, s)
    in
    let key = add_off_listener t update in
    Array.push (key, In t) parents;
    Derived
    { uid = 0
    ; on_listeners
    ; passive_listeners
    ; parents
    ; off_listeners = Inttbl.create ()
    }

  let bind t ~f = join (map t ~f)

  let delta t ~f =
    let last = ref None in
    let update trigger x =
      Option.iter !last ~f:(fun v -> trigger (f v x));
      last := Some x
    in
    create t ~update

  let ticks ms = Prim (Prim.on_interval ms ~f:(fun x -> x))

  let elapsed ms = 
    let t0 = Time.now () in
    Prim (Prim.on_interval ms ~f:(fun t -> Time.(t - t0)))

  let deltas ms = delta (ticks ms) (fun t1 t2 -> Time.(t2 - t1))

  let sequence ts =
    let on_listeners = Inttbl.create () in
    let passive_listeners = Inttbl.create () in
    let ls = [|on_listeners; passive_listeners|] in
    let buf = Array.init (Array.length ts) ~f:(fun _ -> Queue.create ()) in
    let parents = Array.mapi ts ~f:(fun i t ->
      let key = add_off_listener t (fun x ->
        Queue.enqueue buf.(i) x;
        if Array.for_all buf ~f:(fun q -> Option.is_some (Queue.peek q))
        then notify_all ls (Array.map buf ~f:Queue.dequeue_exn))
      in
      (key, In t))
    in
    Derived
    { uid = 0; parents; on_listeners; passive_listeners
    ; off_listeners = Inttbl.create ()
    }

  module Infix = struct
    let (>>=) t f = bind t ~f

    let (>>|) t f = map t ~f

    let (<$>) f t = map t ~f
    
    let (<*>) = ap
  end
end

module Behavior = struct
  type 'a t = 
    { s     : 'a Stream.t
    ; value : 'a ref
    }

  let activate {s; key; value} =
    Option.iter key ~f:(fun k -> Stream.turn_on k s)

  let deactivate {s; key; value} =
    Option.iter key ~f:(fun k -> Stream.turn_off k s)

  let set t x = t.value := x

  let peek t = !t.value

  let return init = { value = init ; s = Stream.never () }

  let skip_duplicates ?eq {value; s} =
    let s'     = Stream.skip_duplicates ?eq s in
    let value' = ref !value in
    let key'   = Stream.add_passive_listener s' (fun x -> value' := x) in
    { value = value' ; s = Stream.skip_duplicates ?eq s ; key = Some key' }

  let map {s; key; value} ~f =
    let value' = ref (f !value) in
    let s'    = Stream.map s ~f in
    let key'  = Stream.add_passive_listener s' (fun x -> value' := x) in
    { value = value'; s = s'; key = Some key' }
  ;;

  let zip_with t1 t2 ~f =
    let value = ref (f !(t1.value) !(t2.value)) in
    let s =
      let on_listeners = Inttbl.create () in
      let passive_listeners = Inttbl.create () in
      let key1 = Stream.add_passive_listener t1.s (fun x ->
        let z = f x !(t2.value) in
        value := z;
        Stream.notify_all [|on_listeners; passive_listeners|] z) in
      let key2 = Stream.add_passive_listener t2.s (fun y ->
        let z = f !(t1.value) y in
        value := z;
        Stream.notify_all [|on_listeners; passive_listeners|] z)
      in
      Stream.(Derived 
      { uid = 0; on_listeners; passive_listeners; off_listeners = Inttbl.create ()
      ; parents = [|(key1, In t1.s); (key2, In t2.s)|]
      })
    in
    { s; value}
  ;;

  let zip_many ts ~f =
    let t' = return (f (Array.map ~f:peek ts)) in
    Array.iter ts ~f:(fun t ->
      add_listener t (fun _ -> trigger t' (f (Array.map ~f:peek ts)))
    );
    t'
  ;;

  let ap tf tx = zip_with tf tx ~f:(fun f x -> f x)

  let zip = zip_with ~f:(fun x y -> (x, y))

  let join t =
    let t' = return t.value.value in
    add_listener t.value (fun x -> trigger t' x);
    add_listener t (fun s ->
      add_listener s (fun x -> trigger t' x)
    );
    t'
  ;;

  let sequence ts =
    let t = return (Array.map ts ~f:(fun t -> t.value)) in
    Array.iteri ts ~f:(fun i x ->
      add_listener x (fun v -> t.value.(i) <- v; notify_listeners t)
    );
    t
  ;;

  let changes {s; _} = s

  let changes t =
    let s = Stream.create () in
    Stream.trigger s t.value;
    add_listener t (Stream.trigger s);
    s
  ;;

  let bind t ~f = join (map t ~f)

  module Infix = struct
    let (>>=) t f = bind t ~f
    let (>>|) t f = map t ~f
    let (<$>) f t = map t ~f
    let (<*>)     = ap
  end
end

let when_ cond s =
  let s' = Stream.create () in
  ignore (Stream.iter s ~f:(fun x -> if cond.Behavior.value then Stream.trigger s' x));
  s'
;;

let scan s ~init ~f =
  let b = Behavior.return init in
  ignore (
    Stream.iter s ~f:(fun x ->
      Behavior.trigger b (f (Behavior.peek b) x)
    )
  );
  b
;;

let project b s         = Stream.map s ~f:(fun _ -> b.Behavior.value)
let project_with b s ~f = Stream.map s ~f:(fun x -> f b.Behavior.value x)

