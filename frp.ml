open Core

(*
module Future = struct
  type 'a t = { f : 'b. ('a -> 'b) -> 'b }

  let return x = { f = fun k -> k x }

  type 'a stream = Cons ('a * 'a stream t)

  let clicks (elt : Jq.t) : 'a stream t =
    let f k = k (Cons ()
      Jq.on "click" elt (fun e ->
      )
end

*)

module Subscription : sig
  type t

  val cancel : t -> unit

  val mk : (unit -> unit) -> t
end = struct
  type t = unit -> unit

  let cancel t = t ()

  let mk x = x
end

module Stream = struct
  type 'a t =
    { listeners   : ('a -> unit) Inttbl.t
    ; mutable uid : int
    }

  let iter t ~f =
    let key = t.uid in
    t.uid <- key + 1;
    Inttbl.add t.listeners ~key ~data:f;
    Subscription.mk (fun () -> Inttbl.remove t.listeners key)
  ;;

  let trigger t x = Inttbl.iter ~f:(fun ~key ~data -> data x) t.listeners

  let create () = { uid = 0 ; listeners = Inttbl.create () }

  let map t ~f =
    let t' = create () in
    ignore (iter t ~f:(fun x -> trigger t' (f x)));
    t'
  ;;

  let filter t ~f =
    let t' = create () in
    ignore (iter t ~f:(fun x -> if f x then trigger t' x));
    t'
  ;;

  let fold t ~init ~f =
    let t' = create () in
    let last = ref init in
    trigger t' init;
    ignore (
      iter t ~f:(fun x ->
        let y = f !last x in
        last := y;
        trigger t' y
      )
    );
    t'
  ;;

  let drop t n =
    let seen = ref 0 in
    let t'   = create () in
    ignore (iter t ~f:(fun x -> if !seen >= n then trigger t' x else incr seen));
    t'
  ;;

  let tail t = drop t 1

  let zip_with t1 t2 ~f =
    let t               = create () in
    let on_value q q' g = fun x ->
      match Queue.dequeue q' with
      | None   -> Queue.enqueue q x
      | Some y -> trigger t (g x y)
    in

    let q1, q2 = Queue.create (), Queue.create () in
    ignore (iter t1 ~f:(on_value q1 q2 f));
    ignore (iter t2 ~f:(on_value q2 q1 (fun x y -> f y x)));
    t
  ;;

  let ap (tf : ('a -> 'b) t) (tx : 'a t) : 'b t = zip_with tf tx ~f:(fun f x -> f x)

  let zip = zip_with ~f:(fun x y -> (x, y))

  let merge t1 t2 =
    let t = create () in
    ignore (iter t1 ~f:(fun x -> trigger t x));
    ignore (iter t2 ~f:(fun x -> trigger t x));
    t
  ;;

  let join t =
    let t' = create () in
    ignore (
      iter t ~f:(fun s ->
        ignore (iter s ~f:(fun x -> trigger t' x))
      )
    );
    t'
  ;;

  let switch t =
    let t'   = create () in
    let last = ref None in
    ignore (
      iter t ~f:(fun s ->
        Option.iter !last ~f:(fun sub -> Subscription.cancel sub);
        last := Some (iter s ~f:(fun x -> trigger t' x))
      )
    );
    t'
  ;;

  let bind t ~f = join (map t ~f)

  let ticks ms =
    let t                    = create () in
    let ms                   = Js.float ms in
    let rec update () : unit =
      trigger t (Time.now ());
      Js.Unsafe.(fun_call (variable "setTimeout")
        [| inject ms; inject (Js.wrap_callback update) |]
      )
    in
    update ();
    t
  ;;

  let delta t ~f =
    let t'   = create () in
    let last = ref None in
    iter t ~f:(fun x ->
      Option.iter !last ~f:(fun v -> trigger t' (f v x));
      last := Some x
    ) |> ignore;
    t'
  ;;

  let deltas ms = delta (ticks ms) (fun t1 t2 -> Time.(t2 - t1))

  let sequence ts =
    let t   = create () in
    let buf = Array.init (Array.length ts) ~f:(fun _ -> Queue.create ()) in
    Array.iteri ts ~f:(fun i x ->
      iter x ~f:(fun v ->
        Queue.enqueue buf.(i) v;
        if Array.for_all buf ~f:(fun q -> Option.is_some (Queue.peek q))
        then trigger t (Array.map buf ~f:Queue.dequeue_exn)
      ) |> ignore
    );
    t
  ;;

  module Infix = struct
    let (>>=) t f = bind t ~f

    let (>>|) t f = map t ~f

    let (<$>) f t = map t ~f
    
    let (<*>) = ap
  end
end

module Behavior = struct
  type 'a t =
    { mutable listeners : ('a -> unit) list
    ; mutable value     : 'a
    }

  (*
  type _ t =
    | Fn     : (Time.t -> 'a) -> 'a t
    | K      : 'a -> 'a t
    | Switch : 'b Stream.t * ('b -> 'a Behavior.t) -> 'a Behavior.t
  *)

  let set t x = t.value <- x

  let add_listener t f =
    t.listeners <- f :: t.listeners 

  let notify_listeners t =
    List.iter ~f:(fun f -> f t.value) t.listeners

  let trigger t x =
    t.value <- x;
    notify_listeners t
  ;;

  let return init = { value = init ; listeners = [] }

  let map t ~f =
    let t' = return (f t.value) in
    add_listener t (fun x -> trigger t' (f x));
    t'
  ;;

  let zip_with t1 t2 ~f =
    let t' = return (f t1.value t2.value) in
    add_listener t1 (fun x -> trigger t' (f x t2.value));
    add_listener t2 (fun y -> trigger t' (f t1.value y));
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

  let changes t =
    let s = Stream.create () in
    add_listener t (trigger t);
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
      b.Behavior.value <- f b.Behavior.value x
    )
  );
  b
;;

let project b s         = Stream.map s ~f:(fun _ -> b.Behavior.value)
let project_with b s ~f = Stream.map s ~f:(fun x -> f b.Behavior.value x)

