open Core

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

  let trigger t x =
    t.value <- x;
    List.iter ~f:(fun f -> f x) t.listeners
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

  let bind t ~f = join (map t ~f)

  module Infix = struct
    let (>>=) t f = bind t ~f
    let (>>|) t f = map t ~f
    let (<$>) f t = map t ~f
    let (<*>)     = ap
  end
end

module Subscription = struct
  type t = unit -> unit

  let cancel t = t ()
end

module Stream = struct
  type 'a t =
    { listeners   : ('a -> unit) Inttbl.t
    ; mutable uid : int
    }

  let trigger t x = Inttbl.iter ~f:(fun ~key ~data -> data x) t.listeners

  let iter t ~f =
    let key = t.uid in
    t.uid <- key + 1;
    Inttbl.add t.listeners ~key ~data:f;
    fun () -> Inttbl.remove t.listeners key
  ;;

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
    let t             = create () in
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

  module Infix = struct
    let (>>=) t f = bind t ~f

    let (>>|) t f = map t ~f

    let (<$>) f t = map t ~f
    
    let (<*>) = ap
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

