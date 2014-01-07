open Core.Std

module Stream = struct
  type 'a t =
    { mutable listeners : ('a -> unit) list
    ; mutable value     : 'a
    (* TODO: add this field to manage unsubscribing
    ; mutable uid       : int *)
    }

  let set t x = t.value <- x

  let add_listener t f =
    t.listeners <- f :: t.listeners 

  let trigger t x =
    t.value <- x;
    List.iter t.listeners ~f:(fun f -> f x)
  ;;

  let iter t ~f = add_listener t f

  let create init = { value = init ; listeners = [] }

  let map t ~f =
    let t' = create (f t.value) in
    add_listener t (fun x -> trigger t' (f x));
    t'
  ;;

  let filter t ~f =
    let t' = create t.value in
    add_listener t (fun x -> if f x then trigger t' x);
    t'
  ;;

  let fold t ~init ~f =
    let t' = create init in
    add_listener t (fun x -> trigger t' (f t'.value x));
    t'
  ;;

  let zip_with t1 t2 ~f =
    let t' = create (f t1.value t2.value) in
    add_listener t1 (fun x -> trigger t' (f x t2.value));
    add_listener t2 (fun y -> trigger t' (f t1.value y));
    t'
  ;;

  let zip = zip_with ~f:(fun x y -> (x, y))

  let merge t1 t2 =
    let t' = create t1.value in
    add_listener t1 (fun x -> trigger t' x);
    add_listener t2 (fun x -> trigger t' x);
    t'
  ;;

  let when_ cond t =
    let t' = create t.value in
    add_listener t (fun x -> if cond.value then trigger t' x);
    t'
  ;;

  let join t =
    let t' = create t.value.value in
    add_listener t.value (fun x -> trigger t' x);
    add_listener t (fun s ->
      add_listener s (fun x -> trigger t' x)
    );
    t'
  ;;

  let return = create

  let bind t ~f = join (map t ~f)

  module Infix = struct
    let (>>=) t f = bind t ~f

    let (>>|) t f = map t ~f

    let (<$>) f t = map t ~f
    
    let (<*>) tf tx = zip_with tf tx ~f:(fun f x -> f x)
  end
end

