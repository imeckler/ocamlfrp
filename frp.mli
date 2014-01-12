module Behavior : sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val zip_with : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val return : 'a -> 'a t

  val join : 'a t t -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  
  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val (<$>) : ('a -> 'b) -> 'a t -> 'b t

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end

  val set : 'a t -> 'a -> unit
end

module Subscription : sig
  type t

  val cancel : t -> unit
end

module Stream : sig
  type 'a t

  val trigger : 'a t -> 'a -> unit

(*   val set : 'a t -> 'a -> unit *)

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val iter : 'a t -> f:('a -> unit) -> Subscription.t

  val filter : 'a t -> f:('a -> bool) -> 'a t

  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val zip_with : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val merge : 'a t -> 'a t -> 'a t

  val drop : 'a t -> int -> 'a t
  
  val tail : 'a t -> 'a t

  val join : 'a t t -> 'a t

  val switch : 'a t t -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val (<$>) : ('a -> 'b) -> 'a t -> 'b t

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end
end

val when_ : bool Behavior.t -> 'a Stream.t -> 'a Stream.t

(* Impossible to implement atm since streams don't hold onto their last values *)
(* val switch : 'a Behavior.t Stream.t -> 'a Behavior.t *)

val scan : 'a Stream.t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum Behavior.t

val project : 'a Behavior.t -> 'b Stream.t -> 'a Stream.t

val project_with : 'a Behavior.t -> 'b Stream.t -> f:('a -> 'b -> 'c) -> 'c Stream.t

