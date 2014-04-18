open Core

module Subscription : sig
  type t

  val empty : t

  val cancel : t -> unit

  val merge : t -> t -> t

  val concat : t array -> t

  val make : (unit -> unit) -> t
end

module Stream : sig
  type ('b, 'a) t

  type 'a td = ([`derived], 'a) t

(*   val set : 'a t -> 'a -> unit *)

  val map : ('c, 'a) t -> f:('a -> 'b) -> 'a td

  val iter : ('c, 'a) t -> f:('a -> unit) -> Subscription.t

  val filter : ('c, 'a) t -> f:('a -> bool) -> 'a td

  val fold : ('c, 'a) t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum td

  val ap : ('c, ('a -> 'b)) t -> ('d, 'a) t -> 'b td

  val zip_with : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c td

  val zip : 'a t -> 'b t -> ('a * 'b) td

  val merge : 'a t -> 'a t -> 'a td

  val drop : 'a t -> int -> 'a td
  
  val tail : 'a t -> 'a td

  val join : 'a t t -> 'a td

  val switch : 'a t t -> 'a td

  val bind : 'a t -> f:('a -> 'b t) -> 'b td

  val ticks : float -> Time.t t

  val elapsed : float -> Time.Span.t t

  val delta : 'a t -> f:('a -> 'a -> 'b) -> 'b t

  val deltas : float -> Time.Span.t t

  val skip_duplicates : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val (<$>) : ('a -> 'b) -> 'a t -> 'b t

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end

  val trigger : 'a t -> 'a -> unit

  val create  : unit -> 'a t

  val stop_updating : 'a t -> unit
end

module Behavior : sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val zip_with : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val zip_many : 'a t array -> f:('a array -> 'b) -> 'b t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val return : 'a -> 'a t

  val join : 'a t t -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val skip_duplicates : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val (<$>) : ('a -> 'b) -> 'a t -> 'b t

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end

  val set : 'a t -> 'a -> unit

  val trigger : 'a t -> 'a -> unit

  val peek : 'a t -> 'a
  
  val notify_listeners : 'a t -> unit

  val changes : 'a t -> 'a Stream.t
end


val when_ : bool Behavior.t -> 'a Stream.t -> 'a Stream.t

(* Impossible to implement atm since streams don't hold onto their last values *)
(* val switch : 'a Behavior.t Stream.t -> 'a Behavior.t *)

val scan : 'a Stream.t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum Behavior.t

val project : 'a Behavior.t -> 'b Stream.t -> 'a Stream.t

val project_with : 'a Behavior.t -> 'b Stream.t -> f:('a -> 'b -> 'c) -> 'c Stream.t

