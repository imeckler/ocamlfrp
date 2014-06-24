open Core

(** OCamlFRP is an OCaml library for functional reactive programming,
    tailored for use in programs compiled to Javascript using js_of_ocaml.
    It follows the traditional model of FRP with a stream type for discrete
    events and a behavior type for values which, at least conceptually,
    vary continuously.
*)


module Subscription : sig
  (** A [Subscription.t] is a token which allows one to stop a listener
      from firing. *)
  type t

  val empty : t

  val cancel : t -> unit

  val merge : t -> t -> t

  val concat : t array -> t

  val make : (unit -> unit) -> t
end

module Stream : sig
  (** The type of discrete streams of values.

      An important caveat: A stream won't update unless it or a derived stream
      has a side-effecting listener (which was attached using [iter], described
      below). E.g., if [number_stream : int t] and one writes
      [ let (unit_stream : unit t) = map ~f:print_int number_stream]
      then nothing will be printed unless [iter] is later called on
      [unit_stream] or something derived from [unit_stream]. *)
  type 'a t

  type 'a trigger = 'a -> unit

  (** Create a stream from an existing one by appling [f] to each value in
      the stream.

      For example, if [click_points : (int * int) t] is a stream of points
      at which the mouse was clicked, then [click_x = map ~f:fst click_points]
      is a stream which updates with the x coordinate of the click point
      whenever the original stream updates *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** Attaches a side-effecting listener to a stream and returns a subscription
      token which allows you to remove the listener. *)
  val iter : 'a t -> f:('a -> unit) -> Subscription.t

  val filter : 'a t -> f:('a -> bool) -> 'a t

  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val zip_with : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val zip_many : 'a t array -> f:('a array -> 'b) -> 'b t

  val sequence : 'a t array -> 'a array t

  val merge : 'a t -> 'a t -> 'a t

  val merge_many : 'a t array -> 'a t

  val take : 'a t -> int -> 'a t

  val drop : 'a t -> int -> 'a t
  
  val tail : 'a t -> 'a t

  val join : 'a t t -> 'a t

  val switch : 'a t t -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val ticks : float -> Time.t t

  val elapsed : float -> Time.Span.t t

  val delta : 'a t -> f:('a -> 'a -> 'b) -> 'b t

  val deltas : float -> Time.Span.t t

  val skip_duplicates : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t

  (* Let's see how this type does *)

  val create : ?start:('a trigger -> (unit -> unit)) -> unit -> 'a t

  val create' : ?start:('a trigger -> (unit -> unit)) -> unit -> 'a t * 'a trigger

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val (<$>) : ('a -> 'b) -> 'a t -> 'b t

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end

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

  val force_updates : 'a t -> unit
  val stop_updates : 'a t -> unit 

  val set : 'a t -> 'a -> unit

  val peek : 'a t -> 'a

  val changes : 'a t -> 'a Stream.t
end


val when_ : bool Behavior.t -> 'a Stream.t -> 'a Stream.t

(* Impossible to implement atm since streams don't hold onto their last values *)
(* val switch : 'a Behavior.t Stream.t -> 'a Behavior.t *)

val scan : 'a Stream.t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum Behavior.t

val latest : 'a Stream.t -> init:'a -> 'a Behavior.t

val project : 'a Behavior.t -> 'b Stream.t -> 'a Stream.t

val project_with : 'a Behavior.t -> 'b Stream.t -> f:('a -> 'b -> 'c) -> 'c Stream.t

