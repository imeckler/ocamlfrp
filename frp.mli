module Stream : sig
  type 'a t

  val create : 'a -> 'a t

  val trigger : 'a t -> 'a -> unit

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val iter : 'a t -> f:('a -> unit) -> unit

  val filter : 'a t -> f:('a -> bool) -> 'a t

  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum t

  val zip_with : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val merge : 'a t -> 'a t -> 'a t

  val when_ : bool t -> 'a t -> 'a t

  val join : 'a t t -> 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val (<$>) : ('a -> 'b) -> 'a t -> 'b t

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end
end

