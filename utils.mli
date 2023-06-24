val assert_property : ?name:string -> ('a -> bool) -> 'a list -> unit

val equivalent : ('a -> 'b) -> ('a -> 'b) -> 'a list -> bool
