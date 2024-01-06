val assert_property : ?name:string -> ('a -> bool) -> 'a list -> unit

val assert_equal : ?name:string -> 'a -> 'a -> unit

val equivalent : ('a -> 'b) -> ('a -> 'b) -> 'a list -> bool

val until : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
