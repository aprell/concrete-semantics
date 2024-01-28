val assert_property : ?name:string -> ('a -> bool) -> 'a list -> unit
val assert_equal : ?name:string -> 'a -> 'a -> unit
val equivalent : ('a -> 'b) -> ('a -> 'b) -> 'a list -> bool
val until : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
val while' : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
val printf : ?indent:int -> ('a, out_channel, unit) format -> 'a
val sprintf : ?indent:int -> ('a, unit, string, string) format4 -> 'a
