open Core

let expected_programs = 99999

type cache = {
    normal : (Program.t, bool) Hashtbl.t;
    weight : (Program.t, int) Hashtbl.t;
    kbo : ((Program.t * Program.t), bool) Hashtbl.t
}

let merge_tables (h1 : ('a, 'b) Hashtbl.t) (h2 : ('a, 'b) Hashtbl.t) : (('a, 'b) Hashtbl.t) =
    let output = Hashtbl.copy h1 in
    let f = fun k v -> Hashtbl.add output k v in
        Hashtbl.iter f h2; output

module Cache = struct
    type t = cache
    (* when we initalize, just make the tables of the expected size *)
    let empty : t = {
        normal = Hashtbl.create expected_programs;
        weight = Hashtbl.create expected_programs;
        kbo = Hashtbl.create expected_programs
    }
    let merge (c1 : t) (c2 : t) : t = {
        normal = merge_tables c1.normal c2.normal;
        weight = merge_tables c1.weight c2.weight;
        kbo = merge_tables c1.kbo c2.kbo;
    }
    let add (h : ('a, 'b) Hashtbl.t) (key : 'a) (ans : 'b) : 'b = begin
        Hashtbl.add h key ans; ans
    end
    let find (h : ('a, 'b) Hashtbl.t) (key : 'a) : 'b option =
        try Some (Hashtbl.find h key)
        with Not_found -> None
end
