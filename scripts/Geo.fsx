#if INTERACTIVE
#else
module Geo
#endif
#I __SOURCE_DIRECTORY__
#load "Common.fsx"

type API<'id, 'actual> =
    abstract getActual: 'id -> 'actual


//type T = string
type Coord = float * float
type Id = string
type Constraint = Empty | Near of Id * distance: float | NearPlace of Coord * distance: float
type Direction = Up | Down | Left | Right
type Preference = CloseTo of Id | AwayFrom of Id | CloseToPlace of Coord | Direction of Direction
let (ms, ns) = ([(1.)..0.5..(2.)], [(0.)..0.5..(20.)])

type Api() =
    interface API<string, string> with
        member this.getActual(id: Id): string = notImpl()

let mutable points : Coord list = []

let mutable spaces : Map<Coord, Id list> = points |> List.map (fun p -> p, []) |> Map.ofList
let mutable occupancy : Map<Id, Coord> = Map.empty

let init() =
    points <-
        [for m in ms do
            for n in ns do
                m,n]
    spaces <- points |> List.map (fun p -> p, []) |> Map.ofList
    occupancy <- Map.empty
let move (thingId: Id, to1) =
    if not (points |> List.contains to1) then shouldntHappen "Out of bounds, out of scope for this scenario."
    else
        let from = occupancy[thingId]
        let creature, remainder = spaces[from] |> List.partition ((=)thingId)
        if creature.IsEmpty then
            shouldntHappen $"Placement error! {thingId} isn't at {from} in the first place!"
        spaces <- spaces
                |> Map.add from remainder
                |> Map.change to1 (function Some lst -> Some (lst@[thingId]) | None -> Some [thingId])
        occupancy <- occupancy |> Map.add thingId to1
let draw() : string =
    let s = System.Text.StringBuilder()
    for m in ms do
        for n in ns do
            match spaces |> Map.tryFind (m,n) with
            | Some (thingId::_) when thingId.Length > 0 ->
                if thingId.Length >= 2 then
                    s.Append $"{thingId[0]}{thingId[thingId.Length - 1]}"
                else
                   s.Append $"{thingId} "
            | None | Some [] | Some _ ->
                s.Append ". "
            |> ignore
        s.AppendLine "" |> ignore
    s.ToString()
let distance ((m1, n1): Coord) ((m2, n2): Coord) : float =
    let (m,n) = (m1-m2), (n1-n2)
    sqrt(m*m + n*n)
let thingsInRegion (coords: Coord list) : _ list =
    coords |> List.collect(fun c -> spaces[c])
let region(constraints: Constraint list, preferences: Preference list) : Coord list =
    let rec constrain filter orderBy = function
        | [] -> filter, orderBy
        | Empty::rest ->
            let filter rhs = (filter rhs && spaces[rhs].IsEmpty)
            constrain filter orderBy rest
        | Near(id, dist)::rest ->
            let otherCoords = occupancy[id]
            let filter rhs = (filter rhs && distance otherCoords rhs <= dist)
            constrain filter orderBy rest
        | NearPlace(otherCoords, dist)::rest ->
            let filter rhs = (filter rhs && distance otherCoords rhs <= dist)
            constrain filter orderBy rest
    let rec order orderBy = function
        | [] -> orderBy
        | CloseTo(id)::rest ->
            let otherCoords = occupancy[id]
            order (List.sortBy (distance otherCoords) >> orderBy) rest
        | CloseToPlace(otherCoords)::rest ->
            order (List.sortBy (distance otherCoords) >> orderBy) rest
        | AwayFrom(id)::rest ->
            let otherCoords = occupancy[id]
            order (List.sortByDescending (distance otherCoords) >> orderBy) rest
        | Direction(dir)::rest ->
            let directionality (m,n) =
                match dir with
                | Up -> m
                | Down -> -m
                | Left -> n
                | Right -> -n
            order (List.sortBy (directionality) >> orderBy) rest
    let filter, orderBy = constrain (fun coord -> true) id constraints
    let orderBy = order orderBy preferences
    points |> List.filter filter |> orderBy
let place (thingId: Id, constraints: Constraint list, prefer : Preference list) : Coord =
    match region(Empty::constraints, prefer) with
    | (coords::_) ->
        if not (points |> List.contains coords) then shouldntHappen "Out of bounds, out of scope for this scenario."
        spaces <- spaces |> Map.change coords (function Some lst -> Some (lst@[thingId]) | None -> Some [thingId])
        occupancy <- occupancy |> Map.add (thingId) coords
        coords
    | [] ->
        failwith "Not possible, sorry!"

type GeoFsi =
    //static member move(id, point) = move(id, point)
    static member move(id, ?constraints, ?preferences) =
        let points = region(Empty::(defaultArg constraints []), defaultArg preferences [])
        move(id, points.Head)
    static member place(id, x, y) = let coords = x,y in place(id, [NearPlace(coords, 3)], [CloseToPlace coords])
    static member place(id, ?constraints, ?preferences) = place(id, defaultArg constraints [], defaultArg preferences [])
    static member moveTowards(id, otherId, ?mv, ?prefs) =
        GeoFsi.move(id, [Near(id, defaultArg mv 1.2)], [CloseTo otherId]@(defaultArg prefs []))
    static member moveAway(id, otherId, ?mv, ?prefs) =
        GeoFsi.move(id, [Near(id, defaultArg mv 1.2)], [AwayFrom otherId]@(defaultArg prefs []))
    static member region(?constraints, ?prefs) = region(defaultArg constraints [], defaultArg prefs [])
    static member things(?constraints, ?prefs) = region(defaultArg constraints [], defaultArg prefs []) |> thingsInRegion
//open type GeoFsi
//init()
//printfn ""
//place("Bob", 1.5, 10.)
//place("Fred", [Near ("Bob",3.)], [CloseTo "Bob"; Direction Right])
//place("Doomchild3", 1., 10.)
//place("Doomchild", 1.5, 10.5)
//for _ in 1..20 do
//    moveAway("Bob", "Doomchild", 3., [AwayFrom "Doomchild3"])
//    moveTowards("Fred", "Bob")
//    moveTowards("Doomchild", "Fred")
//    moveTowards("Doomchild3", "Fred")
//    draw() |> printfn "\n%s"
//    System.Threading.Thread.Sleep 50
//for id in things([Near("Doomchild", 5.)]) do
//    printfn "%s: %f from epicenter" id (distance occupancy[id] occupancy["Doomchild"])
