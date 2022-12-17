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
type Constraint = Near of Id * distance: float | NearPlace of Coord * distance: float
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
let move (thingId: Id, from, to1) =
    if not (points |> List.contains from) then shouldntHappen "Out of bounds, out of scope for this scenario."
    elif not (points |> List.contains to1) then shouldntHappen "Out of bounds, out of scope for this scenario."
    else
        let creature, remainder = spaces[from] |> List.partition ((=)thingId)
        if creature.IsEmpty then
            shouldntHappen $"Placement error! {thingId} isn't at {from} in the first place!"
        spaces <- spaces
                |> Map.add from remainder
                |> Map.change to1 (function Some lst -> Some (lst@[thingId]) | None -> Some [thingId])
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
let thingsInRegion (coords: Coord list) : 't list = notImpl()
let region(constraints: Constraint list, preferences: Preference list) : Coord list =
    let rec constrain filter orderBy = function
        | [] -> filter, orderBy
        | Near(id, dist)::rest ->
            let otherCoords = occupancy[id]
            let filter rhs = (filter rhs && distance otherCoords rhs <= dist)
            let orderBy = List.sortBy (distance otherCoords) >> orderBy
            constrain filter orderBy rest
        | NearPlace(otherCoords, dist)::rest ->
            let filter rhs = (filter rhs && distance otherCoords rhs <= dist)
            let orderBy = List.sortBy (distance otherCoords) >> orderBy
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
    let filter, orderBy = constrain (fun coord -> spaces[coord].IsEmpty) id constraints
    let orderBy = order orderBy preferences
    points |> List.filter filter |> orderBy
let place (thingId: Id, constraints: Constraint list, prefer : Preference list) : Coord =
    match region(constraints, prefer) with
    | (coords::_) ->
        if not (points |> List.contains coords) then shouldntHappen "Out of bounds, out of scope for this scenario."
        spaces <- spaces |> Map.change coords (function Some lst -> Some (lst@[thingId]) | None -> Some [thingId])
        occupancy <- occupancy |> Map.add (thingId) coords
        coords
    | [] ->
        failwith "Not possible, sorry!"

init()
printfn ""
let place1(id, x, y) = let coords = x,y in place(id, [NearPlace(coords, 3)], [CloseToPlace coords])
place1("Bob", 1.5, 10.)
place("Fred", [Near ("Bob",3.)], [Direction Right])
place1("Doomchild3", 1., 10.)
place1("Doomchild", 1.5, 10.5)
draw() |> printfn "%s"
move("Bob", (1.5, 10.), (1.5, 10.))
draw() |> printfn "%s"
move("Bob", (1.5, 10.), (2., 20.))
draw() |> printfn "\n%s"
region([NearPlace ((1.5, 10.), 2)], [Direction Up])
region([Near ("Bob", 2)], [AwayFrom "Doomchild3"; AwayFrom "Doomchild"])