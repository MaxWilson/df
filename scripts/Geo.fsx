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
type Constraint = NearThing of Id * distance: float | NearCoord of Coord * distance: float
type Direction = Up | Down | Left | Right
type Preference = CloseTo of Id | AwayFrom of Id | Direction of Direction
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
let place (thingId: Id, coords) =
    if not (points |> List.contains coords) then shouldntHappen "Out of bounds, out of scope for this scenario."
    spaces <- spaces |> Map.change coords (function Some lst -> Some (lst@[thingId]) | None -> Some [thingId])
    occupancy <- occupancy |> Map.add (thingId) coords
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
let thingsInRegion (coords: Coord list) : 't list = notImpl()
let pointsNear(coord: Coord, constraints: Constraint list, preferences: Preference list) =
    notImpl()

init()
printfn ""
place("Bob", (1.5, 10.))
place("Doomchild3", (1., 10.))
place("Doomchild", (1.5, 10.5))
draw() |> printfn "%s"
move("Bob", (1.5, 10.), (1.5, 10.))
draw() |> printfn "%s"
move("Bob", (1.5, 10.), (2., 20.))
draw() |> printfn "\n%s"
