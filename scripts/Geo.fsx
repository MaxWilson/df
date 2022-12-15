module Geo
#I __SOURCE_DIRECTORY__
#load "Common.fsx"

type API<'T, 'Id> =
    abstract getId: 'T -> 'Id

//type T = string
type Coord = float * float
type 'id Constraint = NearThing of 'id * distance: float | NearCoord of Coord * distance: float
type 'id Preference = CloseTo of 'id | AwayFrom of 'id
type DiscreteGeo<'t, 'id when 'id: comparison and 't: comparison>(ms, ns, api: API<_,_>) =
    let points : Coord list =
        [for m in ms do
            for n in ns do
                m,n]
    let mutable spaces : Map<Coord, 't list> = points |> List.map (fun p -> p, []) |> Map.ofList
    let mutable occupancy : Map<'id, Coord> = Map.empty
    let place (thing: 't, coords) =
        if not (points |> List.contains coords) then shouldntHappen "Out of bounds, out of scope for this scenario."
        spaces <- spaces |> Map.change coords (function Some lst -> Some (lst@[thing]) | None -> Some [thing])
        occupancy <- occupancy |> Map.add (api.getId thing) coords
    let move (thing: 't, from, to1) =
        if not (points |> List.contains from) then shouldntHappen "Out of bounds, out of scope for this scenario."
        elif not (points |> List.contains to1) then shouldntHappen "Out of bounds, out of scope for this scenario."
        else
            let creature, remainder = spaces[from] |> List.partition ((=)thing)
            if creature.IsEmpty then
                shouldntHappen $"Placement error! {thing} isn't at {from} in the first place!"
            spaces <- spaces
                    |> Map.add from remainder
                    |> Map.change to1 (function Some lst -> Some (lst@[thing]) | None -> Some [thing])
    let draw() : string =
        let s = System.Text.StringBuilder()
        for m in ms do
            for n in ns do
                match spaces |> Map.tryFind (m,n) with
                | Some (thing::_) when (api.getId thing).ToString().Length > 0 ->
                    s.Append ((api.getId thing).ToString().Substring(0, 1))
                | None | Some [] | Some _ ->
                    s.Append "."
                |> ignore
            s.AppendLine "" |> ignore
        s.ToString()
    let thingsInRegion (coords: Coord list) : 't list = notImpl()
    let pointsNear(coord: Coord, constraints: _ Constraint list, preferences: _ Preference list) =
        notImpl()
    member _.Place args = place args
    member _.Move args = move args
    member _.Draw() = draw()

type Api() =
    interface API<string, string> with
        member _.getId (s:string) = s
let (ms, ns) = ([(1.)..0.5..(2.)], [(0.)..0.5..(20.)])
let geo = DiscreteGeo(ms, ns, Api())
geo.Place("Bob", (1.5, 10.))
geo.Draw() |> printfn "%s"
geo.Move("Bob", (1.5, 10.), (1.5, 10.))
geo.Draw() |> printfn "%s"
geo.Move("Bob", (1.5, 10.), (2., 20.))
geo.Draw() |> printfn "%s"