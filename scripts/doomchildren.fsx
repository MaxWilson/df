#I __SOURCE_DIRECTORY__
#load "Common.fsx"
open System

type Id = string
type RoundInfo = {
    hasRetreated: bool
    parries: int
    blocks: int
    blockingSpells: int
    hasActed: bool
    }
    with static member fresh = { hasRetreated = false; hasActed = false; parries = 0; blocks = 0; blockingSpells = 0 }
type Condition = MentalStun | PhysicalStun | Unconscious | Dead
type Status = Condition list
let Ok = []
type Roll = { n:int; d:int; bonus: int }
    with
    static member d6(n) = { n = n; d = 6; bonus = 0 }
    static member roll n plus = { n = n; d = 6; bonus = plus }
    member this.throw = ([for _ in 1..this.n -> 1 + rand.Next(this.d) ] |> List.sum) + this.bonus
open type Roll

module Damage =
    type DamageType = Cutting | Impaling | Crushing
open Damage
type Mod =
    | StrikingST of int
    | Berserk of int
type CreatureStats = {
    st: int
    hp: int
    fp: int
    dr: int
    mods: Mod list
    damage: Roll * DamageType
    }
type CreatureStatus = {
    stats: CreatureStats
    status: Status
    }
type Creature = { name: string; id: Id; originalStats: CreatureStats; mutable status: CreatureStatus; mutable roundInfo: RoundInfo }
type World(map, log) =
    let mutable denizens: Map<Id, Creature> = map
    let mutable log : string list = log
    let addCreature(name, stats) =
        let rec getId candidate counter =
            if denizens.ContainsKey candidate then getId (sprintf "%s%d" name counter) (counter + 1)
            else candidate
        let id = getId name 2
        let creature = { name = name; id = id; originalStats = stats; status = { stats = stats; status = Ok }; roundInfo = RoundInfo.fresh }
        denizens <- denizens |> Map.add id creature
        denizens[id]
    member this.Item with get id = denizens[id]
    member this.getLog() = log
    member this.remember (msg: string) = log <- msg :: log
    member this.getDenizens() = denizens
    member this.add(name, stats) = addCreature(name, stats)
    member this.add(name, ?st, ?hp, ?fp, ?dr, ?damage, ?mods) =
        let either v1 v2 = v1 |> Option.defaultValue v2
        let st = either st 10
        let stats: CreatureStats = {
            st = st
            hp = either hp st
            fp = either fp 10
            dr = either dr 0
            damage = either damage (roll 1 -1, Crushing)
            mods = either mods []
            }
        addCreature(name, stats)
let world = World(Map.empty, [])

let updateStats f (c:Creature) =
    c.status <- { c.status with stats = c.status.stats |> f }
    c
let updateStatus f (c:Creature) =
    c.status <- { c.status with status = c.status.status |> f }
    c
let addCondition cond = updateStatus (List.append [cond])
let removeCondition cond = updateStatus (List.filter ((<>)cond))
let kill (target: Creature) =
    target |> addCondition Dead |> updateStats (fun stats -> { stats with hp = target.originalStats.hp * -5 })

type Outcome = CritSuccess | Success | Fail | CritFail
type Actions(world: World) =
    let opposedRoll (aggressorName, aggressorSkill as aggressor) (defenderName, defenderSkill as defender) =
        let eval skill roll =
            match roll with
            | 3 | 4 when skill >= 3 -> CritSuccess, max 0 (skill - roll)
            | 5 | 6 as roll when skill - 10 >= 10 -> CritSuccess, max 0 (skill - roll)
            | 17 -> (if skill >= 16 then Fail else CritFail), max 0 (roll - skill)
            | 18 -> CritFail, max 0 (roll - skill)
            | roll when roll > skill -> (if roll - 10 >= skill then CritFail else Fail), max 0 (roll - skill)
            | roll -> Success, max 0 (skill - roll)
        let throw (name, skill) =
            let v = (d6 3).throw
            let descr, skillValue = skill
            let result, margin = eval skillValue v
            world.remember $"{name} rolls {v} for {descr}: {result} by {margin}"
            result, margin
        let success() = world.remember $"{aggressorName} wins"; Success
        let fail() = world.remember $"{defenderName} wins"; Fail
        match throw aggressor, throw defender with
        | ((CritSuccess | Success), margin1), ((CritSuccess | Success), margin2) ->
            if margin1 > margin2 then success() else fail()
        | ((CritSuccess | Success), _), _ -> success()
        | _ -> fail()
    //opposedRoll ("Bob", ("axe", 16)) ("Doomchild", ("knife parry", 11))
    //world.getLog() |> List.rev |> List.iter (printfn "%s")
    member this.attack (src:string) (target:string) =
        let src = world[src]
        let target = world[target]

        world.remember $"{src.id} attacks {target.id}"
let a = Actions(world)
for _ in 1..3 do
    world.add("Doomchild", st=10, hp=8, damage = (roll 3 -2, Cutting), mods=[Berserk 12; StrikingST +2]) |> fun s -> printfn $"Created {s.id}"
a.attack "Doomchild" "Doomchild2"
a.attack "Doomchild3" "Doomchild2"
world.getLog()
world["Doomchild2"].status.status

world["Doomchild2"] |> kill
world["Doomchild2"].status.status