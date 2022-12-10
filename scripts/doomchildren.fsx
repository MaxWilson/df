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
type Status = Ok | MentalStun | Unconscious | Dead
type Roll = Roll of n:int * d:int * bonus: int
    with
    static member d(n) = Roll(n, 6, 0)
    static member roll n plus = Roll(n, 6, plus)
open type Roll

module Damage =
    type DamageType = Cutting | Impaling | Crushing
open Damage
type CreatureStats = {
    st: int
    hp: int
    fp: int
    dr: int
    damage: Roll * DamageType
    }
type CreatureStatus = {
    stats: CreatureStats
    status: Status
    }
type Creature = { name: string; id: Id; originalStats: CreatureStats; mutable status: CreatureStatus; mutable roundInfo: RoundInfo }
type World(map) =
    let mutable denizens: Map<Id, Creature> = map
    let addCreature(name, stats) =
        let rec getId candidate counter =
            if denizens.ContainsKey candidate then getId (sprintf "%s%d" name counter) (counter + 1)
            else candidate
        let id = getId name 2
        let creature = { name = name; id = id; originalStats = stats; status = { stats = stats; status = Ok }; roundInfo = RoundInfo.fresh }
        denizens <- denizens |> Map.add id creature
        denizens[id]
    member this.getDenizens() = denizens
    member this.add(name, stats) = addCreature(name, stats)
    member this.add(name, ?st, ?hp, ?fp, ?dr, ?damage) =
        let either v1 v2 = v1 |> Option.defaultValue v2
        let st = either st 10
        let stats: CreatureStats = {
            st = st
            hp = either hp st
            fp = either fp 10
            dr = either dr 0
            damage = either damage (roll 1 -1, Crushing)
            }
        addCreature(name, stats)
let world = World(Map.empty)
world.add("Doomchild", st=12).id