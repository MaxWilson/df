#I __SOURCE_DIRECTORY__
#load "Common.fsx"
open System

type Id = string
type RoundInfo = {
    retreatedFrom: Id option
    parries: int
    blocks: int
    blockingSpells: int
    hasActed: bool
    shockPenalty: int
    }
    with static member fresh = { retreatedFrom = None; hasActed = false; parries = 0; blocks = 0; blockingSpells = 0; shockPenalty = 0 }
type Condition = MentalStun | PhysicalStun | Unconscious | Dead | Prone // prone is technically a posture not a condition, but we'll leave posture as TODO for now
type Status = Condition list
let Ok = []
[<StructuredFormatDisplay("{StringRep}")>]
type Roll = { n:int; d:int; bonus: int }
    with
    static member d6(n) = { n = n; d = 6; bonus = 0 }
    static member roll n plus = { n = n; d = 6; bonus = plus }
    static member create(n,?d,?plus) = { n = n; d = defaultArg d 6; bonus = defaultArg plus 0 }
    member this.throw = ([for _ in 1..this.n -> 1 + rand.Next(this.d) ] |> List.sum) + this.bonus
    member this.plus x = { this with bonus = this.bonus + x }
    member this.StringRep = this.ToString()
    override this.ToString() =
        match this.n, this.d, this.bonus with
        | n, 6, 0 -> $"{n}d"
        | n, 6, v when v < 0 -> $"{n}d-{-v}"
        | n, 6, plus -> $"{n}d+{plus}"
        | n, d, 0 -> "${n}d{d}"
        | n, d, v when v < 0 -> "${n}d{d}-{-plus}"
        | n, d, plus -> "${n}d{d}+{plus}"
open type Roll

type Location = Torso | Skull | Eye | Arm | Leg | Hand | Foot | Neck | Vitals | Face
    with
    static member Random() =
        match (d6 3).throw with
        | 3 | 4 -> Skull
        | 5 -> Face
        | 6 | 7 -> Leg // right
        | 8 -> Arm // right
        | 9 | 10 | 11 -> Torso
        | 12 -> Arm // left
        | 13 | 14 -> Leg // left
        | 15 -> Hand
        | 16 -> Foot
        | 17 | 18 -> Neck
        | _ -> shouldntHappen()
    member this.hitPenalty =
        match this with
        | Eye -> -9
        | Skull -> -7
        | Hand | Foot -> -4
        | Vitals -> -3
        | Arm | Leg -> -2
        | Face | Neck -> -5
        | Torso -> 0
module Damage =
    type DamageType = Cutting | Impaling | Crushing | Piercing | Burning
    let woundingMultiplier = function
        | (Skull | Eye), _ -> 4.
        | Neck, Cutting -> 2.
        | _, Cutting -> 1.5
        | Neck, Crushing -> 1.5
        | Vitals, (Piercing | Impaling) -> 3.
        | (Arm | Leg | Hand | Foot), Impaling -> 1.
        | _, Impaling -> 2.
        | _ -> 1.

    let thr st =
        if st <= 14 then roll 1 (-(14-st)/2)
        else roll ((st - 3)/8) (((st - 3)/2 % 4) - 1) // slightly inaccurate over ST 50 but who cares, this is a better house rule anyway
    let sw st =
        if st < 10 then roll 1 (-(12-st)/2)
        elif st <= 27 then roll ((st - 5)/4) (((st - 5) % 4) - 1)
        else roll ((22 + (st - 27)/2)/4) (((22 + (st - 27)/2) % 4) - 1) // inaccurate at ST 45+ but who cares, this is a better house rule anyway
open Damage
type Mod =
    | StrikingST of int
    | Berserk of int
    | CombatReflexes
type DamageClass = Swing | Thrust
type ReadiedWeapon = {
    description: string option
    skill: CreatureStats -> int
    damageType: DamageType
    damageRoll: Roll option
    damageClass: DamageClass
    damageMod: int
    }
    with
    static member create(skill, roll, type1) = { description = None; skill = skill; damageType = type1; damageRoll = Some roll; damageClass = Thrust; damageMod = 0 }
    static member create(skill, class1, mod1, type1) = { description = None; skill = skill; damageType = type1; damageRoll = None; damageClass = class1; damageMod = mod1 }
    static member create(skill, descr, class1, mod1, type1) = { description = Some descr; skill = skill; damageType = type1; damageRoll = None; damageClass = class1; damageMod = mod1 }
    member this.compute st =
        (this.damageRoll
            |> Option.defaultValue (
                (match this.damageClass with Swing -> sw st | Thrust -> thr st).plus
                    this.damageMod))
        |> fun roll -> roll, this.damageType
and CreatureStats = {
    st: int
    dx: int
    iq: int
    ht: int
    hp: int
    fp: int
    dr: Location -> int
    mods: Mod list
    readiedWeapon: ReadiedWeapon
    }
    with
    member this.damage =
        let strikingST = match this.mods |> List.tryPick (function StrikingST v -> Some v | _ -> None) with Some v -> this.st + v | _ -> this.st
        this.readiedWeapon.compute strikingST

type CreatureStatus = {
    stats: CreatureStats
    status: Status
    }
type Creature = { name: string; id: Id; originalStats: CreatureStats; mutable status: CreatureStatus; mutable roundInfo: RoundInfo }
type World(map, log, ?silent) =
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
    member this.remember (msg: string) =
        log <- msg :: log
        if (defaultArg silent false |> not) then
            printfn "%s" msg
    member this.getDenizens() = denizens
    member this.add(name, stats) = addCreature(name, stats)
    member this.add(name, ?st, ?dx, ?iq, ?ht, ?hp, ?fp, ?dr, ?mods, ?attackSkill, ?readiedWeapon, ?damage, ?damageType) =
        let either = defaultArg
        let st = either st 10
        let dx = either dx 10
        let stats: CreatureStats = {
            st = st
            dx = dx
            iq = either iq 10
            ht = either ht 10
            hp = either hp st
            fp = either fp 10
            dr = either dr (function Skull -> 2 | _ -> 0)
            readiedWeapon = either readiedWeapon (ReadiedWeapon.create((fun stats -> stats.dx), either damage (d6 1), either damageType Crushing))
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
let checkCondition (c: Creature) cond = c.status.status |> List.contains cond
let removeCondition cond = updateStatus (List.filter ((<>)cond))
let kill (target: Creature) =
    target |> addCondition Dead |> updateStats (fun stats -> { stats with hp = target.originalStats.hp * -5 })

type Outcome = CritSuccess | Success | Fail | CritFail
module Actions =
    let eval skill roll =
        match roll with
        | 3 | 4 when skill >= 3 -> CritSuccess, max 0 (skill - roll)
        | 5 | 6 as roll when skill - 10 >= roll -> CritSuccess, max 0 (skill - roll)
        | 17 -> (if skill >= 16 then Fail else CritFail), max 0 (roll - skill)
        | 18 -> CritFail, max 0 (roll - skill)
        | roll when roll > skill -> (if roll - 10 >= skill then CritFail else Fail), max 0 (roll - skill)
        | roll -> Success, max 0 (skill - roll)
    let attempt2 skill = eval skill (d6 3).throw
    let attempt skill = attempt2 skill |> fst
    let loggedAttempt2 name activity skill =
        let v = (d6 3).throw
        let result, margin = eval skill v
        world.remember $"{name} needs {skill} to {activity}, rolls {v}: {result}"
        result, margin
    let loggedAttempt name activity skill = loggedAttempt2 name activity skill |> fst
    let opposedRoll (aggressorName, aggressorSkill as aggressor) (defenderName, defenderSkill as defender) =
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
    let damage (creature: Creature) location injury isCrippling =
        let current = creature.status.stats
        let new1 = { current with hp = current.hp - injury }
        creature.status <- { creature.status with stats = new1 }
        let HP = creature.originalStats.hp
        if new1.hp <= HP * -5 then
            world.remember $"{creature.id} drops dead! [HP = {new1.hp}]"
            creature |> addCondition Dead |> ignore
        else
            let addPenalties() =
                // shock, knockdown
                creature.roundInfo <- { creature.roundInfo with shockPenalty = creature.roundInfo.shockPenalty - injury |> max -4 }
                let majorWound = injury >= creature.originalStats.hp / 2
                let knockdownCheck penalty =
                    match loggedAttempt2 creature.id "resist knockdown" (creature.status.stats.ht + penalty) with
                    | (Success | CritSuccess), _ ->
                        ()
                    | Fail, margin when margin < 5 ->
                        addCondition PhysicalStun creature |> ignore
                        addCondition Prone creature |> ignore
                        world.remember $"{creature.id} falls down, stunned"
                    | (Fail | CritFail), _ ->
                        addCondition Unconscious creature |> ignore
                        addCondition Prone creature |> ignore
                        world.remember $"{creature.id} falls down, unconscious"

                match location with
                | Eye | Skull -> knockdownCheck (if majorWound then -10 else 0)
                | Vitals | Face when majorWound -> knockdownCheck -5
                | _ when majorWound -> knockdownCheck 0
                | _ -> if isCrippling then knockdownCheck 0
            let rec deathCheck numberOfChecks =
                match loggedAttempt creature.id "not die" new1.ht with
                | Fail | CritFail ->
                    world.remember $"{creature.id} dies. [HP = {new1.hp}, HT = {new1.ht}]"
                    creature |> addCondition Dead |> ignore
                | CritSuccess | Success ->
                    if numberOfChecks = 1 then
                        world.remember $"{creature.id} is too tough to die yet. [HP = {new1.hp}, HT = {new1.ht}]"
                        addPenalties()
                    else
                        deathCheck (numberOfChecks - 1)

            let deathThresholds stats = -(stats.hp / HP) |> max 0
            if deathThresholds new1 > 0 then
                deathCheck (deathThresholds new1 - deathThresholds current)
            else
                addPenalties()

    let hit isCrit src (target: Creature) location (weapon:ReadiedWeapon) =
        let strikingST =
            src.status.stats.mods
            |> List.tryPick (function StrikingST n -> Some n | _ -> None)
            |> Option.defaultValue 0
        let amount, type1 = weapon.compute(src.status.stats.st + strikingST)
        let basicDamage = amount.throw
        let dr = target.status.stats.dr location
        let dmg = basicDamage - dr
        let mult = woundingMultiplier(location, type1)
        let injury = ((float dmg) * mult |> int)
        let HP = target.originalStats.hp
        let cappedInjury, isCrippling =
            match location with
            | Arm | Leg when injury > HP / 2 -> HP / 2, true
            | Hand | Foot when injury > HP / 3 -> HP / 3, true
            | _ -> injury, false
        let dmgDescr =
            if dr > 0 then $"{dmg}({basicDamage}-{dr})" else $"{basicDamage}"
        let capDescr = (if injury <> cappedInjury then $", capped at {cappedInjury}" else "")
        let verb = if isCrit then "crits" else "hits"
        world.remember $"{src.id} {verb} {target.id} for {cappedInjury} {location} damage! [{dmgDescr} {type1} x {mult}{capDescr}]"
        damage target location cappedInjury isCrippling
    let defend src target =
        let t = target.status.stats
        let hasMod c = t.mods |> List.contains c
        let cr = if hasMod CombatReflexes then +1 else 0
        let retreat = defaultArg target.roundInfo.retreatedFrom src.id = src.id
        let checkCondition = checkCondition target
        if checkCondition Unconscious || checkCondition Dead then false
        else
            let penalties =
                if checkCondition MentalStun || checkCondition PhysicalStun then -4 else 0
                + (if checkCondition Prone then -3 else 0)
            let adjustDodge dodge = if (float t.hp < float target.originalStats.hp / 3.) then dodge / 2 else dodge
            let parry = (t.readiedWeapon.skill t / 2) + 3 + cr + if retreat then +1 else 0 + (target.roundInfo.parries * -4) + penalties
            let dodge = (((t.ht + t.dx)/4) + 3 + cr |> adjustDodge) + if retreat then +3 else 0 + penalties
            if retreat then
                world.remember $"{target.id} retreats!"
                target.roundInfo <- { target.roundInfo with retreatedFrom = Some src.id }
            if parry > dodge then
                target.roundInfo <- { target.roundInfo with parries = target.roundInfo.parries + 1 }
                match loggedAttempt target.id "parry" parry with
                | Success | CritSuccess ->
                    world.remember $"{target.id} parries successfully!"
                    true
                | Fail | CritFail ->
                    world.remember $"{target.id} tries to parry but misses!"
                    false
            else
                match loggedAttempt target.id "dodge" dodge with
                | Success | CritSuccess ->
                    world.remember $"{target.id} dodges successfully!"
                    true
                | Fail | CritFail ->
                    world.remember $"{target.id} tries to dodge but misses!"
                    false
    let miss src target weapon =
        let with1 = match weapon.description with Some descr -> $" with {descr}" | None -> ""
        world.remember $"{src.id} missed {target.id}{with1}!"
    let attack (src:string) (target:string) (location:Location option) =
        let src = world[src]
        let target = world[target]
        if src.status.status |> List.exists (function Dead | MentalStun | PhysicalStun | Unconscious -> true | _ -> false) then
            failwith $"Sorry, {src.id} can't attack because it's {src.status.status}"
        let weapon = src.status.stats.readiedWeapon
        let skill = (weapon.skill src.status.stats)
        let location, hitPenalty =
            match location with
            | Some location -> location, location.hitPenalty
            | None -> Location.Random(), 0 // no penalty for random targeting
        let penalty = src.roundInfo.shockPenalty + hitPenalty + (if checkCondition src Prone then -4 else 0)
        let with1 = match weapon.description with Some descr -> $" with {descr}" | None -> ""
        let penaltyDescr = if penalty > 0 then $" {penalty}" else ""
        world.remember $"{src.id} attacks {target.id}{with1} [skill {skill}{penaltyDescr}]"
        match loggedAttempt src.id "hit" (skill + penalty) with
        | CritSuccess ->
            hit true src target location weapon
        | Success ->
            if defend src target |> not then
                hit false src target location weapon
        | _ ->
            miss src target weapon

open Actions
let largeKnife skillBonus = ReadiedWeapon.create((fun stats -> stats.dx + skillBonus), "Large Knife", Swing, -2, Cutting)
for _ in 1..3 do
    world.add("Doomchild", st=8, dx=18, readiedWeapon = largeKnife 0, mods=[Berserk 12; StrikingST +10]) |> fun s -> printfn $"Created {s.id}"
let lf _ = printfn ""
attack "Doomchild" "Doomchild2" None |> lf
attack "Doomchild3" "Doomchild2" None |> lf
printfn "HP left: %d" world["Doomchild2"].status.stats.hp
world.getLog()
world["Doomchild2"].status.stats.damage

world["Doomchild2"] |> kill
world["Doomchild2"].status.status

