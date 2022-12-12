#I __SOURCE_DIRECTORY__
#load "Common.fsx"
#r "nuget: TextCopy"
open System

type Id = string
type RoundInfo = {
    retreatedFrom: Id option
    parries: int
    blocks: int
    blockingSpells: int
    hasActed: bool
    shockPenalty: int
    pendingShockPenalty: int
    }
    with static member fresh = { retreatedFrom = None; hasActed = false; parries = 0; blocks = 0; blockingSpells = 0; shockPenalty = 0; pendingShockPenalty = 0 }
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

type Side = Left | Right
[<StructuredFormatDisplay("{StringRep}")>]
type Location = Torso | Skull | Eye | Arm of Side | Leg of Side | Hand of Side | Foot of Side | Neck | Vitals | Face
    with
    member this.StringRep =
        match this with
        | Leg side -> $"{side} leg"
        | Arm side -> $"{side} arm"
        | Hand side -> $"{side} hand"
        | Foot side -> $"{side} foot"
        | Eye -> "eye"
        | Torso -> "body"
        | Neck -> "neck"
        | Vitals -> "vitals"
        | Face -> "face"
        | Skull -> "skull"
    static member Random() =
        match (d6 3).throw with
        | 3 | 4 -> Skull
        | 5 -> Face
        | 6 | 7 -> Leg Right // right
        | 8 -> Arm Right // right
        | 9 | 10 | 11 -> Torso
        | 12 -> Arm Left // left
        | 13 | 14 -> Leg Left // left
        | 15 -> Hand ([Left; Right][rand.Next 2])
        | 16 -> Foot ([Left; Right][rand.Next 2])
        | 17 | 18 -> Neck
        | _ -> shouldntHappen()
    member this.hitPenalty =
        match this with
        | Eye -> -9
        | Skull -> -7
        | Hand _ | Foot _ -> -4
        | Vitals -> -3
        | Arm _ | Leg _ -> -2
        | Face | Neck -> -5
        | Torso -> 0
type Condition = MentalStun | PhysicalStun | Unconscious | Dead | Prone | Lost of Location // prone is technically a posture not a condition, but we'll leave posture as TODO for now
type Status = Condition list
module Damage =
    type DamageType = Cutting | Impaling | Crushing | Piercing | Burning
    let woundingMultiplier = function
        | (Skull | Eye), _ -> 4.
        | Neck, Cutting -> 2.
        | _, Cutting -> 1.5
        | Neck, Crushing -> 1.5
        | Vitals, (Piercing | Impaling) -> 3.
        | (Arm _ | Leg _ | Hand _ | Foot _), Impaling -> 1.
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
    | ExtraAttack of int
    | HighPainThreshold
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
    team: string
    st: int
    dx: int
    iq: int
    ht: int
    hp: int
    fp: int
    speed: float
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
type Creature = { name: string; id: Id; originalStats: CreatureStats; mutable current: CreatureStatus; mutable roundInfo: RoundInfo }
let largeKnife skillBonus = ReadiedWeapon.create((fun stats -> stats.dx + skillBonus), "large knife", Swing, -2, Cutting)
let unarmedStrike skillBonus = ReadiedWeapon.create((fun stats -> stats.dx + skillBonus), "unarmed strike", Thrust, -1, Crushing)
let duelingGlaive skillBonus = ReadiedWeapon.create((fun stats -> stats.dx + skillBonus), "dueling glaive", Swing, +2, Cutting)
let updateStats f (c:Creature) =
    c.current <- { c.current with stats = c.current.stats |> f }
    c
let updateStatus f (c:Creature) =
    c.current <- { c.current with status = c.current.status |> f }
    c
let addCondition cond c =
    match cond with
    | Lost (Arm Right | Hand Right) -> c |> updateStats (fun stats -> { stats with readiedWeapon = unarmedStrike 0 } )
    | _ -> c
    |> updateStatus (List.append [cond])
let checkCondition cond (c: Creature) = c.current.status |> List.contains cond
let checkConditions conditions (c: Creature) = conditions |> List.exists (flip List.contains c.current.status)
let checkMod mod1 (c: Creature) = c.current.stats.mods |> List.contains mod1
let checkModf predicate (c: Creature) = c.current.stats.mods |> List.exists predicate
let checkMods mods (c: Creature) = mods |> List.exists (flip List.contains c.current.stats.mods )
let removeCondition cond = updateStatus (List.filter ((<>)cond))

type World(map, log, ?silent) =
    let mutable denizens: Map<Id, Creature> = map
    let mutable log : string list = log
    let remember1 msg =
        log <- msg :: log
        if (defaultArg silent false |> not) then
            printfn "%s" msg
    let addCreature(name, stats) =
        let rec getId candidate counter =
            if denizens.ContainsKey candidate then getId (sprintf "%s%d" name counter) (counter + 1)
            else candidate
        let id = getId name 2
        let creature = { name = name; id = id; originalStats = stats; current = { stats = stats; status = Ok }; roundInfo = RoundInfo.fresh }
        denizens <- denizens |> Map.add id creature
        remember1 $"{id} has entered the fray on team {stats.team}."
        denizens[id]
    member this.Item with get id = denizens[id]
    member this.getLog() = log
    member this.remember (msg: string) =
        remember1 msg
    member this.getDenizens() = denizens
    member this.getDenizen id = denizens |> Map.tryFind id
    member this.clearAll() =
        denizens <- Map.empty
        log <- []
    member this.clearDeadOrUnconscious() =
        denizens <- denizens |> Map.filter (fun k v -> v |> checkConditions [Dead; Unconscious] |> not)
    member this.add(name, stats) = addCreature(name, stats)
    member this.add(name, ?team, ?st, ?dx, ?iq, ?ht, ?hp, ?fp, ?speed, ?dr, ?mods, ?attackSkill, ?readiedWeapon, ?damage, ?damageType) =
        let either = defaultArg
        let st = either st 10
        let dx = either dx 10
        let ht = either ht 10
        let stats: CreatureStats = {
            team = either team (System.Guid.NewGuid().ToString())
            st = st
            dx = dx
            iq = either iq 10
            ht = ht
            hp = either hp st
            fp = either fp 10
            speed = either speed ((float dx + float ht)/4.)
            dr = either dr (function Skull -> 2 | _ -> 0)
            readiedWeapon = either readiedWeapon (ReadiedWeapon.create((fun stats -> stats.dx), either damage (d6 1), either damageType Crushing))
            mods = either mods []
            }
        addCreature(name, stats)
let world = World(Map.empty, [])

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
    let loggedAttempt2 (name: string) activity skill =
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
        let current = creature.current.stats
        let new1 = { current with hp = current.hp - injury }
        creature.current <- { creature.current with stats = new1 }
        let HP = creature.originalStats.hp
        if new1.hp <= HP * -5 then
            world.remember $"{creature.id} drops dead! [HP = {new1.hp}]"
            creature |> addCondition Dead |> ignore
        else
            let addPenalties() =
                // shock, knockdown
                creature.roundInfo <- { creature.roundInfo with pendingShockPenalty = creature.roundInfo.pendingShockPenalty - injury |> max -4 }
                let majorWound = injury >= creature.originalStats.hp / 2
                let knockdownCheck penalty =
                    match loggedAttempt2 creature.id "resist knockdown" (creature.current.stats.ht + penalty + (if creature |> checkMod HighPainThreshold then +3 else 0)) with
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
                if numberOfChecks > 0 then
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
            src.current.stats.mods
            |> List.tryPick (function StrikingST n -> Some n | _ -> None)
            |> Option.defaultValue 0
        let amount, type1 = weapon.compute(src.current.stats.st + strikingST)
        let basicDamage = amount.throw
        let dr = target.current.stats.dr location
        let dmg = basicDamage - dr |> max 0
        let mult = woundingMultiplier(location, type1)
        let injury = ((float dmg) * mult |> int)
        let HP = target.originalStats.hp
        let cappedInjury, cripples =
            match location with
            | Arm _ | Leg _ when injury > HP / 2 ->
                addCondition (Lost location) target |> ignore
                HP / 2, Some location
            | Hand _ | Foot _ when injury > HP / 3 ->
                addCondition (Lost location) target |> ignore
                HP / 3, Some location
            | _ -> injury, None
        let dmgDescr =
            if dr > 0 then $"{dmg} ({basicDamage}-{dr})" else $"{basicDamage}"
        let capDescr = (if injury <> cappedInjury then $", capped at {cappedInjury}" else "")
        let verb = if isCrit then "crits" else "hits"
        let crippleDescr = if cripples.IsSome then ", crippling it" else ""
        world.remember $"{src.id} {verb} {target.id} for {cappedInjury} {location} damage{crippleDescr}! [{dmgDescr} {type1} x {mult}{capDescr}]"
        damage target location cappedInjury cripples.IsSome
    let defend src target penalty =
        let t = target.current.stats
        let hasMod c = t.mods |> List.contains c
        let cr = if hasMod CombatReflexes then +1 else 0
        let retreat = defaultArg target.roundInfo.retreatedFrom src.id = src.id
        let checkCondition cond = target |> checkCondition cond
        if checkCondition Unconscious || checkCondition Dead then false
        else
            let penalties =
                if checkCondition MentalStun || checkCondition PhysicalStun then -4 else 0
                + (if checkCondition Prone then -3 else 0)
                + penalty

            let adjustDodge dodge = if (float t.hp < float target.originalStats.hp / 3.) then dodge / 2 else dodge
            let parry = penalties + (t.readiedWeapon.skill t / 2) + 3 + cr + (if retreat then +1 else 0) + (target.roundInfo.parries * -4)
            let dodge = penalties + (((int t.speed)) + 3 + cr |> adjustDodge) + (if retreat then +3 else 0)
            if retreat then
                world.remember $"{target.id} retreats!"
                target.roundInfo <- { target.roundInfo with retreatedFrom = Some src.id }
            if parry > dodge then
                target.roundInfo <- { target.roundInfo with parries = target.roundInfo.parries + 1 }
                match loggedAttempt target.id "parry" parry with
                | Success | CritSuccess ->
                    true
                | Fail | CritFail ->
                    false
            else
                match loggedAttempt target.id "dodge" dodge with
                | Success | CritSuccess ->
                    true
                | Fail | CritFail ->
                    false
    let miss src target weapon =
        let with1 = match weapon.description with Some descr -> $" with {descr}" | None -> ""
        world.remember $"{src.id} missed {target.id}{with1}!"
    let startTurn (src: string) =
        let src = world[src]
        if src |> checkCondition Unconscious |> not && src.current.stats.hp < 0 then
            match loggedAttempt src.id "stay conscious" src.current.stats.ht with
            | CritSuccess | Success -> ()
            | Fail | CritFail ->
                src |> addCondition Unconscious |> ignore
                world.remember $"{src.id} falls unconscious"
        src.roundInfo <- { RoundInfo.fresh with shockPenalty = src.roundInfo.pendingShockPenalty } // don't clear shock penalty until end of turn
    let endTurn (src: string) =
        let src = world[src]
        if src |> checkCondition PhysicalStun then
            match loggedAttempt src.id "recover from physical stun" src.current.stats.ht with
            | CritSuccess | Success -> src |> removeCondition PhysicalStun |> ignore
            | Fail | CritFail -> ()
    let attack (src:string) (target:string) (deceptive: int) (location:Location option) =
        let src = world[src]
        let target = world[target]
        if src.current.status |> List.exists (function Dead | MentalStun | PhysicalStun | Unconscious -> true | _ -> false) then
            failwith $"Sorry, {src.id} can't attack because it's {src.current.status}"
        let weapon = src.current.stats.readiedWeapon
        let skill = (weapon.skill src.current.stats)
        let locDescr = match location with Some loc -> $" {loc}" | None -> ""
        let location, hitPenalty =
            match location with
            | Some location -> location, location.hitPenalty
            | None ->
                Location.Random(), 0 // no penalty for random targeting
        let location =
            if target |> checkCondition (Lost location) then Torso else location
        let penalty = (if checkMod HighPainThreshold src then 0 else src.roundInfo.shockPenalty)
                        + hitPenalty + (if src |> checkCondition Prone then -4 else 0) +
                        (if src.current.status |> List.exists (function Lost(Leg _) -> true | _ -> false) then -6
                         elif src.current.status |> List.exists (function Lost(Foot _) -> true | _ -> false) then -3
                         else 0)
                         + (-2 * deceptive)
        let with1 = match weapon.description with Some descr -> $" with {descr}" | None -> ""
        let penaltyDescr = if penalty > 0 then $" {penalty}" else ""
        world.remember $"{src.id} attacks {target.id}{locDescr}{with1} [skill {skill}{penaltyDescr}]"
        match loggedAttempt src.id "hit" (skill + penalty) with
        | CritSuccess ->
            hit true src target location weapon
        | Success ->
            if defend src target (-1 * deceptive) |> not then
                hit false src target location weapon
        | _ ->
            miss src target weapon

open Actions
let lf _ = printfn ""
let isActive c = c |> checkConditions [Dead; Unconscious] |> not
let printWorld() =
    for KeyValue(k,v) in world.getDenizens() |> List.ofSeq do
        let activeFlag = if isActive v then "" else "-"
        let status = if v.current.status = Ok then "OK" else String.Join(", ", v.current.status |> List.map (sprintf "%A"))
        world.remember $"{activeFlag}{k} [{v.current.stats.team}]: HP {v.current.stats.hp}/{v.originalStats.hp}, {status}"
    world.remember  "\n"
let newRound roundNumber =
    sprintf "\n=====================\nRound %d" roundNumber |> world.remember
    printWorld()
let doRound() =
    for KeyValue(id,src) in world.getDenizens() |> List.ofSeq |> List.sortByDescending (function KeyValue(k,v) -> v.current.stats.speed) do
        if src |> isActive then
            if src |> checkCondition PhysicalStun then
                world.remember $"{id} is stunned and does nothing"
            else
                startTurn src.id
                if src |> isActive then
                    for _ in 1..(src.current.stats.mods |> Seq.tryPick (function ExtraAttack n -> n + 1 |> Some | _ -> None) |> Option.defaultValue 1) do
                        let team = src.current.stats.team
                        match world.getDenizens().Values |> Seq.filter (fun target -> isActive target && target.current.stats.team <> team) |> List.ofSeq with
                        | [] -> () // no opponents: victory!
                        | targets ->

                            if src.current.stats.team = "red" then
                                // if any targets still have arms, attack them first
                                let lostHand = checkConditions [Lost (Arm Right); Lost (Hand Right)]
                                match targets |> List.sortByDescending (fun t -> t |> lostHand |> not, t.current.stats.hp) with
                                | target::_ ->
                                    // barbarian only attacks torso
                                    if target |> lostHand then
                                        attack src.id target.id 4 (Some Torso)
                                    else
                                        attack src.id target.id 3 (Some (Arm Right))
                                | _ -> shouldntHappen()
                            else
                                attack src.id targets.Head.id 0 None
                            world.remember ""
            src.id |> endTurn
let fightUntilVictory() =
    let mutable round = 1
    while (world.getDenizens().Values |> Seq.filter isActive |> Seq.groupBy (fun c -> c.current.stats.team) |> Seq.length) > 1 do
        newRound round
        doRound()
        round <- round + 1
    world.remember "\nFinal results:"
    printWorld()
    world.clearDeadOrUnconscious()
world.clearAll()
for _ in 1..3 do
    world.add("Doomchild", team="blue", st=8, dx=18, speed = 7, readiedWeapon = largeKnife 0, mods=[Berserk 12; StrikingST +10]) |> lf
world.add("Barbarian", team="red", st=17, dx=13, ht=13, hp=22, speed = 6, readiedWeapon = duelingGlaive +6, mods = [ExtraAttack 1; HighPainThreshold], dr = (function Eye -> 0 | Skull -> 8 | _ -> 6)) |> lf
fightUntilVictory()
String.Join("\n", world.getLog() |> List.rev) |> TextCopy.ClipboardService.SetText
