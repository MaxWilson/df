#if INTERACTIVE
#load "common.fsx"
#else
module Data
#endif

type Id = int
type Name = string
[<AutoOpen>]
module Flags =
    type Flags = Flags of Map<string, obj list>
    let str x = x.ToString()
    let inline check (v:'t) (Flags impl) =
        impl |> Map.containsKey (str v)
    let inline getValue (v:'t) pred (Flags impl) =
        printfn "Checking for %A" v
        match impl |> Map.tryFind (str v) with
        | Some values -> pred(values)
        | _ -> None
    let inline remove (v:'t) (Flags impl) =
        impl |> Map.remove (str v) |> Flags
    let inline impl (v:'t) (Flags impl) =
        impl |> Map.add (str v) [] |> Flags
    let inline implValue (v:'t) payload (Flags impl) =
        impl |> Map.add (str v) [box payload] |> Flags
    let inline implValues (v:'t) (payloads: obj list) (Flags impl) =
        impl |> Map.add (str v) payloads |> Flags
    let inline create values =
        values |> Seq.map (fun v -> str v, []) |> Map.ofSeq |> Flags
    let inline createValue values =
        values |> Seq.map (fun (v, payload) -> str v, [payload]) |> Map.ofSeq |> Flags
    let inline createValues values =
        values |> Seq.map (fun (v, payloads) -> str v, payloads) |> Map.ofSeq |> Flags
type RuntimeValue = Id of Id | Name of Name | Number of int | Flags of Flags
type 't Delayed = Requires of Id * Name | Ready of 't

[<AutoOpen>]
module Property =
    type Type<'t> = { toRuntimeValue: 't -> RuntimeValue; ofRuntimeValue: RuntimeValue -> 't }
    type Property<'t> = { name: string; typ: Type<'t> }
    let tId = { toRuntimeValue = RuntimeValue.Id; ofRuntimeValue = function RuntimeValue.Id id -> id | _ -> shouldntHappen() }
    let tName = { toRuntimeValue = RuntimeValue.Name; ofRuntimeValue = function RuntimeValue.Name id -> id | _ -> shouldntHappen() }
    let tNumber = { toRuntimeValue = RuntimeValue.Number; ofRuntimeValue = function RuntimeValue.Number n -> n | _ -> shouldntHappen() }
    let prop typ name = { name = name; typ = typ }
    let propId = prop tId
    let propName = prop tName
    let propNumber = prop tNumber
    let pId = propId "Id"
    let pName = propName "Name"

[<AutoOpen>]
module Scope =
    type Row = Map<Name, RuntimeValue>
    type Scope = Scope of Map<Id, Row>
    let fresh: Scope = Map.empty |> Scope
    let inline set (property: 't Property) id value (Scope impl) =
        let value = property.typ.toRuntimeValue value
        Scope (impl |> Map.change id (function Some row -> row |> Map.add property.name value |> Some | None -> Map.ofList [property.name, value] |> Some))
    let inline read id (property: 't Property) (Scope impl) =
        let propertyName = property.name
        match impl |> Map.tryFind id with
        | None -> Requires(id, propertyName)
        | Some row ->
            match row |> Map.tryFind propertyName with
            | None -> Requires(id, propertyName)
            | Some v -> Ready(v) // todo: allow defaults
    let inline change (property: 't Property) id f (Scope impl) =
        Scope (impl |> Map.change id (function Some row -> row |> Map.change property.name f |> Some | None -> Map.empty |> Map.change property.name f |> Some))
    let add name (Scope impl as scope) =
        let id = impl.Count + 1
        id, scope |> set pName id name |> set pId id id

let pLevel = propNumber "Level"
let bob, scope = Scope.fresh |> add "Bob"
let scope = scope |> set pLevel bob 20 |> set pLevel bob 23
scope |> read bob pLevel





type Weapon = Rapier | Dagger | Bow | Greatsword
type Trait = Lucky | WeaponMaster of Weapon | HardToKill of int
type TraitName = Lucky | WeaponMaster | HardToKill
let (|WeaponMaster|_|) = Flags.getValue TraitName.WeaponMaster (function [:? Weapon as weapon] -> Some (Trait.WeaponMaster(weapon)) | _ -> None)
let (|HardToKill|_|) = Flags.getValue HardToKill (function [:? int as n] -> Some (HardToKill(n)) | _ -> None)
let traits = [WeaponMaster, [box Rapier]; Lucky, []; HardToKill, [1]] |> Flags.createValues
match traits with
| WeaponMaster (Trait.WeaponMaster w) -> w
