module Data

type Id = string
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

[<AutoOpen>]
module Scope =
    type Row = Map<Name, RuntimeValue>
    type Scope = Map<Id, Row>

type Weapon = Rapier | Dagger | Bow | Greatsword
type Trait = Lucky | WeaponMaster of Weapon | HardToKill of int
type TraitName = Lucky | WeaponMaster | HardToKill
let (|WeaponMaster|_|) = Flags.getValue TraitName.WeaponMaster (function [:? Weapon as weapon] -> Some (Trait.WeaponMaster(weapon)) | _ -> None)
let (|HardToKill|_|) = Flags.getValue HardToKill (function [:? int as n] -> Some (HardToKill(n)) | _ -> None)
let traits = [WeaponMaster, [box Rapier]; Lucky, []; HardToKill, [1]] |> Flags.createValues
match traits with
| WeaponMaster (Trait.WeaponMaster w) -> w
