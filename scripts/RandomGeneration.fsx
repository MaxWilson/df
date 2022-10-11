open System

let traits =    [   "ST", 10
                    "DX", 20
                    "IQ", 20
                    "HT", 10
                    "Rapier Wit", 5
                    "Weapon Master", 20
                    "Luck", 15
                    "HP", 2
                    ]
let traitsMap = traits
                |> Map.ofList
let r = System.Random()
let chooseRandom (lst: _ list) =
    lst[r.Next lst.Length]

let costOf trait' =
    match traitsMap |> Map.tryFind trait' with
    | Some v -> v
    | _ -> failwith $"{trait'} has no assigned cost, please add one and then retry."

let totalCost = List.map costOf >> List.sum

// use this to check whether the template is exceeded
totalCost ["ST"; "ST"; "DX"]


let validOptions budget choices =
    let cost = totalCost choices
    let remainingBudget = budget - cost
    traitsMap.Keys |> Seq.filter (fun c -> traitsMap[c] < remainingBudget) |> List.ofSeq

let construct budget =
    let rec recur choices =
        let cost = totalCost choices
        if cost >= budget then
            choices
        else
            let remainingBudget = budget - cost
            match validOptions budget choices with
            | [] -> choices
            | affordableChoices ->
                recur ((affordableChoices |> chooseRandom)::choices)
    recur []
let format choices =
    let order = traits |> List.mapi (fun ix (name, _) -> name, ix) |> Map.ofList
    choices |> List.countBy id |> List.sortBy (fst >> (fun name -> order[name]))
let prettyPrint choices = format choices |> List.iter (printfn "%A")

totalCost []
validOptions 250 []
construct 250 |> prettyPrint
