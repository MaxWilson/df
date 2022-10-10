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
                |> Map.ofList
let r = System.Random()
let chooseRandom (lst: _ list) =
    lst[r.Next lst.Length]

let costOf trait' =
    match traits |> Map.tryFind trait' with
    | Some v -> v
    | _ -> failwith $"{trait'} has no assigned cost, please add one and then retry."

let totalCost = List.map costOf >> List.sum

// use this to check whether the template is exceeded
totalCost ["ST"; "ST"; "DX"]


let validOptions budget choices =
    let cost = totalCost choices
    let remainingBudget = budget - cost
    traits.Keys |> Seq.filter (fun c -> traits[c] < remainingBudget) |> List.ofSeq

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
    recur [] |> List.countBy id |> List.sortBy fst

totalCost []
validOptions 250 []
construct 250