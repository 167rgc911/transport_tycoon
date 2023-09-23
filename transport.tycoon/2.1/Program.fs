module Program

open System.IO

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

type Waypoint = { Location:string; Route:string list; 
Distance:int }

type Connection = { From:string; To:string; Distance:int }

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "resources", 
    "data.csv")
    |> File.ReadAllText
    |> fun data -> data.Split(System.Environment.NewLine)
    |> Array.skip 1

let readConnections (rows:array<string>) = [
    for row in rows do
        match row.Split(",") with
        | [|start; finish; distance|] -> 
            {   From = start;
                To = finish;
                Distance = int distance }
            {   From = finish;
                To = start;
                Distance = int distance }
        | _ -> ()
]

let getChildren connections wayPoint =
    connections
    |> List.filter (fun cn ->
        cn.From = wayPoint.Location && wayPoint.Route
            |> List.tryFind (fun loc -> loc = cn.To) = None)
    |> List.map
        (fun cn -> { Location = cn.To;
            Route = cn.From :: wayPoint.Route; 
            Distance = cn.Distance + wayPoint.Distance })

let rec treeToList tree =
    match tree with 
    | Leaf x -> [x]
    | Branch (x, xs) -> x :: (List.collect treeToList 
    (xs |> Seq.toList))

let findRoutes getChildRoutes start finish =
    let rec createTree findChildRoutes destination current =
        let childRoutes = findChildRoutes current
        if childRoutes |> List.isEmpty |> not && 
        current.Location <> destination then
            Branch (current, seq {
                for next in childRoutes do
                yield (createTree getChildRoutes destination next)
            })
        else Leaf current
    createTree getChildRoutes finish { Location = start; 
    Route = []; Distance = 0}
    |> treeToList

let selectShortest finish routes =
    routes
    |> List.filter (fun wp -> wp.Location = finish)
    |> List.minBy (fun wp -> wp.Distance)
    |> fun wp -> wp.Location :: wp.Route |> List.rev, 
    wp.Distance

let run start finish =
    findRoutes (lines |> readConnections |> getChildren) start finish
    |> selectShortest finish
    |> printfn "%A"

run "Cogburg" "Leverstorm"
