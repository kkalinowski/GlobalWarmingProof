//co2 data from ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/vostok/co2nat.txt
//temperature data from ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/vostok/deutnat.txt
module Program

open System
open FSharp.Data
open FSharp.Charting
open System.Drawing

type co2Csv = CsvProvider< "co2.csv", "\t" >

type tempCsv = CsvProvider< "temp.csv", "\t" >

let bpYear = 1950
let convertBPtoThousandsYears bp = (bpYear - bp) / 1000

let standardDevation list = 
    let avg = List.average list
    sqrt (List.fold (fun acc elem -> acc + (float elem - avg) ** 2.0) 0.0 list / float list.Length)

let computeCorrelation (first : float list) (second : float list) = 
    if first.Length <> second.Length then failwith "Collections needs to be same size"
    let firstAvg = List.average first
    let secondAvg = List.average second
    let sum = List.zip first second |> List.sumBy (fun x -> (fst x - firstAvg) * (snd x - secondAvg))
    sum / (float first.Length * standardDevation first * standardDevation second)

let getCorrelationMeaning = function
    | 0.0 -> "No correlation"
    | x when Math.Abs(x) < 0.2 -> "Very minor correlation"
    | x when Math.Abs(x) < 0.4 -> "minor correlation"
    | x when Math.Abs(x) < 0.7 -> "Medium correlation"
    | x when Math.Abs(x) < 0.9 -> "Major correlation"
    | x when Math.Abs(x) < 1.0 -> "Very major correlation"
    | _ -> "Error - correlation must be between -1 and 1"

[<EntryPoint>]
let main argv = 
    printfn "Global warming proof"
    let co2Data = co2Csv.Load("co2.csv").Rows |> Seq.skip 1
    let tempData = tempCsv.Load("temp.csv").Rows |> Seq.skip 1
    
    let co2Series = 
        co2Data
        |> Seq.map (fun x -> (convertBPtoThousandsYears x.``Gas age``, x.``CO2 (ppmv)``))
    
    let tempSeries = 
        tempData
        |> Seq.map (fun x -> (convertBPtoThousandsYears x.``Ice age (GT4)``, x.DeltaTS))
    
    let co2List = 
        co2Data
        |> Seq.map (fun x -> float x.``CO2 (ppmv)``)
        |> Seq.toList
    
    let tempList = 
        co2Data
        |> Seq.map (fun x -> x.``Gas age``)
        |> Seq.map (fun x -> tempData |> Seq.minBy (fun y -> Math.Abs(x - y.``Ice age (GT4)``)))
        |> Seq.map (fun x -> float x.DeltaTS)
        |> Seq.toList
    
    let correlation = computeCorrelation co2List tempList 
    printfn "Correlation - %f - %s" correlation (getCorrelationMeaning correlation)

    let co2Chart = Chart.Line(co2Series, "CO2 [ppm]", Color = Color.Red)
    let tempChart = Chart.Line(tempSeries, "Temperature [C]", Color = Color.Blue)
    Chart.Combine([ co2Chart; tempChart ])
    |> Chart.WithXAxis(Title = "Thousands of years")
    |> Chart.WithLegend()
    |> Chart.Show
    Console.Read() |> ignore
    0
