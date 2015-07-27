module Chart

open OxyPlot
open OxyPlot.Axes
open OxyPlot.Series

//-------------------------------------------------------------------------------------------------

let private exportToPng path w h model =

    use writeStream = System.IO.File.OpenWrite(path)
    let pngExporter = OxyPlot.WindowsForms.PngExporter()
    pngExporter.Width <- w
    pngExporter.Height <- h
    pngExporter.Export(model, writeStream)

let private defaultColorsToUseForPlots =

    [| OxyColors.Red
       OxyColors.Green
       OxyColors.Blue
       OxyColors.Orange
       OxyColors.Purple
       OxyColors.Teal |]

let private formatState = function
    | 0 -> "A"
    | 1 -> "B"
    | 2 -> "C"
    | 3 -> "D"
    | 4 -> "E"
    | _ -> failwith "Unexpected state."

//-------------------------------------------------------------------------------------------------

let renderValues path data title =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightBottom
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)
    model.Title <- title

    let axis = new LinearAxis()
    axis.Title <- "State"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0 - 0.2
    axis.Maximum <- 4.0 + 0.2
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.LabelFormatter <- (fun x -> formatState (int x))
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Title <- "Estimated Value"
    axis.Position <- AxisPosition.Left
    axis.Minimum <- 0.0
    axis.Maximum <- 0.9
    axis.MajorStep <- 0.2
    axis.MinorStep <- 0.1
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.AxisTitleDistance <- 18.0
    axis.StringFormat <- "F1"
    model.Axes.Add(axis)

    let series = LineSeries()
    series.Title <- "True"
    series.Color <- OxyColors.Gray
    series.MarkerType <- MarkerType.Circle
    series.MarkerFill <- OxyColors.Gray
    series.StrokeThickness <- 1.0
    Compute.trueValues
    |> Array.mapi (fun i x -> DataPoint(double i, x))
    |> Array.iter series.Points.Add
    model.Series.Add(series)

    for (count, points) in data do
        let series = LineSeries()
        series.Title <- sprintf "%i" count
        series.StrokeThickness <- 1.0
        series.MarkerType <- MarkerType.Circle
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add
        model.Series.Add(series)

    model |> exportToPng path 350 350

let renderErrors path data title =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightTop
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)
    model.Title <- title

    let axis = new LinearAxis()
    axis.Title <- "Episodes"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0 - double Compute.plays / 100.0
    axis.Maximum <- double Compute.plays
    axis.MajorStep <- double Compute.plays / 4.0
    axis.MinorStep <- double Compute.plays / 20.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Title <- "Root Mean Square Error"
    axis.Position <- AxisPosition.Left
    axis.Minimum <- 0.0
    axis.Maximum <- 0.25
    axis.MajorStep <- 0.05
    axis.MinorStep <- 0.025
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.AxisTitleDistance <- 12.0
    axis.StringFormat <- "F2"
    model.Axes.Add(axis)

    for (alpha, points) in data do
        let series = LineSeries()
        series.Title <- sprintf "α = %.2f" alpha
        series.StrokeThickness <- 1.0
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add
        model.Series.Add(series)

    model |> exportToPng path 350 350
