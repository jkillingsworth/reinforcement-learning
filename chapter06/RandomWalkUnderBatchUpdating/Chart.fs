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

//-------------------------------------------------------------------------------------------------

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

    for (title, points) in data do
        let series = LineSeries()
        series.Title <- title
        series.StrokeThickness <- 1.0
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add
        model.Series.Add(series)

    model |> exportToPng path 350 350
