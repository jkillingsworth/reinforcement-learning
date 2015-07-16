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

let renderChart path data =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightTop
    model.PlotMargins <- OxyThickness(nan, nan, 20.0, nan)

    let axis = new LogarithmicAxis()
    axis.Title <- "Episodes"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 1.0 - 0.1
    axis.Maximum <- double Compute.steps
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Title <- "Mean Square Error"
    axis.Position <- AxisPosition.Left
    axis.Minimum <- -0.1
    axis.Maximum <- +4.1
    axis.MajorStep <- 1.0
    axis.MinorStep <- 0.2
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.AxisTitleDistance <- 10.0
    model.Axes.Add(axis)

    for (title, points) in data do
        let series = LineSeries()
        series.Title <- title
        series.StrokeThickness <- 1.0
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add
        model.Series.Add(series)

    model |> exportToPng path 700 400
