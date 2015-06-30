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

let private renderChart path series configureAxisX =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis()
    axis.Title <- "Capital"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0
    axis.Maximum <- 100.0
    axis.MajorStep <- 25.0
    axis.MinorStep <- 5.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Position <- AxisPosition.Left
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)
    configureAxisX axis

    model.Series.Add(series)

    model |> exportToPng path 700 400

//-------------------------------------------------------------------------------------------------

let renderValues path data =

    let configureAxisY (axis : LinearAxis) =
        axis.Title <- "Values"
        axis.Minimum <- -0.05
        axis.Maximum <- 1.05
        axis.MajorStep <- 0.2
        axis.MinorStep <- 0.1
        axis.AxisTitleDistance <- 9.0
        axis.StringFormat <- "F1"

    let series = LineSeries()
    series.StrokeThickness <- 1.0
    data
    |> Array.mapi (fun i x -> DataPoint(double i, x))
    |> Array.iter series.Points.Add

    renderChart path series configureAxisY

let renderPolicy path data =

    let configureAxisY (axis : LinearAxis) =
        axis.Title <- "Policy"
        axis.Minimum <- -2.5
        axis.Maximum <- 52.5
        axis.MajorStep <- 10.0
        axis.MinorStep <- 5.0
        axis.AxisTitleDistance <- 12.0
        axis.StringFormat <- "F0"

    let series = ScatterSeries()
    series.MarkerType <- MarkerType.Circle
    seq { for x = 0 to Array.length data - 1 do
          for y = 0 to Array.length data.[x] - 1 do
          yield x, data.[x].[y] }
    |> Seq.map (fun (x, y) -> ScatterPoint(double x, double y, 2.0))
    |> Seq.iter series.Points.Add

    renderChart path series configureAxisY
