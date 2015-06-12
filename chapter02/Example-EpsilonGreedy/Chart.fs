module Chart

open OxyPlot
open OxyPlot.Axes
open OxyPlot.Series

//-------------------------------------------------------------------------------------------------

let private defaultColorsToUseForPlots =

    [| OxyColors.Red
       OxyColors.Green
       OxyColors.Blue
       OxyColors.Orange
       OxyColors.Purple
       OxyColors.Teal |]

let private exportToPng model path w h =

    use writeStream = System.IO.File.OpenWrite(path)
    let pngExporter = OxyPlot.WindowsForms.PngExporter()
    pngExporter.Width <- w
    pngExporter.Height <- h
    pngExporter.Export(model, writeStream)

//-------------------------------------------------------------------------------------------------

let renderAverageReward path data =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Black
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightBottom
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis(Title = "Steps")
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0
    axis.Maximum <- 1000.0
    axis.MajorStep <- 250.0
    axis.MinorStep <- 50.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis(Title = "Average Reward")
    axis.Position <- AxisPosition.Left
    axis.Minimum <- 0.0
    axis.Maximum <- 1.5
    axis.MajorStep <- 0.5
    axis.MinorStep <- 0.1
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.AxisTitleDistance <- 14.0
    model.Axes.Add(axis)

    for item in data do
        let series = LineSeries(Title = sprintf "ε = %.2f" (fst item))
        series.StrokeThickness <- 1.0
        model.Series.Add(series)
        snd item
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add

    exportToPng model path 700 400

//-------------------------------------------------------------------------------------------------

let renderOptimalAction path data =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Black
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightBottom
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis(Title = "Steps")
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0
    axis.Maximum <- 1000.0
    axis.MajorStep <- 250.0
    axis.MinorStep <- 50.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis(Title = "Optimal Action")
    axis.Position <- AxisPosition.Left
    axis.Minimum <- 0.0
    axis.Maximum <- 1.0
    axis.MajorStep <- 0.2
    axis.MinorStep <- 0.1
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.StringFormat <- "P0"
    axis.AxisTitleDistance <- 14.0
    model.Axes.Add(axis)

    for item in data do
        let series = LineSeries(Title = sprintf "ε = %.2f" (fst item))
        series.StrokeThickness <- 1.0
        model.Series.Add(series)
        snd item
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add

    exportToPng model path 700 400
