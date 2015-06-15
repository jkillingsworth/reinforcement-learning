module Chart

open OxyPlot
open OxyPlot.Axes
open OxyPlot.Series
open Bandits

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

let private describe = function
    | EpsilonGreedyAverage taskdef -> sprintf "Q1 = %.0f, ε = %.2f" taskdef.Q1 taskdef.Epsilon
    | UpperConfidenceBound taskdef -> sprintf "Q1 = %.0f, c = %.2f" taskdef.Q1 taskdef.Confidence
    | GradientAscentBandit taskdef -> sprintf "H1 = %.0f, α = %.2f" taskdef.H1 taskdef.Alpha

//-------------------------------------------------------------------------------------------------

let renderAverageReward path data =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightBottom
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis(Title = "Steps")
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0
    axis.Maximum <- (double Bandits.steps)
    axis.MajorStep <- (double Bandits.steps) / 4.0
    axis.MinorStep <- (double Bandits.steps) / 20.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis(Title = "Average Reward")
    axis.Position <- AxisPosition.Left
    axis.Minimum <- Bandits.meanActionValue
    axis.Maximum <- Bandits.meanActionValue + 1.6
    axis.MajorStep <- 0.5
    axis.MinorStep <- 0.1
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.AxisTitleDistance <- 14.0
    model.Axes.Add(axis)

    for (taskdef, points) in data do
        let series = LineSeries(Title = describe taskdef)
        series.StrokeThickness <- 1.0
        model.Series.Add(series)
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add

    exportToPng model path 700 400

//-------------------------------------------------------------------------------------------------

let renderOptimalAction path data =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightBottom
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis(Title = "Steps")
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0
    axis.Maximum <- (double Bandits.steps)
    axis.MajorStep <- (double Bandits.steps) / 4.0
    axis.MinorStep <- (double Bandits.steps) / 20.0
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

    for (taskdef, points) in data do
        let series = LineSeries(Title = describe taskdef)
        series.StrokeThickness <- 1.0
        model.Series.Add(series)
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add

    exportToPng model path 700 400
