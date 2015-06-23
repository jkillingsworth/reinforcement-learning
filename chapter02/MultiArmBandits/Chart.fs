module Chart

open OxyPlot
open OxyPlot.Axes
open OxyPlot.Series
open Compute

//-------------------------------------------------------------------------------------------------

let private defaultColorsToUseForPlots =

    [| OxyColors.Red
       OxyColors.Green
       OxyColors.Blue
       OxyColors.Orange
       OxyColors.Purple
       OxyColors.Teal |]

let private formatAlpha = function
    | Constant alpha -> sprintf "%.2f" alpha
    | OneOverK -> "1/k"

let private formatTitle = function
    | EpsilonGreedyProcess x -> sprintf "Q1 = %.0f, α = %s, ε = %.2f" x.Q1 (formatAlpha x.Alpha) x.Epsilon
    | UpperConfidenceBound x -> sprintf "Q1 = %.0f, α = %s, c = %.2f" x.Q1 (formatAlpha x.Alpha) x.Confidence
    | GradientAscentBandit x -> sprintf "H1 = %.0f, α = %s, baseline = %b" x.H1 (formatAlpha x.Alpha) x.Baseline

let private renderChart data configureAxisY =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightBottom
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis()
    axis.Title <- "Steps"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0 - double Compute.steps / 100.0
    axis.Maximum <- double Compute.steps
    axis.MajorStep <- double Compute.steps / 4.0
    axis.MinorStep <- double Compute.steps / 20.0
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
    configureAxisY axis

    for (taskdef, points) in data do
        let series = LineSeries()
        series.Title <- formatTitle taskdef
        series.StrokeThickness <- 1.0
        points
        |> Array.mapi (fun i x -> DataPoint(double i, x))
        |> Array.iter series.Points.Add
        model.Series.Add(series)

    model

let private exportToPng path w h model =

    use writeStream = System.IO.File.OpenWrite(path)
    let pngExporter = OxyPlot.WindowsForms.PngExporter()
    pngExporter.Width <- w
    pngExporter.Height <- h
    pngExporter.Export(model, writeStream)

//-------------------------------------------------------------------------------------------------

let renderAverageReward path data =

    let configureAxisY (axis : LinearAxis) =
        axis.Title <- "Average Reward"
        axis.Minimum <- Compute.meanActionValue - 0.1
        axis.Maximum <- Compute.meanActionValue + 1.6
        axis.MajorStep <- 0.5
        axis.MinorStep <- 0.1
        axis.AxisTitleDistance <- 22.0
        axis.StringFormat <- "F1"

    configureAxisY
    |> renderChart data
    |> exportToPng path 700 400

let renderOptimalAction path data =

    let configureAxisY (axis : LinearAxis) =
        axis.Title <- "Optimal Action"
        axis.Minimum <- 0.0
        axis.Maximum <- 1.0
        axis.MajorStep <- 0.2
        axis.MinorStep <- 0.1
        axis.AxisTitleDistance <- 4.0
        axis.StringFormat <- "P0"

    configureAxisY
    |> renderChart data
    |> exportToPng path 700 400
