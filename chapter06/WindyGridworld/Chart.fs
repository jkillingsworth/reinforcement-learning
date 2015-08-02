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

let renderStats path counts =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = new LinearAxis()
    axis.Title <- "Time Steps"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- -80.0
    axis.Maximum <- 8000.0
    axis.MajorStep <- 1000.0
    axis.MinorStep <- 200.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Title <- "Episodes"
    axis.Position <- AxisPosition.Left
    axis.Minimum <- -10.0
    axis.Maximum <- 180.0
    axis.MajorStep <- 50.0
    axis.MinorStep <- 10.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.AxisTitleDistance <- 14.0
    model.Axes.Add(axis)

    let series = LineSeries()
    series.StrokeThickness <- 1.0
    counts
    |> Array.mapi (fun i x -> DataPoint(double i, double x))
    |> Array.iter series.Points.Add
    model.Series.Add(series)

    model |> exportToPng path 700 400

let renderRoute path states =

    let rows = Compute.cells |> Array2D.length2
    let cols = Compute.cells |> Array2D.length1

    let mapping x y = function
        | Compute.Goal -> -1.0
        | Compute.WS w when (x, y) = Compute.start -> -2.0
        | Compute.WS w -> double w

    let data = Compute.cells |> Array2D.mapi mapping

    let model = PlotModel()

    model.LegendPlacement <- LegendPlacement.Outside
    model.LegendPosition <- LegendPosition.LeftTop
    model.LegendTitle <- "Moves"
    model.LegendTitleFontWeight <- FontWeights.Normal
    model.Padding <- OxyThickness(2.0, 0.0, 1.0, 1.0)
    model.PlotType <- PlotType.Cartesian

    let colorStart = OxyColor.FromArgb(127uy, 127uy, 127uy, 127uy)
    let colorGoal  = OxyColor.FromArgb(127uy, 127uy, 127uy, 127uy)
    let colorWind0 = OxyColor.FromArgb(000uy, 000uy, 127uy, 255uy)
    let colorWind1 = OxyColor.FromArgb(031uy, 000uy, 127uy, 255uy)
    let colorWind2 = OxyColor.FromArgb(063uy, 000uy, 127uy, 255uy)

    let axis = LinearColorAxis()
    axis.Position <- AxisPosition.None
    axis.Palette <- OxyPalette(colorStart, colorGoal, colorWind0, colorWind1, colorWind2)
    model.Axes.Add(axis)

    let axis = new LinearAxis()
    axis.Position <- AxisPosition.Bottom
    axis.IsAxisVisible <- true
    axis.Minimum <- 0.0
    axis.Maximum <- double cols
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.MajorTickSize <- 0.0
    axis.MinorTickSize <- 0.0
    axis.StringFormat <- "''"
    axis.AxisTickToLabelDistance <- 0.0
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Position <- AxisPosition.Left
    axis.IsAxisVisible <- true
    axis.Minimum <- 0.0
    axis.Maximum <- double rows
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.MajorTickSize <- 0.0
    axis.MinorTickSize <- 0.0
    axis.StringFormat <- "''"
    axis.AxisTickToLabelDistance <- 0.0
    model.Axes.Add(axis)

    let series = HeatMapSeries()
    series.Data <- data
    series.X0 <- 0.5 + 0.0
    series.X1 <- 0.5 + double cols - 1.0
    series.Y0 <- 0.5 + 0.0
    series.Y1 <- 0.5 + double rows - 1.0
    series.Interpolate <- false
    model.Series.Add(series)

    let series = LineSeries()
    series.Title <- sprintf "%i" (List.length states - 1)
    series.Color <- OxyColors.Red
    series.StrokeThickness <- 2.0
    states
    |> List.mapi (fun i (x, y) -> DataPoint(0.5 + double x, 0.5 + double y))
    |> List.iter series.Points.Add
    model.Series.Add(series)

    let annotationPoint text point =
        let annotation = OxyPlot.Annotations.TextAnnotation()
        annotation.Text <- text
        annotation.TextPosition <- DataPoint(double (fst point) + 0.25, double (snd point) + 0.5)
        annotation.TextColor <- OxyColors.Yellow
        annotation.FontWeight <- FontWeights.Bold
        annotation.Background <- OxyColors.Transparent
        annotation.Stroke <- OxyColors.Transparent
        model.Annotations.Add(annotation)

    let pointGoal =
        seq { for x = 0 to cols - 1 do
              for y = 0 to rows - 1 do
              if Compute.cells.[x, y] = Compute.Goal then yield (x, y) }
        |> Seq.exactlyOne

    annotationPoint "S" Compute.start
    annotationPoint "G" pointGoal

    let scale = 30
    model |> exportToPng path ((cols * scale) + 70) (rows * scale)
