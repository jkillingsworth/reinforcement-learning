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

//-------------------------------------------------------------------------------------------------

let renderChart path track traces =

    let rows = track |> Array2D.length2
    let cols = track |> Array2D.length1

    let mapping x y = function
        | Track.Cell.Start -> 1.0
        | Track.Cell.Track -> 2.0
        | Track.Cell.Final -> 3.0
        | Track.Cell.Crash -> 0.0

    let data = track |> Array2D.mapi mapping

    let model = PlotModel()

    model.Padding <- OxyThickness(0.0, 0.0, 1.0, 1.0)
    model.PlotType <- PlotType.Cartesian

    let colorStart = OxyColor.FromArgb(127uy, 000uy, 095uy, 000uy)
    let colorTrack = OxyColor.FromArgb(127uy, 063uy, 063uy, 063uy)
    let colorFinal = OxyColor.FromArgb(127uy, 127uy, 000uy, 000uy)

    let axis = LinearColorAxis()
    axis.Position <- AxisPosition.None
    axis.Palette <- OxyPalette(OxyColors.Transparent, colorStart, colorTrack, colorFinal)
    model.Axes.Add(axis)

    let axis = new LinearAxis()
    axis.Position <- AxisPosition.Bottom
    axis.IsAxisVisible <- true
    axis.Minimum <- 0.0
    axis.Maximum <- double cols
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
    series.Color <- OxyColors.Yellow
    series.StrokeThickness <- 2.0
    traces
    |> List.mapi (fun i ((px, py, _, _), a, r) -> DataPoint(0.5 + double px, 0.5 + double py))
    |> List.iter series.Points.Add
    model.Series.Add(series)

    let folder items (s, a, r) =
        let px, py, vx, vy = s
        let points, r' = items
        let points = if r' = Compute.rewardCrash then (px, py) :: points else points
        points, r

    for point in traces |> List.fold folder ([], 0) |> fst do
        let px, py = point
        let annotation = OxyPlot.Annotations.PointAnnotation()
        annotation.X <- double px + 0.5
        annotation.Y <- double py + 0.5
        annotation.Shape <- MarkerType.Star
        annotation.StrokeThickness <- 1.0
        annotation.Stroke <- OxyColors.Yellow
        model.Annotations.Add(annotation)

    let value = traces |> List.map (fun (s, a, r) -> r) |> List.sum
    let annotation = OxyPlot.Annotations.TextAnnotation()
    annotation.Background <- OxyColors.White
    annotation.Text <- sprintf "Return: %i" value
    annotation.TextColor <- OxyColors.Black
    annotation.TextPosition <- DataPoint(double (Array2D.length1 data) - 1.0, 1.0)
    annotation.TextVerticalAlignment <- VerticalAlignment.Bottom
    annotation.TextHorizontalAlignment <- HorizontalAlignment.Right
    annotation.Stroke <- OxyColors.Gray
    annotation.StrokeThickness <- 1.0
    model.Annotations.Add(annotation)

    let scale = 15
    model |> exportToPng path (cols * scale) (rows * scale)
