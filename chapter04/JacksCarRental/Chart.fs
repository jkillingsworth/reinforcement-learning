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

let private renderChart path data configureModel configureAxisColor =

    let rows = data |> Array2D.length1
    let cols = data |> Array2D.length2
    let data = Array2D.init cols rows (fun m n -> double data.[n, m])

    let model = PlotModel()

    model.PlotType <- PlotType.Cartesian
    model |> configureModel

    let axis = LinearColorAxis()
    axis.Position <- AxisPosition.Right
    axis.AxisDistance <- 8.0
    axis |> configureAxisColor
    model.Axes.Add(axis)

    let axis = new LinearAxis()
    axis.Position <- AxisPosition.Bottom
    axis.IsAxisVisible <- true
    axis.Minimum <- -0.5
    axis.Maximum <- double cols - 0.5
    axis.MajorStep <- 10.0
    axis.MinorStep <- 1.0
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Position <- AxisPosition.Left
    axis.IsAxisVisible <- true
    axis.Minimum <- -0.5
    axis.Maximum <- double rows - 0.5
    axis.MajorStep <- 10.0
    axis.MinorStep <- 1.0
    model.Axes.Add(axis)

    let series = HeatMapSeries()
    series.Data <- data
    series.X0 <- 0.0
    series.X1 <- double cols - 1.0
    series.Y0 <- 0.0
    series.Y1 <- double rows - 1.0
    series.Interpolate <- false
    model.Series.Add(series)

    model |> exportToPng path 300 300

//-------------------------------------------------------------------------------------------------

let renderPolicy path data =

    let data = data |> Array2D.map double

    let configureModel (model : PlotModel) =
        model.Padding <- OxyThickness(4.0, 5.0, 9.0, 6.0)
        model.Title <- "Policy"

    let configureAxisColor (axis : LinearColorAxis) =
        axis.Palette <- OxyPalettes.Jet(Compute.maxCarsCanMove * 2 + 1)
        axis.Minimum <- -(double Compute.maxCarsCanMove + 0.5)
        axis.Maximum <- +(double Compute.maxCarsCanMove + 0.5)
        axis.MajorStep <- 5.0
        axis.MinorStep <- 1.0
        axis.StringFormat <- "+#;-#;0"

    renderChart path data configureModel configureAxisColor

let renderValues path data =

    let configureModel (model : PlotModel) =
        model.Padding <- OxyThickness(4.0, 5.0, 4.0, 6.0)
        model.Title <- "Values"

    let configureAxisColor (axis : LinearColorAxis) =
        axis.Palette <- OxyPalettes.Jet(240)
        axis.LowColor <- OxyColors.Gray
        axis.Minimum <- 400.0
        axis.Maximum <- 640.0
        axis.MajorStep <- 40.0
        axis.MinorStep <- 20.0

    renderChart path data configureModel configureAxisColor
