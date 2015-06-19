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

let renderGrid path data =

    let rows = data |> Array2D.length1
    let cols = data |> Array2D.length2
    let data = Array2D.init cols rows (fun m n -> data.[n, m])

    let model = PlotModel()

    model.Padding <- OxyThickness(0.0, 0.0, 1.0, 1.0)
    model.PlotType <- PlotType.Cartesian

    let axis = LinearColorAxis()
    axis.Palette <- OxyPalettes.Jet(200)
    model.Axes.Add(axis)

    let axis = new LinearAxis()
    axis.Position <- AxisPosition.Top
    axis.IsAxisVisible <- false
    axis.Minimum <- 0.0
    axis.Maximum <- double cols
    axis.StartPosition <- 0.0
    axis.EndPosition <- 1.0
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Position <- AxisPosition.Left
    axis.IsAxisVisible <- false
    axis.Minimum <- 0.0
    axis.Maximum <- double rows
    axis.StartPosition <- 1.0
    axis.EndPosition <- 0.0
    model.Axes.Add(axis)

    let series = HeatMapSeries()
    series.Data <- data
    series.X0 <- 0.5
    series.X1 <- double cols - 0.5
    series.Y0 <- 0.5
    series.Y1 <- double rows - 0.5
    series.Interpolate <- false
    series.LabelFontSize <- 0.25
    series.LabelFormatString <- "F1"
    model.Series.Add(series)

    model |> exportToPng path 300 300
