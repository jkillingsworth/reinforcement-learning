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

    let model = PlotModel()

    let axis = LinearColorAxis()
    axis.Palette <- OxyPalettes.Jet(200)
    model.Axes.Add(axis)

    let axis = new LinearAxis()
    axis.Position <- AxisPosition.Top
    axis.IsAxisVisible <- false
    axis.Minimum <- 0.0 - 0.5
    axis.Maximum <- 4.0 + 0.5
    axis.StartPosition <- 0.0
    axis.EndPosition <- 1.0
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Position <- AxisPosition.Left
    axis.IsAxisVisible <- false
    axis.Minimum <- 0.0 - 0.5
    axis.Maximum <- 4.0 + 0.5
    axis.StartPosition <- 1.0
    axis.EndPosition <- 0.0
    model.Axes.Add(axis)

    let series = HeatMapSeries()
    series.Data <- data |> Array2D.mapi (fun m n _ -> data.[n, m])
    series.X0 <- 0.0
    series.X1 <- 4.0
    series.Y0 <- 0.0
    series.Y1 <- 4.0
    series.Interpolate <- false
    series.LabelFontSize <- 0.25
    series.LabelFormatString <- "F1"
    model.Series.Add(series)

    model |> exportToPng path 400 400
