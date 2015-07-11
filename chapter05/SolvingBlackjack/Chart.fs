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

let private renderChart path data title initializeData configureModel configureAxisColor =

    let base1 = data |> Array2D.base1
    let base2 = data |> Array2D.base2
    let length1 = data |> Array2D.length1
    let length2 = data |> Array2D.length2

    let data = Array2D.init length1 length2 (initializeData data base1 base2)

    let model = PlotModel()

    model.PlotType <- PlotType.Cartesian
    model.Title <- title
    model |> configureModel

    let axis = LinearColorAxis()
    axis.Position <- AxisPosition.Right
    axis.AxisDistance <- 8.0
    axis.Minimum <- -1.0
    axis.Maximum <- +1.0
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis |> configureAxisColor
    model.Axes.Add(axis)

    let axis = new LinearAxis()
    axis.Title <- "Dealer"
    axis.Position <- AxisPosition.Bottom
    axis.IsAxisVisible <- true
    axis.Minimum <- -0.5
    axis.Maximum <- double length1 - 0.5
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis.LabelFormatter <- (fun x -> if x = 0.0 then "A" else sprintf "%.0f" (x + double base1))
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Title <- "Player"
    axis.Position <- AxisPosition.Left
    axis.IsAxisVisible <- true
    axis.Minimum <- -0.5
    axis.Maximum <- double length2 - 0.5
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis.LabelFormatter <- (fun x -> sprintf "%.0f" (x + double base2))
    axis.AxisTitleDistance <- 6.0
    model.Axes.Add(axis)

    let series = HeatMapSeries()
    series.Data <- data
    series.X0 <- 0.0
    series.X1 <- double length1 - 1.0
    series.Y0 <- 0.0
    series.Y1 <- double length2 - 1.0
    series.Interpolate <- false
    model.Series.Add(series)

    model |> exportToPng path 300 300

//-------------------------------------------------------------------------------------------------

let renderValues path data title =

    let initializeData (data : double[,]) base1 base2 x1 x2 =
        data.[x1 + base1, x2 + base2]
    
    let configureModel (model : PlotModel) =
        model.Padding <- OxyThickness(8.0, 8.0, 8.0, 8.0)

    let configureAxisColor (axis : LinearColorAxis) =
        axis.StringFormat <- "+#;-#;0"

    renderChart path data title initializeData configureModel configureAxisColor

let renderPolicy path data title =

    let initializeData (data : Compute.Action[,]) base1 base2 x1 x2 =
        match data.[x1 + base1, x2 + base2] with
        | Compute.Action.Stand -> -1.0
        | Compute.Action.Hit -> +1.0
    
    let configureModel (model : PlotModel) =
        model.Padding <- OxyThickness(8.0, 8.0, 7.0, 8.0)

    let configureAxisColor (axis : LinearColorAxis) =
        axis.StringFormat <- "Hit;'';''"
        axis.Palette <- OxyPalette(OxyColors.Gray, OxyColors.GreenYellow)

    renderChart path data title initializeData configureModel configureAxisColor
