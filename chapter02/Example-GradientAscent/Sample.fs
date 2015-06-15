module Sample

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let discreteDistribution distribution random =
    let bound = List.sum distribution
    let value = Sample.continuousUniform 0.0 bound random
    distribution
    |> List.scan (+) 0.0
    |> List.tail
    |> List.findIndex (fun x -> x >= value)
