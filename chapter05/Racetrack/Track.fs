module Track

//-------------------------------------------------------------------------------------------------

let private definition1 =
    [ "                   "
      "    XXXXXXXXXXXXXF "
      "   XXXXXXXXXXXXXXF "
      "   XXXXXXXXXXXXXXF "
      "  XXXXXXXXXXXXXXXF "
      " XXXXXXXXXXXXXXXXF "
      " XXXXXXXXXXXXXXXXF "
      " XXXXXXXXXX        "
      " XXXXXXXXX         "
      " XXXXXXXXX         "
      " XXXXXXXXX         "
      " XXXXXXXXX         "
      " XXXXXXXXX         "
      " XXXXXXXXX         "
      " XXXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "  XXXXXXXX         "
      "   XXXXXXX         "
      "   XXXXXXX         "
      "   XXXXXXX         "
      "   XXXXXXX         "
      "   XXXXXXX         "
      "   XXXXXXX         "
      "   XXXXXXX         "
      "    XXXXXX         "
      "    XXXXXX         "
      "    SSSSSS         "
      "                   " ]

let private definition2 =
    [ "                                  "
      "                 XXXXXXXXXXXXXXXF "
      "              XXXXXXXXXXXXXXXXXXF "
      "             XXXXXXXXXXXXXXXXXXXF "
      "            XXXXXXXXXXXXXXXXXXXXF "
      "            XXXXXXXXXXXXXXXXXXXXF "
      "            XXXXXXXXXXXXXXXXXXXXF "
      "            XXXXXXXXXXXXXXXXXXXXF "
      "             XXXXXXXXXXXXXXXXXXXF "
      "              XXXXXXXXXXXXXXXXXXF "
      "               XXXXXXXXXXXXXXXX   "
      "               XXXXXXXXXXXXX      "
      "               XXXXXXXXXXXX       "
      "               XXXXXXXXXX         "
      "               XXXXXXXXX          "
      "              XXXXXXXXXX          "
      "             XXXXXXXXXXX          "
      "            XXXXXXXXXXXX          "
      "           XXXXXXXXXXXXX          "
      "          XXXXXXXXXXXXXX          "
      "         XXXXXXXXXXXXXXX          "
      "        XXXXXXXXXXXXXXXX          "
      "       XXXXXXXXXXXXXXXXX          "
      "      XXXXXXXXXXXXXXXXXX          "
      "     XXXXXXXXXXXXXXXXXXX          "
      "    XXXXXXXXXXXXXXXXXXXX          "
      "   XXXXXXXXXXXXXXXXXXXXX          "
      "  XXXXXXXXXXXXXXXXXXXXXX          "
      " XXXXXXXXXXXXXXXXXXXXXXX          "
      " XXXXXXXXXXXXXXXXXXXXXXX          "
      " SSSSSSSSSSSSSSSSSSSSSSS          "
      "                                  " ]

//-------------------------------------------------------------------------------------------------

type Cell =
    | Start
    | Track
    | Final
    | Crash

let private loadDefinition definition =

    let definition = List.rev definition

    let xLength = definition |> List.map String.length |> List.max
    let yLength = definition |> List.length

    let initialize x y =
        match definition.[y].[x] with
        | 'S' -> Start
        | 'X' -> Track
        | 'F' -> Final
        | _   -> Crash

    Array2D.init xLength yLength initialize

//-------------------------------------------------------------------------------------------------

let racetrack1 = loadDefinition definition1
let racetrack2 = loadDefinition definition2
