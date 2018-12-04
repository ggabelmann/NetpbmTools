open System
open System.IO
open System.Collections.Generic


// Utilities

// Line feed.
let LF = 10uy

// The position in an image.
type Position = {Column : int; Row : int}

// Translate an index into a virtual Position.
let funcCalcVirtualPosition numColumns index =
    {Column = index % numColumns; Row = index / numColumns}

// Translate a Position to an index.
let funcCalcIndexOfPosition numColumns (position : Position) =
    position.Row * numColumns + position.Column

// Generate a set of random numbers.
let funcRandomNumbers (rnd : System.Random) exclusiveMax count = 
    let chosen = new HashSet<int> ()
    while chosen.Count < count do
        rnd.Next exclusiveMax |> chosen.Add |> ignore
    chosen

// Scan through a list of functions until one of the functions returns a Some.
let rec scanUntilSome theFunctionList theArg =
    match theFunctionList with
        | [] -> None
        | head :: tail ->
            let result = head theArg
            match result with
                | Some _ -> result
                | None -> scanUntilSome tail theArg


// Pipeline handling code

// Change all even Rows to black.
let funcInterlacing (position : Position) =
    if position.Row % 2 = 0 then Some 0uy
    else None

// Change all 'None' to the given gray value.
let funcByteHandler (noneValue : byte) = function
    | Some b -> b
    | None -> noneValue

// Push some pixels down based on Column and Row.
let funcSkew (positionHandler : Position -> byte option) (position : Position) =
    let skewed = {Column = position.Column; Row = position.Row - (position.Column/2)}
    if skewed.Row < 0 then
        None
    else
        positionHandler skewed

// Let a positionHandler handle an area of pixels.
let funcEmbed (topLeftInclusive : Position) (bottomRightExclusive : Position) (positionHandler : Position -> byte option) (position : Position) =
    if position.Column >= topLeftInclusive.Column && position.Column < bottomRightExclusive.Column && position.Row >= topLeftInclusive.Row && position.Row < bottomRightExclusive.Row then
        positionHandler {Column = position.Column - topLeftInclusive.Column; Row = position.Row - topLeftInclusive.Row}
    else
        None


// Column/Row mapping code

// What to do with a column/row.
type Action =
    | Direct of int
    | Interpolate of int

// "exclusiveEnd" must be the number of source columns.
// Returns a mapping from virtual rows to source rows (or virtual columns to source columns).
let funcCalcMapping (remove : HashSet<int>) (interpolate : HashSet<int>) exclusiveEnd =
    let mutable mapping = Map.empty
    let mutable shift = 0

    for index in 0 .. exclusiveEnd - 1 do
        if remove.Contains index then
            shift <- shift - 1
        else
            mapping <- mapping.Add(index + shift, Direct index)
        
        if interpolate.Contains index then
            shift <- shift + 1
            mapping <- mapping.Add(index + shift, Interpolate (index))
    // do assertion here on the number of keys
    mapping

// Translate a virtual Position back into a source Position.
let funcMapVirtualPosition (rnd : System.Random) (rowMapping : Map<int, Action>) (columnMapping : Map<int, Action>) (virtualPosition : Position) =
    let sourceRow =
        match rowMapping.TryFind virtualPosition.Row with
            | Some theMapping ->
                match theMapping with
                    | Direct d -> d
                    | Interpolate i -> i + rnd.Next 2
            | _ -> failwith "missing row"

    let sourceColumn =
        match columnMapping.TryFind virtualPosition.Column with
            | Some theMapping ->
                match theMapping with
                    | Direct d -> d
                    | Interpolate i -> i + rnd.Next 2
            | _ -> failwith "missing column"
    
    {Column = sourceColumn; Row = sourceRow}


[<EntryPoint>]
let main argv = 
    printfn "args: %A" argv
    
    // Hardcoded source image info.
    let sourcePath = "C:\Users\gregg\Downloads\Test.pgm"
    let skip = 38 // skip some header bytes in the source image.
    let sourceRows = 525
    let sourceColumns = 850

    let rnd = System.Random ()
    let funcSetOfRandomNumbers = funcRandomNumbers rnd
    
    // Remove 85 columns.
    let removeColumns = funcSetOfRandomNumbers sourceColumns 85
    // Add 85 new columns. Don't allow the rightmost column to be picked because of the way the interpolation alg works.
    let chosenColumns = funcSetOfRandomNumbers (sourceColumns - 1) 85
    let columnMapping = funcCalcMapping removeColumns chosenColumns sourceColumns

    let removeRows = funcSetOfRandomNumbers sourceRows 52
    let chosenRows = funcSetOfRandomNumbers (sourceRows - 1) 52
    let rowMapping = funcCalcMapping removeRows chosenRows sourceRows

    let destPath = "C:\Users\gregg\Downloads\Test_stretched.pgm"

    let mappingRows = sourceRows - removeRows.Count + chosenRows.Count
    let mappingColumns = sourceColumns - removeColumns.Count + chosenColumns.Count

    let canvasRows = mappingRows + 10
    let canvasColumns = mappingColumns + 10
    let dimensions = String.Format ("{0} {1}", canvasColumns, canvasRows)
    
    use streamWriter = new StreamWriter(destPath, false)
    streamWriter.WriteLine "P5"
    streamWriter.WriteLine dimensions
    streamWriter.Write "255"
    streamWriter.Flush ()
    // On windows the header contains CRLFs so far.
    // Now force only a LF to be used because IrfanView expects a single byte newline char after the "255".
    streamWriter.BaseStream.WriteByte LF

    let allBytes = File.ReadAllBytes sourcePath

    let funcSquishAndStretch (position : Position) =
        let mappedPosition = funcMapVirtualPosition rnd rowMapping columnMapping position
        Some allBytes.[skip + (funcCalcIndexOfPosition sourceColumns mappedPosition)]
    
    let funcBorderAroundWarp = 
        funcEmbed
            {Column = 5; Row = 5}
            {Column = mappingColumns + 5; Row = mappingRows + 5}
            (funcSkew funcSquishAndStretch)

    let funcConsumePosition =
        (scanUntilSome [funcInterlacing; funcBorderAroundWarp]) >>
        (funcByteHandler 255uy) >>
        streamWriter.BaseStream.WriteByte

    for row in 0 .. canvasRows - 1 do
        for column in 0 .. canvasColumns - 1 do
            funcConsumePosition {Column = column; Row = row}

    streamWriter.Close()

    Console.WriteLine "press Enter to finish..."
    Console.ReadKey() |> ignore
    
    0 // return an integer exit code
