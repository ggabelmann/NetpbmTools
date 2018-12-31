// The design is changing to:

// Overall, the tool loops over a function that translates Positions to Pixels.
// The input to that function and its output will eventually be stdin/stdout.
// The implementation of that function is a DAG of functions:
//    Position -> Pixel
//    Position -> Position
//    Pixel -> Pixel

// Depending on what one wants to do with the tool, the DAG can be assembled in different ways.
// This should cover my main use cases (stretching and shrinking in different ways).

open System
open System.IO
open System.Collections.Generic


// Types


type RGB = {Red : byte; Green : byte; Blue : byte}

type Pixel =
    | GrayPixel of byte
    | RGBPixel of RGB

// The position in an image.
type Position = {Column : int; Row : int}

type ImageMetadata = {BytesPerPixel : int; PixelsPerRow : int}

let BlackPixel = RGBPixel {Red = 0uy; Green = 0uy; Blue = 0uy}
let GrayPixel = RGBPixel {Red = 128uy; Green = 128uy; Blue = 128uy}
let WhitePixel = RGBPixel {Red = 255uy; Green = 255uy; Blue = 255uy}

// Magic number in PPM file format.
let BinaryGray = "P5"
let BinaryRGB = "P6"


// Utilities


// Translate an index into a Position.
let funcCalcPosition (imageMetadata : ImageMetadata) index =
    {Column = index % imageMetadata.PixelsPerRow; Row = index / imageMetadata.PixelsPerRow}

// Translate a Position to an index.
let funcCalcIndexOfPosition (imageMetadata : ImageMetadata) (position : Position) =
    position.Row * imageMetadata.PixelsPerRow * imageMetadata.BytesPerPixel + position.Column * imageMetadata.BytesPerPixel

// Generate a set of random numbers.
let funcRandomNumbers (rnd : System.Random) exclusiveMax count = 
    let chosen = new HashSet<int> ()
    while chosen.Count < count do
        rnd.Next exclusiveMax |> chosen.Add |> ignore
    chosen

// Scan through a list of functions until one of the functions returns a Some.
// Chain of Responsibility pattern.
let rec scanUntilSome theFunctionList theArg =
    match theFunctionList with
        | [] -> None
        | head :: tail ->
            let result = head theArg
            match result with
                | Some _ -> result
                | None -> scanUntilSome tail theArg

// Writes a Pixel out based on its type.
let funcWritePixel (write : byte -> unit) = function
    | GrayPixel p ->
        write p
    | RGBPixel p ->
        write p.Red
        write p.Green
        write p.Blue

// Writes the given text followed by a LineFeed.
// On Windows OS the system WriteLine function seems to write CRLF (which causes problems for IrfanView).
let funcWriteLF (writer : StreamWriter) (text : string) =
    writer.Write text
    writer.Write "\n"


// Pipeline handling code


// Change all even Rows to the given Pixel.
let funcInterlace (pixel : Pixel) (handler : Position -> Pixel) (position : Position) =
    if position.Row % 2 = 0 then pixel
    else handler position

// An area of pixels is handled by one function and all other pixels by another function.
// The position is not shifted.
let funcEmbed (topLeftInclusive : Position) (bottomRightExclusive : Position) (insideHandler : Position -> Pixel) (outsideHandler : Position -> Pixel) (position : Position) =
    if position.Column >= topLeftInclusive.Column && position.Column < bottomRightExclusive.Column && position.Row >= topLeftInclusive.Row && position.Row < bottomRightExclusive.Row then
        insideHandler position
    else
        outsideHandler position

// Shift the given Position by some amount.
let funcShift columnShift rowShift (position : Position) =
    {Column = position.Column + columnShift; Row = position.Row + rowShift}

// Randomly pick a nearby Pixel.
// Values may be negative, but that can be handled by a subsequent function.
let funcJitter (rnd : System.Random) (position : Position) = 
    {Column = position.Column + (rnd.Next 3) - 1; Row = position.Row + (rnd.Next 3) - 1}


// Column/Row mapping code -- WIP!


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


// Most of this is hardcoded as I explore F# and how image manipulation works.
[<EntryPoint>]
let main argv = 
    printfn "args: %A" argv
    
    // Hardcoded source image info.
    let sourcePath = "C:/Users/ggabelmann/Downloads/test.ppm"
    let skip = 38 // skip some header bytes in the source image.
    let sourceRows = 680
    let sourceColumns = 850
    let imageMetadata = {BytesPerPixel = 3; PixelsPerRow = sourceColumns}

    let rnd = System.Random ()
    let funcSetOfRandomNumbers = funcRandomNumbers rnd
    
    // Stretching is being refactored.

    // Remove 85 columns.
    let removeColumns = funcSetOfRandomNumbers sourceColumns 85
    // Add 85 new columns. Don't allow the rightmost column to be picked because of the way the interpolation alg works.
    let chosenColumns = funcSetOfRandomNumbers (sourceColumns - 1) 85
    let columnMapping = funcCalcMapping removeColumns chosenColumns sourceColumns

    let removeRows = funcSetOfRandomNumbers sourceRows 0
    let chosenRows = funcSetOfRandomNumbers (sourceRows - 1) 0
    let rowMapping = funcCalcMapping removeRows chosenRows sourceRows

    let mappingRows = sourceRows - removeRows.Count + chosenRows.Count
    let mappingColumns = sourceColumns - removeColumns.Count + chosenColumns.Count

    // end Stretching

    let canvasRows = mappingRows + 10
    let canvasColumns = mappingColumns + 10
    let dimensions = String.Format ("{0} {1}", canvasColumns, canvasRows)
    
    let destPath = "C:/Users/ggabelmann/Downloads/test_out.ppm"
    use streamWriter = new StreamWriter(destPath, false)
    let funcWriteLine = funcWriteLF streamWriter
    
    // Write to the dest file.
    funcWriteLine BinaryRGB
    funcWriteLine dimensions
    funcWriteLine "255"
    streamWriter.Flush ()

    // Read in the source image so that all columns/rows are in memory.
    let allBytes = File.ReadAllBytes sourcePath

    // Return an RGB Pixel from allBytes.
    let funcLookupSourcePixels (position : Position) =
        let index = skip + (funcCalcIndexOfPosition imageMetadata position)
        RGBPixel {Red = allBytes.[index]; Green = allBytes.[index + 1]; Blue = allBytes.[index + 2]}

    // The core function.
    // In this case, a square of the original image is jittered, everything else is white, and finally black lines over it all.
    let funcMapPositionToPixel = 
        funcInterlace
            (BlackPixel)
            (funcEmbed
                {Column = 5; Row = 5}
                {Column = 600; Row = 600}
                ((funcJitter rnd) >> funcLookupSourcePixels)
                (fun pos -> WhitePixel))
    
    let funcPixelWriter =
        funcWritePixel streamWriter.BaseStream.WriteByte

    // Loop through the "output canvas", calculate each Pixel, and write it out.
    for row in 0 .. canvasRows - 1 do
        for column in 0 .. canvasColumns - 1 do
            {Column = column; Row = row} |> (funcMapPositionToPixel >> funcPixelWriter)

    streamWriter.Close()

    Console.WriteLine "press Enter to finish..."
    Console.ReadKey() |> ignore
    
    0 // return an integer exit code
