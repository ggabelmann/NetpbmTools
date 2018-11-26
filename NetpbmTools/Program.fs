open System
open System.IO
open System.Collections.Generic

// Index into source image.
type SourceIndex = {Index : int}

// Index into virtual image (ie, the manipulated image).
type VirtualIndex = {Index : int}

// The position in the virtual image.
type Position = {Index : VirtualIndex; Column : int; Row : int}

// What to do with a column or two.
type Action =
    | Direct of int
    | Interpolate of int * int

// Line feed.
let LF = 10uy

// Generate a set of random numbers.
let funcRandomNumbers (rnd : System.Random) exclusiveMax count = 
    let chosen = new HashSet<int> ()
    
    while chosen.Count < count do
        rnd.Next exclusiveMax |> chosen.Add
    chosen

// "exclusiveEnd" must be the number of source columns.
// Return a mapping from virtual columns to source columns.
let funcCalcColumnMapping (skipColumns : HashSet<int>) (chosenColumns : HashSet<int>) exclusiveEnd =
    let mutable mapping = Map.empty
    let mutable shift = 0

    for index in 0 .. exclusiveEnd - 1 do
        if skipColumns.Contains index then
            shift <- shift - 1
        else
            mapping <- mapping.Add(index + shift, Direct index)
        
        if chosenColumns.Contains index then
            shift <- shift + 1
            mapping <- mapping.Add(index + shift, Interpolate (index, index + 1))
    // do assertion here on the number of keys
    mapping

// Translate the virtual index into a virtual position.
let funcCalcVirtualPosition virtualColumns index =
    {Index = index; Column = index.Index% virtualColumns; Row = index.Index / virtualColumns}

// Translate a virtual Position back into a (possibly interpolated) byte.
let funcCalcByte (rnd : System.Random) sourceLookup (columnMapping : Map<int, Action>) (virtualPosition : Position) : byte =
    match columnMapping.TryFind virtualPosition.Column with
        | Some theMapping ->
            match theMapping with
                | Direct d -> sourceLookup (virtualPosition.Row, d)
                | Interpolate (l, r) ->
                    if rnd.Next 2 = 0 then
                        sourceLookup (virtualPosition.Row, l)
                    else
                        sourceLookup (virtualPosition.Row, r)
        | _ -> failwith "missing column"

// "numColumns" is the number of source columns.
// Lookup a byte from the source image.
let funcSourceLookup (bytes : byte[]) (skip : SourceIndex) numColumns (row, column) =
    bytes.[skip.Index + row * numColumns + column]


[<EntryPoint>]
let main argv = 
    printfn "args: %A" argv
    
    // Hardcoded source image info.
    let sourcePath = "C:\Users\gregg\Downloads\Test.pgm"
    let skip : SourceIndex = {Index = 38} // skip some header bytes in the source image.
    let rows = 525
    let columns = 850

    let rnd = System.Random ()
    
    // Remove 85 columns.
    let removeColumns = funcRandomNumbers (rnd) columns 170
    // Add 85 new columns. Don't allow the rightmost column to be picked because of the way the interpolation alg works.
    let chosenColumns = funcRandomNumbers (rnd) (columns - 1) 170
    let columnMapping = funcCalcColumnMapping removeColumns chosenColumns columns

    let destPath = "C:\Users\gregg\Downloads\Test_stretched.pgm"
    use streamWriter = new StreamWriter(destPath, false)

    let virtualColumns = columns - removeColumns.Count + chosenColumns.Count
    let dimensions = String.Format ("{0} {1}", virtualColumns, rows)
    streamWriter.WriteLine "P5"
    streamWriter.WriteLine dimensions
    streamWriter.Write "255"
    streamWriter.Flush ()
    // On windows the header contains CRLFs so far.
    // Now force only a LF to be used because IrfanView expects a single byte newline char after the "255".
    streamWriter.BaseStream.WriteByte LF

    let allBytes = File.ReadAllBytes sourcePath

    let interpolateAndWrite =
        (funcCalcVirtualPosition virtualColumns) >>
        (funcCalcByte rnd (funcSourceLookup allBytes skip columns) columnMapping) >>
        streamWriter.BaseStream.WriteByte
    
    seq {
        for i in 0 .. virtualColumns * rows - 1 do
            yield {Index = i}
    }
    |> Seq.iter interpolateAndWrite

    streamWriter.Close()

    Console.WriteLine "press Enter to finish..."
    Console.ReadKey() |> ignore
    
    0 // return an integer exit code
