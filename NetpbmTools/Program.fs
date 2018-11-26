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
            mapping <- mapping.Add(index + shift, Interpolate (index, index + 1))
    // do assertion here on the number of keys
    mapping

// Translate the virtual index into a virtual position.
let funcCalcVirtualPosition virtualColumns index =
    {Index = index; Column = index.Index% virtualColumns; Row = index.Index / virtualColumns}

// Translate a virtual Position back into a (possibly interpolated) byte.
let funcCalcByte (rnd : System.Random) sourceLookup (rowMapping : Map<int, Action>) (columnMapping : Map<int, Action>) (virtualPosition : Position) : byte =
    let sourceRow =
        match rowMapping.TryFind virtualPosition.Row with
            | Some theMapping ->
                match theMapping with
                    | Direct d -> d
                    | Interpolate (l, r) ->
                        if rnd.Next 2 = 0 then
                            l
                        else
                            r
            | _ -> failwith "missing row"

    let sourceColumn =
        match columnMapping.TryFind virtualPosition.Column with
            | Some theMapping ->
                match theMapping with
                    | Direct d -> d
                    | Interpolate (l, r) ->
                        if rnd.Next 2 = 0 then
                            l
                        else
                            r
            | _ -> failwith "missing column"
    
    sourceLookup (sourceRow, sourceColumn)

// Lookup a byte from the source image.
let funcSourceLookup (bytes : byte[]) (skip : SourceIndex) numSourceColumns (sourceRow, sourceColumn) =
    bytes.[skip.Index + sourceRow * numSourceColumns + sourceColumn]


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
    let removeColumns = funcRandomNumbers (rnd) columns 85
    // Add 85 new columns. Don't allow the rightmost column to be picked because of the way the interpolation alg works.
    let chosenColumns = funcRandomNumbers (rnd) (columns - 1) 85
    let columnMapping = funcCalcMapping removeColumns chosenColumns columns

    let removeRows = funcRandomNumbers (rnd) rows 52
    let chosenRows = funcRandomNumbers (rnd) (rows - 1) 52
    let rowMapping = funcCalcMapping removeRows chosenRows rows

    let destPath = "C:\Users\gregg\Downloads\Test_stretched.pgm"
    use streamWriter = new StreamWriter(destPath, false)

    let virtualRows = rows - removeRows.Count + chosenRows.Count
    let virtualColumns = columns - removeColumns.Count + chosenColumns.Count
    let dimensions = String.Format ("{0} {1}", virtualColumns, virtualRows)
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
        (funcCalcByte rnd (funcSourceLookup allBytes skip columns) rowMapping columnMapping) >>
        streamWriter.BaseStream.WriteByte
    
    seq {
        for i in 0 .. virtualColumns * virtualRows - 1 do
            yield {Index = i}
    }
    |> Seq.iter interpolateAndWrite

    streamWriter.Close()

    Console.WriteLine "press Enter to finish..."
    Console.ReadKey() |> ignore
    
    0 // return an integer exit code
