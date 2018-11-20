open System
open System.IO
open System.Collections.Generic

type GrayStruct = {Column : int; Row : int; Value : byte}
type Pixel = 
    | Gray of GrayStruct

let funcGetPixels skip numColumns =
    // After reading 3+ newlines need to switch to processing values
    // We'll handle that properly later on.
    // Just use 'skip' var for now.

    let allBytes = File.ReadAllBytes "C:\Users\gregg\Downloads\Test.pgm"

    let allPixels : Pixel array = Array.zeroCreate (allBytes.Length - skip)

    for index in 0 .. allPixels.Length - 1 do
        let column = index % numColumns
        let row = index / numColumns
        let value = allBytes.[index + skip]
        allPixels.[index] <- Gray {Column = column; Row = row; Value = value}

    allPixels

let funcRandomColumns (rnd : System.Random) max count = 
    let chosenColumns = new HashSet<int> ()
    while chosenColumns.Count < count do
        rnd.Next max |> chosenColumns.Add
    chosenColumns

let funcWriteBytes (writer : Stream) (bytes : byte list) = 
    bytes |> List.iter writer.WriteByte

let funcStretch (columns : HashSet<int>) (aPixel : Pixel) =
    match aPixel with
    | Gray gray when columns.Contains gray.Column -> 
        [gray.Value; gray.Value]
    | Gray gray -> 
        [gray.Value]


[<EntryPoint>]
let main argv = 
    printfn "args: %A" argv
    
    let skip = 39
    let rows = 525
    let columns = 850
    let chosenColumns = funcRandomColumns (System.Random ()) columns (columns / 10)
    let dimensions = String.Format ("{0} {1}", columns + chosenColumns.Count, rows)

    use streamWriter = new StreamWriter("C:\Users\gregg\Downloads\Test_stretched.pgm", false)
    streamWriter.WriteLine "P5"
    streamWriter.WriteLine dimensions
    streamWriter.WriteLine "255"
    streamWriter.Flush ()

    // Accepts a pixel, possibly expands it, and then writes it out.
    let funcHandlePixel = (funcStretch chosenColumns) >> (funcWriteBytes streamWriter.BaseStream)
    
    // Load the pixels, handle each one, and then close the stream.
    (funcGetPixels skip columns) |> Array.iter funcHandlePixel
    streamWriter.Close()

    Console.WriteLine "press Enter to finish..."
    Console.ReadKey() |> ignore
    
    0 // return an integer exit code
