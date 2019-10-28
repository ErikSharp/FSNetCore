module FSharpTour

open System

let double x = x + x
let multThree x = x * 3
let doubleTimesThree = double >> multThree
let shift4 x = x <<< 4
let intFromDouble = int 3.6
let showHex x = printfn "%X" x
//creates tuples with the first being the index and the second the square of the input
let squares = [ 0 .. 5 ] |> List.mapi (fun i x -> (i, x * x))
let tuple1 = (1, "Erik")
let letter = fst tuple1
let me = snd tuple1
let tuple2 = ('a', 'b', 'c')
let tuple3 = ('a', 4.5, "turd")
//this function is generalized
let switchOuter (a, b, c) = (c, b, a)
let tupleSwitch2 = switchOuter tuple2
let tupleSwitch3 = switchOuter tuple3
//struct tuples - good for preventing garbage
let storedOnTheStack = struct (4, 4.7)

[ 1 .. 40 ]
|> List.filter (fun x -> x % 3 = 2)
|> List.iter
    (double
     >> multThree
     >> doubleTimesThree
     >> shift4
     >> showHex)

//Lists are immutable singly-linked lists and are best for iteration
module Lists =
    let emptyList = []
    //must use semicolons for lists
    let list1 = [ 1; 2; 3 ]
    //this is a list that contains 3 int tuples - be careful
    let notAList = [ 1, 2, 3 ]

    //won't work as they are not the same type
    //let list2 = ['a'; 2; 3]
    let daysList =
        [ for month in 1 .. 12 do
            for day in 1 .. System.DateTime.DaysInMonth(2017, month) do
                yield System.DateTime(2017, month, day) ]

    let firstFive = daysList |> List.take 5

//Arrays are fixed-size, mutable collections of elements of the same type.
//They support fast random access of elements, and are faster than F# lists because they are just contiguous blocks of memory.
module Arrays =
    let empty = [||]
    let mustBeSameType = [| 1; 2; 3; 4 |]
    let numbers = [| 1 .. 100 |]
    //slice notation
    let subArraySlice = numbers.[6..9]

    //can mutate
    subArraySlice.[0] <- 50

//elements are computed only when they are needed
//just like IEnumerable in LINQ
module Sequences =
    let empty = Seq.empty

    let namesSeq =
        seq {
            yield "Erik"
            yield "Lynsey"
            yield "James"
            yield "Naho"
        }

    let loadsOfNumbers = seq { 1 .. 100000 }

module Records =
    type ContactCard =
        { Name: string
          Phone: string
          Verified: bool }

    //automatically knows this is a ContactCard
    let erik =
        { Name = "Erik"
          Phone = "07412 196 361"
          Verified = true }

    let lynsey = { erik with Name = "Lynsey" }

    let showContact (c: ContactCard) =
        c.Name + " Phone: " + c.Phone + (if c.Verified then " Verified"
                                         else "")

    type OopRecord =
        { FirstName: string
          LastName: string }
        member this.SayHello = "Hello " + this.FirstName + " " + this.LastName

    let oopErik =
        { FirstName = "Erik"
          LastName = "Sharp" }

    [<Struct>]
    type ContactCardStruct =
        { Name: string
          Phone: string
          Verified: bool }

module DiscriminatedUnions =
    type Suit =
        | Hearts
        | Clubs
        | Diamonds
        | Spades

    type Rank =
        | Ace
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Jack
        | Queen
        | King

    type Card =
        { Suit: Suit
          Rank: Rank }

    let aceOfSpades =
        { Suit = Spades
          Rank = Ace }

    let printCard (card: Card) = printfn "%A of %A" card.Rank card.Suit

    type Address = Address of string

    type PostCode = PostCode of string

    let homePostCode = PostCode "SE18 6XD"
    let getPostCodeString (PostCode pc) = pc

    type PostalAddress =
        { Address: Address
          PostCode: PostCode }

    //I wouldn't be able to put homePostCode into Address
    let someAddress =
        { Address = Address "asdkfjasd"
          PostCode = homePostCode }

    type BinarySearchTree<'T> =
        | Empty
        | Node of value: 'T * left: BinarySearchTree<'T> * right: BinarySearchTree<'T>

    let rec exists item bst =
        match bst with
        | Empty -> false
        | Node(x, left, right) ->
            if item = x then true
            elif item < x then (exists item left)
            else (exists item right)

    let rec insert item bst =
        match bst with
        | Empty -> Node(item, Empty, Empty)
        | Node(x, left, right) as node ->
            if item = x then node
            elif item < x then Node(x, insert item left, right)
            else Node(x, left, insert item right)

    //                  10
    //            6              20
    //        3      8       15      27
    let myBst =
        insert 10 Empty
        |> insert 20
        |> insert 15
        |> insert 6
        |> insert 3
        |> insert 8
        |> insert 15
        |> insert 27
        |> insert 10

    let threeExists = exists 3 myBst

module PatternMatching =
    type Person =
        { First: string
          Last: string }

    type Employee =
        | Engineer of engineer: Person
        | Manager of manager: Person * reports: List<Employee>
        | Executive of executive: Person * reports: List<Employee> * assistant: Employee

    let rec countReports (emp: Employee) =
        1 + match emp with
            | Engineer(person) -> 0
            | Manager(person, reports) -> reports |> List.sumBy countReports
            | Executive(person, reports, assistant) -> (reports |> List.sumBy countReports) + countReports assistant

    //Find all managers/executives with no reports whos first name is Dave
    let rec findDaveWithOpenPostition (emps: List<Employee>) =
        emps
        |> List.filter (function
            | Manager({ First = "Dave" }, []) -> true
            | Executive({ First = "Dave" }, [], _) -> true
            | _ -> false)

    let erik =
        Engineer
            { First = "Erik"
              Last = "Sharp" }

    let lynsey =
        Engineer
            { First = "Lynsey"
              Last = "Sharp" }

    let ron =
        Engineer
            { First = "Ron"
              Last = "Sharp" }

    let desma =
        Engineer
            { First = "Desma"
              Last = "Sharp" }

    let jim =
        { First = "Jim"
          Last = "Raynor" }

    let dave =
        Executive
            ({ First = "Dave"
               Last = "Smith" }, [ erik; lynsey ], desma)

    let emps: List<Employee> =
        [ erik
          lynsey
          ron
          desma
          dave
          Manager
              ({ First = "Dave"
                 Last = "Letterman" }, []) ]

    emps
    |> findDaveWithOpenPostition
    |> List.iter (printfn "%A")
    dave
    |> countReports
    |> printfn "Dave has %i reports"

module ActivePatterns =
    let (|RGB|) (col: System.Drawing.Color) = (col.R, col.G, col.B)
    let (|HSB|) (col: System.Drawing.Color) = (col.GetHue(), col.GetSaturation(), col.GetBrightness())

    let printRGB (col: System.Drawing.Color) =
        match col with
        | RGB(r, g, b) -> printfn " Red: %d Green: %d Blue: %d" r g b

    let printHSB (col: System.Drawing.Color) =
        match col with
        | HSB(h, s, b) -> printfn " Hue: %f Saturation: %f Brightness: %f" h s b

    let printAll col colorString =
        printfn "%s" colorString
        printRGB col
        printHSB col

    printAll System.Drawing.Color.Red "Red"
    printAll System.Drawing.Color.Black "Black"
    printAll System.Drawing.Color.White "White"
    printAll System.Drawing.Color.Gray "Gray"
    printAll System.Drawing.Color.BlanchedAlmond "BlanchedAlmond"

/// Units of measure are a way to annotate primitive numeric types in a type-safe way.
/// You can then perform type-safe arithmetic on these values.
///
/// To learn more, see: https://docs.microsoft.com/dotnet/fsharp/language-reference/units-of-measure
module UnitsOfMeasure =
    /// First, open a collection of common unit names
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

    /// Define a unitized constant
    let sampleValue1 = 1600.0<meter>

    /// Next, define a new unit type
    [<Measure>]
    type mile =
        /// Conversion factor mile to meter.
        static member asMeter = 1609.34<meter/mile>

    /// Define a unitized constant
    let sampleValue2 = 500.0<mile>

    /// Compute with metric-system constant
    let sampleValue3 = (sampleValue2 * mile.asMeter) + sampleValue1

    module MoreExamples =
        [<Measure>]
        type Cm

        [<Measure>]
        type Mm

        //conversion constant
        let mmPerCm: float<Mm / Cm> = 10.0<Mm/Cm>
        //conversion function
        let convertCmToMm (x: float<Cm>) = x * mmPerCm

        let getCircleCircumference (r: float<Cm>) = 2.0 * Math.PI * r
        let circ = getCircleCircumference 10.0<Cm>

        let circMm = convertCmToMm circ

module CurryingFunctions =
    //function with more than one parameter
    let multiply x y = x * y
    //partial application of args to create a function that will triple an int
    let triple = multiply 3
    let twentySeven = triple 9

module test =
    let intList = [ 1 .. 100 ]

    let mapped =
        List.map (fun i ->
            (i * i - i,
             (if i % 2 = 0 then "Even"
              else "Odd"))) intList
