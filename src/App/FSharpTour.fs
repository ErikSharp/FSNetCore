module FSharpTour

open System
open System.Windows.Forms

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

module TypeAnnotations =
    module ParametersAndReturns =
        //compiler doesn't know what x is to use Length
        //let length x = x.Length
        let length (x: string) = x.Length

        //here's how to specify return type if necessary
        let length2 (x: string): string = sprintf "length is %i" x.Length

    module Functions =
        let evalWith5ThenAdd2 fn = fn 5 + 2
        let times3 x = x * 3

        let result = evalWith5ThenAdd2 times3

        //here the compiler cannot figure out what the return is
        let evalWith5 fn = fn 5
        let yes (x: int) = true
        let result2 = evalWith5 yes

        let beingSpecific (fn: int -> int) = fn 5
        let evalWith5AsString fn: string = fn 5

    module Unit =
        let whatIsThis = ()

        //Here there is no domain and range, so the compiler sees
        //this as a simple value and not a function
        let helloWorld = printfn "Hello World"

        helloWorld

        //Force to create a function
        //We are declaring a unit input parameter
        let helloWorld2() = printfn "Hello World"

        //Now in order to call it, you have to specify the unit value
        //DONT GET THIS CONFUSED WITH C#!!!!!!!!!!!
        helloWorld2()

        module Ignore =
            let something =
                2 + 2 |> ignore
                "bueno"

    module Generics =
        let foo (fn: unit -> string) = "hello"
        let blah x = foo x.ToString
        let asdf x = x.ToString() + "foo"

        //When you see 'a it is referring to a generic type
        let onAStick x = x.ToString() + " on a stick"
        let a = onAStick 1
        let b = onAStick 1.2
        let c = onAStick "shit"

        let concat x y = x.ToString() + " " + y.ToString()

        //Here it figures out that x and y must be of the same type
        let isEqual x y = (x = y)
        let result = isEqual 'a' 'b'

    module OtherTypes =
        let takesATuple (x: int * string) = x.ToString()
        let aTuple = (1, "erik")

        let takesAnArray (x: int []) = x.ToString()
        let someArray = [| 1; 2; 3; 4 |]

        let takesAList (x: int list) = x.ToString()
        let someList = [ 1; 2; 3; 4 ]

        let takesASequence (x: seq<int>) = x.ToString()
        let someSequence = { 1 .. 4 }

        let takesAnOption (x: int option) = x.ToString()
        let anOption = Some(1)
        let anOption2 = None

        module TypeTests =
            let testA = float 2
            let testB x = float 2
            let testC x = float 2 + x
            let testD x = x.ToString().Length
            let testE (x: float) = x.ToString().Length
            let testF x = printfn "%s" x
            let testG x = printfn "%f" x
            let testH = 2 * 2 |> ignore
            let testI x = 2 * 2 |> ignore
            let testJ (x: int) = 2 * 2 |> ignore
            let testK = "hello"
            let testL() = "hello"
            let testM x = x = x
            let testN x = x 1 // hint: what kind of thing is x?
            let testO x: string = x 1

module Currying =
    //notice the signature shows the currying
    let printTwoParameters x y = printfn "x=%i y=%i" x y

    let x = 6
    let y = 99

    //broken apart
    let intermediateFn = printTwoParameters x
    let result = intermediateFn y

    //inline version of above
    let result2 = (printTwoParameters x) y

    //normal version
    let result3 = printTwoParameters x y

    module Operators =
        let result = 3 * 5

        //the operator can be broken apart as well
        //the parenthesis around the operator indicate that it is being used as a normal function and not an infix operator
        let intermediateFn = (*) 3
        let result2 = intermediateFn 5

        let result3 = (+) 3 5
        let theSame = 3 + 5

        let func x y = x + y

        let multiParmFunc (p1: int) (p2: bool) (p3: string) (p4: float) = ()
        //notice that it starts at the left - you couldn't use a float - You can supply the first N paramters
        let intermediate1 = multiParmFunc 1
        let intermediate2 = intermediate1 true
        let intermediate3 = intermediate2 "Erik"
        let final = intermediate3 3.14

        //watch out for mistakes like this - by putting the parenthesis, the function takes a unit parameter that must be supplied to execute
        //otherwise you are just dealing with a parially applied function
        let printHello() = printfn "hello"
        //the only way to make this work now is to supply the unit
        printHello()

        module TooManyParameters =
            let add1 x = x + 1
            //below won't work and it says that the value is not a function and cannot be applied
            //let x = add1 2 3

            //breaking down the above
            let intermediateFunc = add1 2
//this gives the same error as above
//let x = intermediateFunc 3

module PartialApplication =
    let twoIsLessThan = (<) 2
    let shouldBeFalse = twoIsLessThan 1

    let printer = printfn "printing param=%i"

    [ 1; 2; 3 ] |> List.iter printer

    let square i = i * i
    let divisibleBy3 i = i % 3 = 0
    let squareEach = List.map square
    let squares = [ 1 .. 50 ] |> squareEach
    let squaresDivisibleByThree = squares |> List.filter divisibleBy3

    module Deeper =
        let adder = (+)

        let adderWithPluggableLogger logger x y =
            logger "x" x
            logger "y" y
            let result = adder x y
            logger "x + y" result
            result

        let consoleLogger argName argValue = printfn "%s=%A" argName argValue

        let addWithConsoleLogger = adderWithPluggableLogger consoleLogger

        let sum = addWithConsoleLogger 1 3

        // You have to put this into FSI before this code will run
        // -r:assembly-filename System.Windows.Forms;;
        // options for FSI - https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-interactive-options
        // even though we have open System.Windows.Forms up top, we have to fully qualify it in FSI
        let popupLogger argName argValue =
            let message = sprintf "%s = %A" argName argValue
            System.Windows.Forms.MessageBox.Show
                (text = message, caption = "Logger", buttons = System.Windows.Forms.MessageBoxButtons.OK,
                 icon = System.Windows.Forms.MessageBoxIcon.Information) |> ignore

        let addWithPopupLogger = adderWithPluggableLogger popupLogger

        let sum2 = addWithPopupLogger 1 3

        let add42 = adder 42
        let add42WithConsole = addWithConsoleLogger 42
        let results = [ 1 .. 5 ] |> List.map add42
        let printedResults = [ 1 .. 5 ] |> List.map add42WithConsole

        module DesigningForPartialApplication =
            // 1. Put earlier: parameters more likely to be static
            let eachAdd42 = List.map (fun i -> i + 42)
            let mapped = [ 1 .. 22 ] |> eachAdd42

            // 2. Put last: the data structure or collection (or most varying argument)
            let compositeOp = List.map (fun i -> i + 1) >> List.filter (fun i -> i > 5)
            let result = compositeOp [ 1 .. 10 ]

            // 3. For well-known operations such as “subtract”, put in the expected order
            let substract5 x = x - 5
            let result2 = 10 |> substract5

            module ReplaceBclFunctions =
                //put replace into an order that works better for F#
                let replace oldStr newStr (s: string) = s.Replace(oldValue = oldStr, newValue = newStr)

                let result = "Erik" |> replace "ri" "ir"

                let startsWith (lookFor: string) (s: string) = s.StartsWith(lookFor)

                let fWords = [ "the"; "quick"; "brown"; "fox" ] |> List.filter (startsWith "f")

    module PipeFunction =
        // All it does is allow you to put the function argument in front of the function rather than after. That’s all.
        let parsedToInt = "12" |> int

        let chain =
            1
            |> (+) 2
            |> (*) 3

// A good explanation of the difference between |> and >>
// When piping, it is the arguments of the functions that are the data, and the functions are called one after another along the pipe.
// When composing on the other hand, it is the functions themselves that are the data, and the functions are glued together to form a composite function, which can then be called.

module Functions =
    let f a b c = a b c
    // the above actually means the function 'a' to the argument 'b', and then take the resulting function and evaluate it with the argument 'c'
    // it is the same as
    let f2 a b c = (a b) c
    // this is because function application is left associative which is the same way that partial application works

    // if you wanted right association, you can do these
    let f3 a b c = a (b c)
    let f4 a b c = b c |> a
    let f5 a b c = a <| b c

    module FunctionComposition =
        let f (x: int) = float x * 3.14159
        let g (x: float) = x > 4.0

        let combineLong (x: int) =
            let y = f x
            g y

        let combineShorter (x: int) = g (f x)

        // You can see that the compiler figured out that this was ultimately a function that maps 'a to 'c
        let compose f g x = g (f x)
        let shouldBeTrue = compose f g 3

        let (>>) f g x = g (f (x))

        let add1 x = x + 1
        let times2 x = x * 2
        let add1Times2 x = (>>) add1 times2 x

        //test
        let shouldBe8 = add1Times2 3

        let combineShortest = f >> g

module test =
    let intList = [ 1 .. 100 ]

    let mapped =
        List.map (fun i ->
            (i * i - i,
             (if i % 2 = 0 then "Even"
              else "Odd"))) intList
