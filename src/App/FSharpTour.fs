module FSharpTour

let double x = x + x
let multThree x = x * 3
let doubleTimesThree = double >> multThree
let shift4 x = x <<< 4
let intFromDouble = int 3.6
let showHex x = printfn "%X" x
//creates tuples with the first being the index and the second the square of the input
let squares = [ 0..5 ] |> List.mapi (fun i x -> (i, x * x))
let tuple1 = (1, 2, 3)
let tuple2 = ('a', 'b', 'c')
let tuple3 = ('a', 4.5, "turd")
//this function is generalized
let switchOuter (a, b, c) = (c, b, a)
let tupleSwitch1 = switchOuter tuple1
let tupleSwitch2 = switchOuter tuple2
let tupleSwitch3 = switchOuter tuple3
//struct tuples - good for preventing garbage
let storedOnTheStack = struct (4, 4.7)

[ //pipelining and composing functions
  1..40 ]
|> List.filter (fun x -> x % 3 = 2)
|> List.iter (double
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
        [ for month in 1..12 do
              for day in 1..System.DateTime.DaysInMonth(2017, month) do
                  yield System.DateTime(2017, month, day) ]

    let firstFive = daysList |> List.take 5

//Arrays are fixed-size, mutable collections of elements of the same type.
//They support fast random access of elements, and are faster than F# lists because they are just contiguous blocks of memory.
module Arrays =
    let empty = [||]
    let mustBeSameType = [| 1; 2; 3; 4 |]
    let numbers = [| 1..100 |]
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

    let loadsOfNumbers = seq { 1..100000 }

module Records =
    type ContactCard =
        { Name : string
          Phone : string
          Verified : bool }

    //automatically knows this is a ContactCard
    let erik =
        { Name = "Erik"
          Phone = "07412 196 361"
          Verified = true }

    let lynsey = { erik with Name = "Lynsey" }

    let showContact (c : ContactCard) =
        c.Name + " Phone: " + c.Phone + (if c.Verified then " Verified"
                                         else "")

    type OopRecord =
        { FirstName : string
          LastName : string }
        member this.SayHello = "Hello " + this.FirstName + " " + this.LastName

    let oopErik =
        { FirstName = "Erik"
          LastName = "Sharp" }

    [<Struct>]
    type ContactCardStruct =
        { Name : string
          Phone : string
          Verified : bool }

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
        { Suit: Suit; Rank: Rank}

    let aceOfSpades = {Suit = Spades; Rank = Ace}    

    let printCard (card: Card) =
        printfn "%A of %A" card.Rank card.Suit

    type Address = Address of string
    type PostCode = PostCode of string    
    let homePostCode = PostCode "SE18 6XD"
    let getPostCodeString (PostCode pc) = pc

    type PostalAddress = 
        { Address: Address; PostCode: PostCode}

    //I wouldn't be able to put homePostCode into Address
    let someAddress = {Address = Address "asdkfjasd"; PostCode = homePostCode}