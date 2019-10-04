module DerekBanas
open System
open System.Text
// https://youtu.be/c7eNDJN758U

// https://fsharpforfunandprofit.com/posts/printf/
// https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/core.printf-module-%5Bfsharp%5D

let sayHello() =
    printf "What is your name: "
    let name = Console.ReadLine()
    printfn "Hello %s, nice to meet you%s" name Environment.NewLine

sayHello()

let pi_float = Math.PI

let showTruncatedFloat() =
    printfn "%f" pi_float

showTruncatedFloat()

let choosePrecision() =
    printfn "PI float to 3 decimals: %.3f" pi_float

choosePrecision()

let showDecimal() =
    let pi_literal = 3.1415926535897932384626433832795028841971693993751058209749445923078M
    //You cannot have automatic type conversion in F#
    //printfn "PI: %f" pi_literal
    printfn "PI: %M" pi_literal

showDecimal()

let showPadding() =
    //left justify and then right justify with preceeding zeroes
    printfn "%-10s %010i" "10" 10

showPadding()

let dynamicPadding() =
    printfn "%*s" 10 "Hello"

dynamicPadding()

let bind_stuff() =
    let mutable weight = 175
    weight <- 160

    assert (weight = 160)

    //reference cells
    let change_me = ref 10
    change_me := 50

    assert(!change_me = 50)

bind_stuff()

let do_funcs() =

    //types are explicitly declared
    let get_sum (x: int, y: int) : int = x + y

    assert (get_sum(5, 7) = 12)

    //specify that a function is recursive with rec
    //a factorial is the product of an integer and all the integers below it - 1 X 2 X 3 X 4
    let rec factorial x =
        if x < 1 then 1
        else x * factorial (x - 1)

    assert (factorial 4 = 24)

    let rand_list = [1;2;3]

    //lambda expressions
    let rand_list2 = List.map(fun x -> x * 2) rand_list

    assert(rand_list2 = [2;4;6])

    // piping results (not sure how to assert this)
    [5;6;7;8]
    |> List.filter(fun v -> (v % 2) = 0)
    |> List.map (fun x -> x * 2)
    |> printfn "Even Doubles: %A"    


    let mult_three x = x * 3
    let add_five y = y + 5

    let mult_add = mult_three >> add_five
    let add_mult = mult_three << add_five

    assert(mult_add 10 = 35)
    assert(add_mult 10 = 45)

do_funcs()


let do_math() = 
    assert (5 + 4 = 9)
    assert (5 - 4 = 1)
    assert (5 * 4 = 20)
    assert (5 / 4 = 1)
    assert (5 % 4 = 1)
    assert (5.0 ** 2.0 = 25.0)

    // checking the type
    let number = 2
    assert (number.GetType() = typeof<System.Int32>)

    // casting to float which in F# is a Double - float32 is the the float in C#
    // don't use the keyword float - use double and single instead
    let number2 = (float number) // (double number) would be the same
    assert (number2.GetType() = typeof<System.Double>)
    assert ((int 3.14159).GetType() = typeof<System.Int32>)

    assert (abs(-42) = 42)
    assert (ceil(4.5) = 5.0)
    assert (floor(4.5) = 4.0)
    assert (log(2.7188) > 1.00019)
    assert (log10(1000.0) > 2.99999)
    assert (sqrt(25.0) = 5.0)

do_math()

let string_stuff() =
    
    let backslash = "1234\\5678"
    let no_escaping = @"1234\5678"

    assert(backslash = no_escaping)

    let combined = backslash + " " + no_escaping
    assert (combined = @"1234\5678 1234\5678")

    assert (backslash.Length = 9)

    //accessing a character
    assert (backslash.[1] = '2')

    //substring index 1 to index 4
    assert (backslash.[1..4] = @"234\")

    //make string a csv by doing something to each character of a string
    let csv = String.collect(fun c -> c.ToString() + ",") "Erik"
    assert (csv = "E,r,i,k,")

    let any_upper = String.exists(fun c -> Char.IsUpper(c)) "Erik"
    assert (any_upper)

    let a_number = String.forall(fun c -> Char.IsDigit(c)) "1231d231"
    assert(not a_number)

    let evens = String.init 10 (fun i -> (i * 2).ToString() + " ")
    assert(evens = "0 2 4 6 8 10 12 14 16 18 ")

    //do something with each character
    //here I am shifting to the next ascii code with my name
    // 1uy is specifying 1 as a byte
    // have to pipe to ignore as appending to the string builder returns itself
    let cypher_builder = StringBuilder()
    String.iter(fun c -> cypher_builder.Append(char ((byte c) + 1uy)) |> ignore) "Erik Sharp"
    assert("Fsjl!Tibsq" = cypher_builder.ToString())


string_stuff()

let loop_stuff() =
    let magic_num = "7"
    let mutable guess = ""

    while not (magic_num.Equals(guess)) do
        printf "Guess the Number (it's 7): " 
        guess <- Console.ReadLine()

    printfn "Wow, you got it right"

    for i = 1 to 10 do
        printf "%i " i

    printfn ""

    for i = 10 downto 1 do
        printf "%i " i

    printfn ""

    for i in [3..7] do
        printf "%i " i

    printfn ""

    //this is the prefered way to do things in F#
    [2..8] |> List.iter(printf "%i ")
    
    // 1 * 2 = 2   2 * 3 = 6   6 * 4 = 24   24 * 5 = 120
    //the parameters to the reduce function are the accumulator and the element
    //Just putting the * by itself just means multiply them both together
    let sum = List.reduce (*) [1..5]
    assert (120 = sum)
    
loop_stuff()


let conditional_stuff() =
    let age = 8

    let get_foo = 
        if age < 5 then
            "Preschool"
        elif age = 5 then
            "Kindergarten"
        elif (age > 5) && (age <= 18) then
            let grade = age - 5
            "Grade " + grade.ToString()
        else
            "Go to college"

    assert (get_foo = "Grade 3")


    let gpa = 3.9
    let income = 15000
    let gets_grant = ((gpa >= 3.8) || (income <= 12000))
    assert(gets_grant)

    assert((not true) = false)

    let grade2: string = 
        match age with
        | age when age < 5 -> "Preschool"
        | 5 -> "Kindergarten"
        | age when ((age > 5) && (age <= 18)) -> "Grade " + (age - 5).ToString()
        | _ -> "College"

    assert(grade2 = "Grade 3")

conditional_stuff()

let list_stuff() =
    // :: the element on the left is prepended to the list on the right - just creates a list
    let list1 = 5::6::7::[]
    let expected1 = [5; 6; 7]
    assert(expected1 = list1)
    
    let list2 = [1..5]
    let list3 = ['a'..'g']
    let expected3 = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g']
    assert(expected3 = list3)

    let list5 = List.init 5 (fun i -> i * 2)
    let expected5 = [0; 2; 4; 6; 8]
    assert(expected5 = list5)

    let list6 = [ for a in 1..5 do yield (a * a)]
    let expected6 = [1; 4; 9; 16; 25]
    assert(expected6 = list6)

    let list7 = [ for a  in 1..10 do if a % 2 = 0 then yield a ]
    let expected7 = [2; 4; 6; 8; 10]
    assert(expected7 = list7)

    let list8 = [ for a in 1..3 do yield! [a .. a + 2]]
    let expected8 = [1; 2; 3; 2; 3; 4; 3; 4; 5]
    assert(expected8 = list8)

    assert (list8.Length = 9)
    assert (list8.IsEmpty = false)
    assert (list8.Item(2) = 3) //get by index
    assert (list8.[2] = 3) //same as above
    assert (list3.Head = 'a')
    assert (list1.Tail = [6; 7]) //Tail skips the first element

    let list9 = list2 |> List.filter (fun x -> x % 2 = 0)
    assert (list9 = [2; 4])

    let list10 = list1 |> List.map(fun x -> x * x)
    assert (list10 = [25; 36; 49])

    let list11 = [1..5] |> List.sortDescending
    assert(list11 = [5;4;3;2;1])

    //the first parameter 3 is the starting value - 3 + 1 + 2 + 3 - appears to be the same as List.reduce, but with the starting value
    let sum = List.fold (fun sum elem -> sum + elem) 3 [1..3]
    assert(sum = 9)    

list_stuff()

printfn ""

type emotion = 
| joy = 0
| fear = 1
| anger = 2

let enum_stuff() =
    let my_feeling = emotion.joy

    match my_feeling with
    | joy -> printfn "I'm joyful"
    | fear -> printfn "I'm fearful"
    | anger -> printfn "I'm angry"

enum_stuff()

let option_stuff() =
    let divide x y = 
        match y with
        | 0 -> None
        | _ -> Some(x/y)

    if (divide 5 0).IsSome then
        printfn "5 / 0 = %A" ((divide 5 0).Value)
    elif (divide 5 0).IsNone then
        printfn "Can't Divide by Zero"
    else
        printfn "Something weird happened"

option_stuff()

let tuple_stuff() =
    //the tuple is a parameter
    let avg (w, x, y, z) : float = 
        let sum = w + x + y + z
        sum / 4.0

    assert (avg (1.0, 2.0, 3.0, 4.0) = 2.5)

    //store the tuple
    let my_data = ("Erik", 42, 6.25)

    //pulling the data out of the tuple
    let (name, age, _) = my_data

    assert (name = "Erik")

tuple_stuff()


type customer =
    { Name: string;
    Balance: float }

type customer2 =
    { Name: string;
    Balance: float }

let record_stuff() =
    //The compiler automatically inferred that this was a customer2 which is covering up the customer record
    let bob = {Name = "Bob Smith"; Balance = 101.50 }

    assert(bob.Balance > 100.0)

record_stuff()


let seq_stuff() = 
    let seq1 = seq { 1..100 }
    let seq2 = seq { 0 .. 2 .. 20 } //only even numbers
    let seq3 = seq { 50 .. 1 } //descending
    
    printfn "Sequence shows truncated list %A" seq2
    //this is the same as calling ToList in Linq on an IEnumerable
    printfn "Sequence whole list %A" (Seq.toList seq2)

    let is_prime n =
        let rec check i =
            i > n / 2 || (n % i <> 0 && check (i + 1))
        check 2

    let prime_seq = seq {for n in 1..500 do if is_prime n then yield n}

    printfn "Prime sequence %A" prime_seq

seq_stuff()


let map_stuff() =
    //maps are key / value pairs
    let customers = 
        Map.empty.
            Add("Bob Smith", 101.50).
            Add("Sally Marks", 50.25)

    printfn "# of Customers %i" customers.Count

    let cust = customers.TryFind "Bob Smith"
    match cust with 
    | Some x -> printfn "Balance : %.2f" x
    | None -> printfn "Not found"

    printfn "Customers : %A" customers

    if customers.ContainsKey "Bob Smith" then
        printfn "Bob Smith was found"

    printfn "Bob's balance: %.2f" customers.["Bob Smith"]

    let custs2 = Map.remove "Sally Marks" customers

    printfn "# of Customers %i" custs2.Count

map_stuff()


let add_stuff<'T> x y = 
    printfn "%A" (x + y)

//https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/
let generic_stuff() =
    add_stuff<int> 1 2
    //for some reason I can't have both of these at the same time
    //add_stuff<float> 4.2 3.1


generic_stuff()


let exception_stuff() =
    let divide x y =
        try
            if y = 0 then raise (DivideByZeroException "Can't divide by zero")
            printfn "%i / %i = %i" x y (x / y)
        with
            | :? System.DivideByZeroException as ex -> printfn "%s" ex.Message

    divide 5 4
    divide 5 0

exception_stuff()


type Rectangle = struct 
    val Length : float
    val Width : float
    
    new (length, width) =
        {Length = length; Width = width}
end

let struct_stuff() =
    let area(shape: Rectangle) =
        shape.Length * shape.Width

    let rect = new Rectangle(5.0, 6.1)

    let rect_area = area rect

    printfn "Area: %.2f" rect_area

struct_stuff()


type Animal = class
    val Name: string
    val Height: float
    val Weight: float

    new (name, height, weight) =
        {Name = name; Height = height; Weight = weight}

    member x.Run = 
        printfn "%s Runs" x.Name
end

//notice that when you inherit you aren't using the class and end keywords
type Dog(name, height, weight) =
    inherit Animal(name, height, weight)

    member x.Bark = 
        printfn "%s barks" x.Name

let class_stuff() =
   let spot = new Animal("Spot", 20.5, 40.5)
   spot.Run

   let bowser = new Dog("Bowser", 20.5, 45.0)
   bowser.Run
   bowser.Bark

class_stuff()

printfn ""
printfn "...all assertions have passed"
Console.ReadKey() |> ignore