module ListExercises

open System

//although lists must contain items of the same type,
//they can contain polymorphic types if the base type is specified
//let myControlList : Control list = [ new Button(); new CheckBox() ]
let throughTen = [ 1 .. 10 ]
let emptyList = []
let emptyList2: int list = List.Empty

let listFromSequenceExpression =
    [ for i in 1 .. 10 -> i * i ]

//gives you:     int list = [100; 2; 3; 4]
let useConsOperator = 100 :: [ 2; 3; 4 ]
let low = [ 1; 2; 3 ]
let high = [ 4; 5; 6 ]
let combined = low @ high
let one = combined.Head
let allButOne = combined.Tail
let six = combined.Length
//remember that Lists are forward only Linked lists therefore doing this
//is O(n). If you did this in an array then you can go straight to the item so
//access is O(1)
let four = combined.Item 3

module recursion =
    //Here the cons operator is separating the head from the tail
    //I think this only works in pattern matching
    //Be careful as this could overflow the stack
    let rec sum list =
        match list with
        | head :: tail -> head + sum tail
        | [] -> 0

    let twentyOne = sum combined

    //Uses an accumulator and is tail recursive to prevent overflowing the stack
    let sumTailRecursive list =
        let rec loop list acc =
            match list with
            | head :: tail -> loop tail (acc + head)
            | [] -> acc
        loop list 0

    let twenty = sumTailRecursive combined.Tail

    module RemoveNonPrimes =
        let IsPrimeMultipleTest n x = x = n || x % n <> 0

        let rec RemoveAllMultiples listn listx =
            match listn with
            | head :: tail -> RemoveAllMultiples tail (List.filter (IsPrimeMultipleTest head) listx)
            | [] -> listx

        let GetPrimesUpTo n =
            let max = int (sqrt (float n))
            RemoveAllMultiples [ 2 .. max ] [ 1 .. n ]

        printfn "Primes Up To %d:\n %A" 100 (GetPrimesUpTo 100)

module ListModuleFunctions =
    let shouldBeTrue =
        emptyList.IsEmpty && [ 1 .. 5 ] |> List.exists (fun i -> i = 4) && List.exists2 (=) [ 1 .. 3 ] [ 3 .. -1 .. 1 ] && //2 = 2
        [ 2 .. 2 .. 10 ] |> List.forall (fun i -> i % 2 = 0) && //all even numbers
        List.forall2 (=) [ 1 .. 5 ] [ 1 .. 5 ] //every element is equal in each list

    let numbers = [ -3; 8; 2; -2; 3; 3 ]
    let sorted = numbers |> List.sort
    let absSorted = List.sortBy abs numbers

    module SortWith =
        type Widget =
            { ID: int
              Rev: int }

        let compareWidgets widget1 widget2 =
            if widget1.ID < widget2.ID then -1
            else if widget1.ID > widget2.ID then 1
            else if widget1.Rev < widget2.Rev then -1
            else if widget1.Rev > widget2.Rev then 1
            else 0

        let listToCompare =
            [ { ID = 92
                Rev = 1 }
              { ID = 110
                Rev = 1 }
              { ID = 100
                Rev = 5 }
              { ID = 100
                Rev = 2 }
              { ID = 92
                Rev = 1 } ]

        let sortedWidgetList = List.sortWith compareWidgets listToCompare

        printfn "%A" sortedWidgetList

    module Searching =
        //finds the first
        let ten = [ 1 .. 20 ] |> List.find (fun i -> i = 10)

        let valuesList =
            [ ("a", 1)
              ("b", 2)
              ("c", 3) ]

        let resultPick =
            List.pick (fun elem ->
                match elem with
                | (value, 2) -> Some value
                | _ -> None) valuesList

        printfn "%A" resultPick


        let list1d = [ 1; 3; 7; 9; 11; 13; 15; 19; 22; 29; 36 ]
        let isEven x = x % 2 = 0

        match List.tryFind isEven list1d with
        | Some value -> printfn "The first even value is %d." value
        | None -> printfn "There is no even value in the list."

        match List.tryFindIndex (fun i -> i = 9) list1d with
        | Some value -> printfn "The value 9 is at index %d." value
        | None -> printfn "There is no value 9 in the list."

    module Arithmetic =
        let sum1 = List.sum [ 1 .. 10 ]
        let sum2 = [ 1 .. 10 ] |> List.sum

        assert (sum1 = sum2)

        let sum3 = List.sumBy (fun i -> i * i) [ 1 .. 10 ]

        //will not work with integers as they always have a remainder with division
        let avg1 = List.average [ 1.2; 2.1; 3.2; 7.4 ]
        let avg2 = List.averageBy float [ 1 .. 10 ]

    module ListsWithTuples =
        let list1 = [ 1 .. 5 ]
        let list2 = [ 'a'; 'b'; 'c'; 'd'; 'e' ]
        let zipped = List.zip list1 list2
        let unzipped = List.unzip zipped
        let list1Extracted = fst unzipped
        let list2Extracted = snd unzipped

        let list3 = [ "Erik"; "Lynsey"; "Stephan"; "Louanne"; "Torben" ]
        let zipped3 = List.zip3 list1 list2 list3

    module Iterating =
        let list1 = [ 4 .. 2 .. 20 ]

        List.iter (fun n -> printfn "%i" n) list1

        List.iteri (fun i n -> printfn "%i - %i" i n) list1

        let list2 = [ 20 .. -2 .. 4 ]

        List.iter2 (fun m n -> printfn "%i : %i" m n) list1 list2

    module Mapping =
        let list1 = [ 'a'; 'b'; 'c'; 'd'; 'e' ]
        let upper = List.map (fun c -> Char.ToUpper(c)) list1

        let list2 =
            [ char 1
              char 1
              char 1
              char 1
              char 1 ]

        let result2 = List.map2 (+) list1 list2

        let result3 = List.map3 (fun x y z -> x + y + z) list1 list2 result2

        let collectList =
            List.collect (fun x ->
                [ for i in 1 .. 3 -> x * i ]) [ 1 .. 3 ]

        let filtered = List.filter (fun i -> i % 2 = 0) [ 1 .. 10 ]

        let listWords = [ "and"; "Rome"; "Bob"; "apple"; "zebra" ]
        let isCapitalized (string1: string) = System.Char.IsUpper string1.[0]
        //List.choose lets you select (filter) and transform (map) at the same time
        let results =
            List.choose (fun elem ->
                match elem with
                | elem when isCapitalized elem -> Some(elem + "'s")
                | _ -> None) listWords

        let twoListsJoined = List.append [ 1 .. 3 ] [ 7 .. 9 ]
        let threeListsJoined = List.concat [ list1; list2; result2 ]

    module FoldAndScan =
        let multiplyUp list = List.fold (*) 1 list
        let result = multiplyUp [ 1 .. 6 ]

        // The following example uses List.fold to reverse a list.
        // The accumulator starts out as the empty list, and the function uses the cons operator
        // to add each successive element to the head of the accumulator list, resulting in a
        // reversed form of the list.
        let reverseList list = List.fold (fun acc elem -> elem :: acc) [] list
        let reversed = reverseList [ 1 .. 10 ]

        module BankingExample =
            type Transaction =
                | Deposit
                | Withdrawal
                | Interest

            let transactionTypes = [ Deposit; Deposit; Withdrawal; Interest ]

            let transactionAmounts =
                [ 100.00
                  1000.00
                  95.00
                  0.05 / 12.0 ]

            let initialBalance = 200.00

            // Use fold2 to perform a calculation on the list to update the account balance.
            let endingBalance =
                List.fold2 (fun acc elem1 elem2 ->
                    match elem1 with
                    | Deposit -> acc + elem2
                    | Withdrawal -> acc - elem2
                    | Interest -> acc * (1.0 + elem2)) initialBalance transactionTypes transactionAmounts

        let scan = List.scan (*) 1 [ 1 .. 5 ]

        let reduce =
            List.reduce (fun acc elem ->
                printfn "Acc: %i, Elem: %i" acc elem
                acc * elem) [ 1 .. 4 ]

    module Conversion =
        let list1 = [ 1 .. 5 ]
        let sequence = list1 |> List.toSeq
        let list2 = List.ofSeq sequence
        let array = List.toArray list1
        let list3 = array |> List.ofArray
