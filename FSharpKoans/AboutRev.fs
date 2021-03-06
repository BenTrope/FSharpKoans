﻿namespace FSharpKoans
open NUnit.Framework

(*
Reversing a list.
*)

module ``14: Reversing a list`` =

    [<Test>]
    let ``01 Reversing a list, the hard way`` () =
        let rev (xs : 'a list) : 'a list =
            let rec innerF lst tsil =              //recursive function to reverse a list here.
                match lst with
                |[] -> tsil                         //Base case returns reversed
                |a -> innerF a.[1..] (a.[0]::tsil)   //appends first letter of list to the reverse list (tsil)
            innerF xs []
        //let a = rev [0;2;4]
             
        rev [9;8;7] |> should equal [7;8;9]
        rev [] |> should equal []
        rev [0] |> should equal [0]
        rev [9;3;4;1;6;5;4] |> should equal [4;5;6;1;4;3;9]

    // Hint: https://msdn.microsoft.com/en-us/library/ee340277.aspx
    [<Test>]
    let ``02 Reversing a list, the easy way`` () =
        List.rev [9;8;7] |> should equal [7;8;9]
        List.rev [] |> should equal []
        List.rev [0] |> should equal [0]
        List.rev [9;8;5;8;45] |> should equal [45;8;5;8;9]
