namespace FSharpKoans
open NUnit.Framework

(*
A filter only lets particular elements through.
*)

module ``16: Filtering a list`` =
    [<Test>]
    let ``01 Fixed-function filtering, the hard way`` () =
        let filter (xs : int list) : int list =             //function to filter for odd elements only.

            let rec innerF (lst : int list) (ls : int list) (inc : int) : int list =    // inc used as length at first call, and list incerment in recursion 
                match inc with
                |0 -> ls
                |a -> match lst.[(inc-1)]%2 with // Only adds element to filtered list if 
                      |1 -> innerF lst (( (lst.[(inc-1)]))::ls) (inc-1)   //InnerF (Original List, Updated list, Increment--
                      |_ -> innerF lst ls (inc-1)       //case for even number
            innerF xs [] xs.Length  
 
        filter [1; 2; 3; 4] |> should equal [1; 3]
        filter [10; 9; 8; 7] |> should equal [9; 7]
        filter [15; 2; 7] |> should equal [15; 7]
        filter [215] |> should equal [215]
        filter [216] |> should equal []
        filter [2;4;6;8;10] |> should equal []
        filter [1;3;5;7;9] |> should equal [1;3;5;7;9]
        filter [] |> should equal []

   // once again, this would be a heck of a lot more flexible if we
   // were able to filter using different criteria!
   //
   // ... you can make a function to do that, right? ^_^.

    [<Test>]
    let ``02 Specified-function filtering, the hard way`` () =
        let filter (f : 'a -> bool) (xs : 'a list) : 'a list =      //a function which filters based on the specified criteria

            let rec innerF (lst : 'a list) (ls : 'a list) (inc : int) : 'a list =    // inc used as length at first call, and list incerment in recursion 
                match inc with
                |0 -> ls                                        //Base Case: returns list with elements matching specifications
                |a -> match (f lst.[(inc-1)]) with
                      |true -> innerF lst (( (lst.[(inc-1)]))::ls) (inc-1)   //pass, InnerF (Original List, Updated list, Increment--)
                      |_ -> innerF lst ls (inc-1)       //case for not pass, does not update the return list
            innerF xs [] xs.Length   

        filter (fun x -> x > 19) [9; 5; 23; 66; 4] |> should equal [23; 66]
        filter (fun x -> String.length x = 4) ["moo"; "woof"; "yip"; "nyan"; "meow"]
        |> should equal ["woof"; "nyan"; "meow"]
        filter (fun (a,b) -> a*b >= 14) [9,3; 4,2; 4,5] |> should equal [9,3; 4,5]

    // Hint: https://msdn.microsoft.com/en-us/library/ee370294.aspx
    [<Test>]
    let ``03 Specified-function mapping, the easy way`` () =
        List.filter (fun x -> x > 19) [9; 5; 23; 66; 4] |> should equal [23; 66]
        List.filter (fun x -> String.length x = 4) ["moo"; "woof"; "yip"; "nyan"; "meow"]
        |> should equal ["woof"; "nyan"; "meow"]
        List.filter (fun (a,b) -> a*b >= 14) [9,3; 4,2; 4,5] |> should equal [9,3; 4,5]
