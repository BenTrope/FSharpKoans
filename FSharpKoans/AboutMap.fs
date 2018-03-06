namespace FSharpKoans
open NUnit.Framework

(*
Transforming a sequence is called "mapping".
*)

module ``15: Applying a map to a list`` =
    [<Test>]
    let ``01 Fixed-function mapping, the hard way (part 1).`` () =
        let map (xs : int list) : int list =        // function which adds 1 to each element
            let rec innerF (lst : int list) (ls : int list) (inc : int) : int list =    // inc used as length at first call, and list incerment in recursion 

                match inc with
                |0 -> ls                //Base Case: when the are no more iterations left, returns the altered list
                |a -> innerF lst ((lst.[(inc-1)]+1)::ls) (inc-1)   //InnerF (Original List, Updated list, Increment--)
            innerF xs [] xs.Length
        
        map [1; 2; 3; 4] |> should equal [2; 3; 4; 5]
        map [9; 8; 7; 6] |> should equal [10; 9; 8; 7]
        map [15; 2; 7] |> should equal [16; 3; 8]
        map [215] |> should equal [216]
        map [] |> should equal []

    [<Test>]
    let ``02 Fixed-function mapping, the hard way (part 2).`` () =
        let map (xs : int list) : int list =    //function which doubles each element
            let rec innerF (lst : int list) (ls : int list) (inc : int) : int list =    // inc used as length at first call, and list incerment in recursion 
                match inc with
                |0 -> ls            //Base Case: when the are no more iterations left, returns the altered list
                |a -> innerF lst ((lst.[(inc-1)]*2)::ls) (inc-1)   //InnerF (Original List, Updated list, Increment--)
            innerF xs [] xs.Length 

        map [1; 2; 3; 4] |> should equal [2; 4; 6; 8]
        map [9; 8; 7; 6] |> should equal [18; 16; 14; 12]
        map [15; 2; 7] |> should equal [30; 4; 14]
        map [215] |> should equal [430]
        map [] |> should equal []

   (*
      Well, that was repetitive!  The only thing that really changed
      between the functions was a single line.  How boring.

      Perhaps we could reduce the boilerplace if we just specified
      the transforming function, and left the rest of the structure
      intact?
   *)

    [<Test>]
    let ``03 Specified-function mapping, the hard way`` () =
        let map (f : 'a -> 'b) (xs : 'a list) : 'b list =       // a map which applies f to each element
            let rec innerF (lst : 'a list) (ls : 'b list) (inc : int) : 'b list =    // inc used as length at first call, and list incerment in recursion 
                match inc with
                |0 -> ls                //Base Case: when the are no more iterations left, returns the altered list
                |a -> innerF lst (( (f lst.[(inc-1)]))::ls) (inc-1)   //InnerF (Original List, Updated list, Increment--). applies function to element in updated list
            innerF xs [] xs.Length  

        map (fun x -> x+1) [9;8;7] |> should equal [10;9;8]
        map ((*) 2) [9;8;7] |> should equal [18;16;14]
        map (fun x -> sprintf "%.2f wut?" x)  [9.3; 1.22] |> should equal ["9.30 wut?"; "1.22 wut?"]

    // Hint: https://msdn.microsoft.com/en-us/library/ee370378.aspx
    [<Test>]
    let ``04 Specified-function mapping, the easy way`` () =
        List.map (fun x -> x+1) [9;8;7] |> should equal [10;9;8]
        List.map ((*) 2) [9;8;7] |> should equal [18;16;14]
        List.map (fun x -> sprintf "%.2f wut?" x)  [9.3; 1.22] |> should equal ["9.30 wut?"; "1.22 wut?"]
