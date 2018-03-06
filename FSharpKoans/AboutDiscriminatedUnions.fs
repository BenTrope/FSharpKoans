namespace FSharpKoans
open NUnit.Framework


//Simillar to data struct named Records

(*
    A discriminated union is a disjoint set of named cases, where each case
    may have some linked/associated data.  If a discriminated union case has
    associated data, the case name is a function which takes the associated
    data as input and gives a value of the discriminated union type as output.
*)

module ``10: The Good Kind of Discrimination`` = 
    open System

    type Subject = // <-- feel free to add your own subjects!
    | Philosophy
    | Linguistics
    | ComputerScience
    | Mathematics
    | Economics
    | Management
    | English
    | SoundTechnology
    | Accounting

    type UndergraduateDegree = 
    | BSc of Subject * Subject
    | BCom of Subject * Subject
    | BPharm
    | BA of Subject * Subject

    type PostgraduateDegree =
    | Honours of Subject
    | Masters of Subject

    [<Test>]                                                        /// Matches whether a subject is undergrad or not, sneaky union work with how 
                                                                    /// One needs to look of how each degree is defined ie. Bpharm has nothing vs Bsc with two
    let ``01 A case isn't the same as a type`` () = 
        let aDegree = BSc (Linguistics, ComputerScience)
        let anotherDegree = BPharm
        let philosopherKing = Masters Philosophy
        aDegree |> should be ofType<UndergraduateDegree> 
        anotherDegree |> should be ofType<UndergraduateDegree> 
        philosopherKing |> should be ofType<(PostgraduateDegree)> 
                                                                    /// look at ling of what a subject is defined by
    [<Test>]
    let ``02 Creating & pattern-matching a discriminated union`` () = 
        let randomOpinion degree =
            match degree with
            | BSc (_, ComputerScience) | BSc (ComputerScience, _) -> "Good choice!"
            | BSc _ -> "!!SCIENCE!!"
            | BPharm -> "Meh, it's OK."
            | BCom (Economics, Accounting ) -> "Money, money, money."
            | BA (English, Philosophy) -> "A thinker, eh?"
        randomOpinion (BSc (ComputerScience, Mathematics )) |> should equal "Good choice!" // Has to contain one computer science, biased much?
        randomOpinion (BSc (Mathematics, SoundTechnology) )|> should equal "!!SCIENCE!!"   // has to contain any two combo of subjects
        randomOpinion (BCom (Economics, Accounting)) |> should equal "Money, money, money." //COPY PASTE
        randomOpinion (BCom (Economics, Accounting)) |> should equal "Money, money, money." //COPY PASTE
        randomOpinion (BA (English, Philosophy)) |> should equal "A thinker, eh?"   //COPY PASTE
        randomOpinion (BPharm)|> should equal "Meh, it's OK." // I mean they friendly tho

    type EquipmentStatus =
    | Available
    | Broken of int // takes an int, gives back en EquipmentStatus
                   
    | Rented of string  
                                                                            //if a discrimated case has associated data, said data becomes the 
                                                                            //input of Function and outputs the value of discrimated union
    [<Test>]
    let ``03 A discriminated union case with associated data is a function`` () =
        Broken |> should be ofType<int -> EquipmentStatus>                                      // associated data is in, discrimated union is Equipment status            
        Rented |> should be ofType<string -> EquipmentStatus>                          // assocated data is a string


        // Explanation of binary trees https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions
        (* 
        Discrimated unions can be recursive, meaning that the union itself can be
        included in the type of one or more cases.
        Recursive discrimated unions can be used to create tree structures, which are used to model sxpressions
        in programming languages. In the following code, recusive discrimated union is used
        to create a binary tree data structure. The union consists of two cases:
        Node: which is a node with a string to the right AND left of current value
        Empty: terminates the tree



        *)

    type BinaryTree =
    | Empty
    | Node of string * BinaryTree * BinaryTree

    [<Test>]
    let ``04 A discriminated union can refer to itself (i.e., it can be recursive).`` () =
        let rec depth x =
            match x with
            | Empty -> 0
            | Node (_, a, b) -> 1 + max (depth a) (depth b)
        let a = Node("0", Node("1", Node("2", Node("End",Empty, Empty), Empty), Node("3", Empty, Empty)), Node("4", Empty, Empty)) // <-- you may want to spread this over multiple lines and/or let-bindings ...!
        depth a |> should equal 4
     
     
    

     
