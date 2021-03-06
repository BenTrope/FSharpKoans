﻿namespace FSharpKoans
open NUnit.Framework

(*
    In functional programming, we often care about the "shape" of data, rather
    than the data itself.  For example, if we are trying to find the length of a list,
    it doesn't matter whether we have a list-of-strings or a list-of-floats or a
    list-of-something-elses.  All that matters is that the data occurs in the shape
    of a list.  Similarly, if we are given a tuple of two items and we only look at
    the second item, then the type of the first item doesn't matter to us - it
    could be anything, and the meaning of the program wouldn't change at all!
    
    But what if we are given a tuple of 4 items, and our function outputs the
    third item?  Now we have an interesting situation.  We hope to be able to
    take in a tuple of ANY four types, but output a value that has the type of
    the third item.  We can achieve this if we specify the type of the tuple
    generically, as
    
        'a * 'b * 'c * 'd

    This gives each component of the tuple a "fake" type (indicated by the ' symbol
    before the type-name).  The function returns the third element, so we can
    say that the type of the function is

        'a * 'b * 'c * 'd -> 'c

    ...and now we have a perfectly generic function, which can be use with any
    4-tuple, and which is also completely safe to use.  And as a side-effect,
    by looking at the function type, we can tell what the function does without
    even looking at the code of the function!  The function cannot know what
    'a, 'b, 'c, or 'd types are, and it cannot know how to construct them.  The
    only way that it can get hold of a 'c value is by having it passed in.  Since
    we know this to be true, it must be the case that the function is returning
    the third item in the tuple [0]!

    The ability to look at the *shape* of data, and ignore unnecessarily-specific
    types, is called "parametric polymorphism".  The non-specific (or "generic")
    types are given as 'a, 'b, 'c, and so on.  We sometimes say that types which are
    parametrically polymorphic are "generic types".

    We may sometimes want to make our more structured types, like records
    or discriminated unions, generic.
*)

module ``11: Parametric polymorphism`` =
    [<Test>]
    let ``01 id: the simplest built-in generic function`` () =
        let id x = x

        id 8 |> should equal 8
        id 7.6 |> should equal 7.6
        id "wut!" |> should equal "wut!"
        // id can be surprisingly useful.  Remember it :).
        
    [<Test>]
    let ``02 Defining a generic function`` () =                                         //check f# interactive
        let f (x :'a) (y : 'b )  = (x , y , y)                                          //issssaTUPLE!!!
        f 4 5 |> should equal (4, 5, 5)
        f "k" 'p' |> should equal ("k", 'p', 'p')

    // this is how we might define a record type with two generic fields.
    type GenericRecordExample<'a,'b> = {
        Something : 'a
        Blah : int
        Otherwise : 'b
        What : 'a * string * 'b
    }
    // we might create this with: { Something=5; Blah=8; Otherwise=9.3; What=77,"hi",0.88 }

    type MyRecord<'a, 'b, 'c> = {
        Who : 'a // <-- should be generic
        What : 'b // <-- should be generic, and a different type to Who
        Where : 'c

    }   
    //  let book = { Title="Dune"; Author="Frank Herbert"; Year=1965 }

    [<Test>]
    let ``03 Creating a generic record`` () =
        // You need to edit the definition of MyRecord first!  It's just above this test.
        let a = {Who = "The Doctor"; What = 4.53 ; Where = "TTFN"  }
        let b =  {Who = 'R'; What = false ; Where = "tiffin"}
        a.Who |> should equal "The Doctor"
        b.Who |> should equal 'R'
        a.What |> should equal 4.53
        b.What |> should equal false
        a.Where |> should equal "TTFN"
        b.Where |> should equal "tiffin"
     
    type GenericDiscriminatedUnionExample<'a,'b> =
    | Frist
    | Secnod of 'a * 'b
    | Thrid of ('a -> ('b * 'a * int)) // <-- this shouldn't look odd.  Functions are first-class!

    [<Test>]
    let ``04 Creating a generic discriminated union (Part 1).`` () =
        let a = Secnod (6.55, 7)
        let b = Thrid (fun k -> true, k, 8)
        // how do you write a generic type?                     //GOOGLE PORFAVOR
        a |> should be ofType
        b |> should be ofType

    type MyDiscriminatedUnion <'a, 'b > =
    | Furoth of 'a
    | Fevi
    | Sxi of 'b

    [<Test>]
    let ``05 Creating a generic discriminated union (Part 2).`` () =
        // You need to edit the definition of MyDiscriminatedUnion first!  It's just above this test.
        let a = Furoth 7
        let b = Sxi "bleh"
        let c = Furoth 't'
        let d = Sxi true
        match a with
        | Furoth n -> n |> should equal 7
        | _ -> Assert.Fail ()
        match b with
        | Sxi x -> x |> should equal "bleh"
        | _ -> Assert.Fail ()
        match c with
        | Furoth p -> p |> should equal 't'
        | _ -> Assert.Fail ()
        match d with
        | Sxi y -> y |> should equal true
        | _ -> Assert.Fail ()
