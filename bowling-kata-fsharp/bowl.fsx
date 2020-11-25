
module Bowling =
    type Frame =
        | Strike
        | Spare of int
        | Open of int * int
        | Tenth of int * int * Option<int>
        | Partial of int * Option<int>

    type Game = Frame list

    let firstThrow (frame: Frame) =
        match frame with
        | Strike -> 10
        | Spare throw -> throw
        | Open (throw, _) -> throw
        | Tenth (throw, _, _) -> throw
        | Partial (throw, _) -> throw

    let isPartial (frame: Frame) =
        match frame with
        | Partial _ -> true
        | _ -> false

    let (|PartialFrame|) frame = match frame with Partial _ -> true | _ -> false

    let scoreFrame (game: Game) =
        match game with
        | [] -> 0
        | [Strike] -> 10
        | Strike :: [Strike] -> 10 + 10
        | Strike :: Strike :: next :: _ -> 10 + 10 + firstThrow next
        | Strike :: Spare _ :: _ -> 10 + 10
        | Strike :: Open (first, second) :: _ -> 10 + first + second
        | Strike :: Tenth (first, second, _) :: _ -> 10 + first + second
        | Strike :: Partial (first, None) :: _ ->  10 + first
        | Strike :: Partial (first, Some second) :: _ -> 10 + first + second
        | [Spare _] -> 10
        | Spare _ :: next :: _ -> 10 + firstThrow next
        | Open (first, second) :: _ -> first + second
        | Tenth (first, second, None) :: _ -> first + second
        | Tenth (first, second, Some third) :: _ -> first + second + third
        | Partial (first, None) :: _ -> first
        | Partial (first, Some second) :: _ -> first + second

    let rec scoreGameAcc (game: Game) (acc: int) =
        match game with
        | [] -> acc
        | _ :: tail -> scoreGameAcc tail (acc + scoreFrame game)

    let scoreGame (game: Game) = scoreGameAcc game 0

    // TODO is this foldable?
    //let scoreGame2 (game: Game) = List.fold scoreFrame game 0

    let bowl (pinsInStrike: int) (game: Game) (throw: int) =
        match game with
        | [] ->
            if throw = pinsInStrike
            then [Strike]
            else [Partial(throw, None)]
        | Strike :: _
        | Spare _ :: _
        | Open _ :: _ ->
            if throw = pinsInStrike && game.Length <> 9
            then Strike :: game
            else Partial (throw, None) :: game
        | Tenth _ :: _ -> failwith "Can't bowl on a finished game"
        | Partial (first, None) :: _ ->
            match game.Length with
            | 10 ->
                if first = pinsInStrike || first + throw = pinsInStrike
                then Partial (first, Some throw) :: List.tail game
                else Tenth (first, throw, None) :: List.tail game
            | _ ->
                if first + throw = pinsInStrike
                then Spare first :: List.tail game
                else Open (first, throw) :: List.tail game
        | Partial (first, Some second) :: _ -> Tenth (first, second, Some throw) :: List.tail game

    // `pinsInStrike` is a small amount of complexity that allows for bowling with different rules (e.g. nine pin bowling)
    let bowlTenPin = bowl 10
    let bowlNinePin = bowl 9

    let parseGame (throws: int list) =
        throws |> List.fold bowlTenPin [] |> List.rev

open Bowling

let test (description: string) (throws: int list) (score: int) =
    let game = parseGame throws
    let calculatedScore = scoreGame game
    if score <> calculatedScore
    then printfn "%s: FAILED: Got score %d, expected %d, on game %A" description calculatedScore score throws
    else printfn "%s: PASSED" description

test "Initial strike test" [10; 5; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0] 28
test "Awful player test" [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] 0
test "Initial spare test" [0; 10; 5; 4; 0; 0; 0; 0; 0; 0; 0; 0] 24
test "Later strike test" [0; 0; 10; 5; 4; 0; 0; 0; 0; 0; 0; 0; 0] 28
test "Later spare test" [0; 0; 5; 5; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0] 26
test "Double strike test" [0; 0; 10; 10; 5; 2; 0; 0; 0; 0; 0; 0] (25 + 17 + 7)
test "Realistic (for me) score test" [2; 8; 10; 5; 4; 8; 2; 7; 2; 10; 5; 4; 10; 8; 1; 9; 1; 7] 147
test "Perfect game" [10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10] 300

// Thanks Stack Overflow
let read parser =
    Seq.initInfinite(fun _ -> System.Console.ReadLine())
    |> Seq.choose (parser >> function true, v -> Some v | _ -> None)

let rec bowlPartial (game: Game) =
    printfn "Current game: (score: %d) %A" (scoreGame (List.rev game)) (List.rev game)
    if game.Length = 10 && not (game |> Seq.head |> isPartial)
    then game
    else read System.Int32.TryParse |> Seq.head |> bowlTenPin game |> bowlPartial

let bowlGame = bowlPartial []
let myGame = bowlGame
printfn "All done! You scored %d. Your bowls: %A" (scoreGame (List.rev myGame)) (List.rev myGame)
