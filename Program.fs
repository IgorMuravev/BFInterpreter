open System

let Code = "++++++++++[->++++++>++++++++++>+++<<<]>+++++>--->++>++[-<<<<+++++++++++++>>>>]+++++<<<<[->.+>.+>.>[->+>+<<]>>[-<<+>>]+<[>-<[-]]>[[-]++++++++++.<<++++++>>[-]]<<-<<<<]"
let Cmds = [| for c in Code do yield c |]
let mutable CmdCurrent = 0

let TapeLength = 1024;
let Tape = [|for i in 1..TapeLength do yield 0|]
let mutable current = 0;

let Next() = 
    if current < TapeLength then
        current <- current + 1
    ()

let Prev() =  
    if current > 0 then
        current <- current - 1
    ()

let Add() = 
    Tape.[current] <- Tape.[current] + 1
    ()

let Minus() =
    Tape.[current] <- Tape.[current] - 1
    ()

let Out() = 
    printf "%c" <| Convert.ToChar(Tape.[current])
    ()

let Input() =
    Tape.[current] <- (int)(Console.ReadKey().KeyChar)
    ()


let Forward() =
    let rec Move(bracket) =
        match Cmds.[CmdCurrent] with
            | ']' when bracket = 0 ->
                ()
            | ']' ->
                CmdCurrent <- CmdCurrent + 1
                Move(bracket - 1)
            | '[' ->
                CmdCurrent <- CmdCurrent + 1
                Move(bracket + 1)
            | _ -> 
                CmdCurrent <- CmdCurrent + 1
                Move(bracket)

    if Tape.[current] = 0 then
        CmdCurrent <- CmdCurrent + 1
        Move(0)

let Back() =
    let rec Move(bracket) =
        match Cmds.[CmdCurrent] with
            | '[' when bracket = 0 ->
                ()
            | '[' ->
                CmdCurrent <- CmdCurrent - 1
                Move(bracket + 1)
            | ']' ->
                CmdCurrent <- CmdCurrent - 1
                Move(bracket - 1)
            | _ ->
                CmdCurrent <- CmdCurrent - 1
                Move(bracket)

    if Tape.[current] <> 0 then
        CmdCurrent <- CmdCurrent - 1
        Move(0)


let Interpretation(cmd : char) =
    match cmd with
        | '>' -> Next()
        | '<' -> Prev()
        | '+' -> Add()
        | '-' -> Minus()
        | '.' -> Out()
        | ',' -> Input()
        | '[' -> Forward()
        | ']' -> Back()
        | _ -> 
            CmdCurrent <- CmdCurrent + 1
            ()

let rec Parse() = 
    if CmdCurrent < Cmds.Length then
        Interpretation(Cmds.[CmdCurrent])
        CmdCurrent <- CmdCurrent + 1
        Parse()
    ()
   

[<EntryPoint>]
let main argv = 
    Parse()
    0

