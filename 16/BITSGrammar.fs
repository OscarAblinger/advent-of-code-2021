module BITSGrammar

open FParsec

/// <summary>
/// `count n parser` takes the next n elements and tries to parse them with `parser`.
/// It returns the result of `parser`.
/// </summary>
let private count (n: int) (parser: Parser<'r, 's>): Parser<'r, 's> = 
    fun (chStream: FParsec.CharStream<'s>) ->
        match runParserOnString parser chStream.UserState (chStream.Name + $">count({n})") (chStream.Read(n)) with
        | Success (result, userState, position) ->
            chStream.UserState <- userState
            Reply (result)
        | Failure (errorAsString, error, userState) ->
            chStream.UserState <- userState
            printfn "count resulted in error: %s" errorAsString
            Reply (ReplyStatus.Error, error.Messages)

type private PacketType =
    | LiteralType
    | OperatorType of int

type Packet =
    | LiteralPacket of version: int * value: int64
    | SumPacket of version: int * children: Packet list
    | ProductPacket of version: int * children: Packet list
    | MinimumPacket of version: int * children: Packet list
    | MaximumPacket of version: int * children: Packet list
    | GreaterThanPacket of version: int * children: Packet list
    | LessThanPacket of version: int * children: Packet list
    | EqualToPacket of version: int * children: Packet list

let (|OperatorPacket|_|) (packet: Packet) =
    match packet with
    | LiteralPacket _ -> None
    | SumPacket (version, children) -> Some (version, children)
    | ProductPacket (version, children) -> Some (version, children)
    | MinimumPacket (version, children) -> Some (version, children)
    | MaximumPacket (version, children) -> Some (version, children)
    | GreaterThanPacket (version, children) -> Some (version, children)
    | LessThanPacket (version, children) -> Some (version, children)
    | EqualToPacket (version, children) -> Some (version, children)

type private OperatorLength =
    | Bits of int
    | SubPackages of int

let private seqToInt (s: char seq) =
    System.Convert.ToInt32 (System.String.Concat s, 2)

let private seqToInt64 (s: char seq) =
    System.Convert.ToInt64 (System.String.Concat s, 2)

let private binary = anyOf "01"

let private threeBitNumber =
    parray 3 binary |>> seqToInt

let private version = threeBitNumber
let private typeId =
    threeBitNumber
    |>> fun typeId ->
        match typeId with
        | 4 -> LiteralType
        | id -> OperatorType id

let private literalPacketDataGroup = parray 4 binary

let private literalPacketData =
    many (pchar '1' >>. literalPacketDataGroup)
    .>>. (pchar '0' >>. literalPacketDataGroup)
    |>> fun (charsList, moreChars) ->
        (List.collect List.ofArray charsList) @ (List.ofArray moreChars)
        |> seqToInt64

let private operatorLengthDescriptorBits = 
    pchar '0' >>. parray 15 binary |>> fun bin -> Bits (seqToInt bin)

let private operatorLengthDescriptorSubPackages =
    pchar '1' >>. parray 11 binary |>> fun bin -> SubPackages (seqToInt bin)

let private operatorLengthDescriptor =
    operatorLengthDescriptorBits <|> operatorLengthDescriptorSubPackages

#nowarn "40"
let rec private operatorPacketData =
    operatorLengthDescriptor
    >>= fun length ->
        match length with
        | Bits n -> count n (many packet) 
        | SubPackages n -> parray n packet |>> List.ofArray
and private packet =
    version .>>. typeId
    >>= fun (version, typeId) ->
        match typeId with
        | LiteralType -> literalPacketData |>> fun value -> LiteralPacket (version, value)
        | OperatorType 0 -> operatorPacketData |>> fun packets -> SumPacket (version, packets)
        | OperatorType 1 -> operatorPacketData |>> fun packets -> ProductPacket (version, packets)
        | OperatorType 2 -> operatorPacketData |>> fun packets -> MinimumPacket (version, packets)
        | OperatorType 3 -> operatorPacketData |>> fun packets -> MaximumPacket (version, packets)
        | OperatorType 5 -> operatorPacketData |>> fun packets -> GreaterThanPacket (version, packets)
        | OperatorType 6 -> operatorPacketData |>> fun packets -> LessThanPacket (version, packets)
        | OperatorType 7 -> operatorPacketData |>> fun packets -> EqualToPacket (version, packets)
        | OperatorType id -> fail $"Unknown operator type '{id}' found"

let parse = runParserOnString packet () "base"
