namespace FluTe


module internal Parser = 
    open FParsec
    open FSharpx.Strings
    open FluTe.Core
    open FSharpx.Collections
    open System
    type Parser<'a> = Parser<'a, unit>
    [<Literal>]
    let TOKEN_START = '{'
    [<Literal>]
    let TOKEN_END = '}'
    [<Literal>]
    let ESCAPE = '`'
    [<Literal>]
    let NAME_SEP = ':'
    [<Literal>]
    let PROP_SEP = '.'
    [<Literal>]
    let EOS = '\u0000'
    let parseToken = 0
    // parse |->

    let inline (|?) (opt : _ option) dflt  = match opt with | Some t -> t | None -> dflt

    type ParsingException (message : string, tmplt : string,inner : Exception) = 
        inherit Exception(sprintf "The template, `%s` could not be parsed because:\n %s" tmplt message,inner)
        new (message : string, tmplt : string) = ParsingException(message,tmplt,null)
        
        member val Template = tmplt

    let failparse msg tmplt (inner : _ option) = ParsingException(msg,tmplt,inner |? null)
    let inline parseWith (p : _ Parser) (v : string) = 
        match run p v with
        | Success(par,_,_) -> par
        | Failure(par,_,_) -> raise <| failparse v par None

    let inline (|->) (inp : string) (p : _ Parser) = 
        inp |> parseWith p

    type SynToken = 
    | SynLiteral of string
    | SynActive of string

    let tokenizer : Parser<SynToken list>= 
        //This is a tokenizer that separates literal tokens from injection tokens. 
        let escapeInLiteral = pchar ESCAPE >>. anyChar |>> string //We parse `* and return the captured * char
        let literalBody = many1Chars (noneOf [ESCAPE; TOKEN_START]) //We parse any char except for ESCAPE or TOKEN_START
           
            
        //We combine the parsers so that when literalBody fails, escapeInLiteral starts if possible. 
        //This is repeated many times and the result is joined.
        let literal = many1Strings (literalBody <|> escapeInLiteral) |>> SynLiteral 

        //We simply capture all characters between TOKEN_START and TOKEN_END
        let token = pchar TOKEN_START >>. (manyChars (noneOf [TOKEN_END]) .>> (pchar TOKEN_END) |>> SynActive <??> "End of token")
  
        //The final tokenizer uses the above sub-units one after the other until end of input.
        many1 (literal <|> token)  
    open FluTe.Core.ProcessingSteps
    let identParser = identifier(IdentifierOptions())
    
    type PTokenA = {ShLabel : string}
    type PTokenB = {ShLabel : string; Props : string list}
    type PTokenC = {TkLabel : string; InLabel : string}
    type PTokenD = {TkLabel : string; InLabel : string; Props : string list}
      
    let tParserA = //Variety A is {__LABEL__}
        spaces >>. identParser .>> spaces .>> eof |>> fun lbl -> {PTokenA.ShLabel = lbl;}
    
    let tParserB = //Variety B is {__LABEL__(.PROP__)+}
        spaces >>. identParser .>> spaces 
        .>>. many1 (pchar PROP_SEP >>. spaces 
        >>. identParser .>> spaces) .>> eof |>> fun (lbl,prps) -> {ShLabel = lbl; Props = prps}

    let tParserC = //Variety C is {__TOKEN LABEL__:__INPUT LABEL__}
        spaces >>. identParser .>> spaces 
        .>> pchar NAME_SEP .>> spaces 
        .>>. identParser .>> spaces .>> eof |>> fun (tkn,inpt) -> {PTokenC.TkLabel = tkn; PTokenC.InLabel = inpt}

    let tParserD = //Variety D is {__TOKEN LABEL__:__INPUT LABEL__(.PROP__)+}
        spaces >>. identParser .>> spaces 
        .>> pchar NAME_SEP .>> spaces 
        .>>. identParser .>> spaces .>>. 
        many1 (pchar PROP_SEP >>. identParser .>> spaces) 
        .>> eof |>> fun ((tkn,inpt),prps) -> {InLabel = inpt; TkLabel = tkn; Props = prps}

    let (|TryParse|_|) (parser : Parser<_>) (str : string)= 
        match run parser str with
        | Success(v,_,_) -> Some v
        | Failure(_) -> None

    let (|TokenA|TokenB|TokenC|TokenD|Invalid|) (str : string) = 
        match str with
        | TryParse tParserA x -> TokenA x
        | TryParse tParserB x -> TokenB x
        | TryParse tParserC x -> TokenC x
        | TryParse tParserD x -> TokenD x
        | _ -> Invalid

    let parseBody str tmplt = 
        match str with
        | TokenA tok -> 
            TeActive(tok.ShLabel, {Input=tok.ShLabel;Steps=Deque.empty})
        | TokenB tok -> 
            let tokLabel = tok.ShLabel::tok.Props |> separatedBy "_"
            let steps = Deque.singleton (PropChainStep (tok.Props) :> IProcessingStep) 
            TeActive(tokLabel,{Input=tok.ShLabel;Steps=steps})
        | TokenC tok ->
            let tokLabel = tok.TkLabel
            let inpLabel = tok.InLabel
            TeActive(tok.TkLabel,{Input=tok.InLabel; Steps=Deque.empty})
        | TokenD tok ->
            let steps = Deque.singleton (PropChainStep (tok.Props) :> IProcessingStep)
            TeActive(tok.TkLabel,{Input=tok.InLabel; Steps = steps})
        | Invalid -> raise <| failparse (sprintf "The token `{%s}` could not be parsed." str) tmplt None

    let parseTemplate str = 
        let logTokens = 
            match run tokenizer str with
            | Success(synTokens,_,_) ->
                let parseToken (tok : SynToken) = 
                    match tok with
                    | SynLiteral(s) -> TeLiteral(s)
                    | SynActive(s) -> 
                        let tryParse = parseBody s str
                        tryParse
                [for tok in synTokens -> parseToken tok] |> TePrototype.Create
            | Failure(s,_,_) -> raise <| failparse s str None
        logTokens
    

            

           


        

    