module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

listOfReservedNames = ["true","false","skip","if", "then","else","end", "while","do", "repeat", "input", "print", "tic", "sin", "cos", "tan", "ceil", "floor", "pi", "round"]

listOfReservedOPNames = ["+", "-", "*", "/", "<", ">", "&", "|", "=", ";", "~", ":="]

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = listOfReservedNames
                                  , reservedOpNames = listOfReservedOPNames
                                   }
                                 )

----------------------------------
--- Parser de expresiones
-----------------------------------
expParser :: Parser Exp
expParser = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens lis expParser)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> try (do reserved lis "pi"
                     return Pi)
         <|> try (do reserved lis "sin"
                     e <- factor
                     return (Sin e))
         <|> try (do reserved lis "cos"
                     e <- factor
                     return (Cos e))
         <|> try (do reserved lis "tan"
                     e <- factor
                     return (Tan e))
         <|> try (do reserved lis "ceil"
                     e <- factor
                     return (Ceil e))
         <|> try (do reserved lis "floor"
                     e <- factor
                     return (Floor e))
        <|> try (do reserved lis "round"
                    e <- factor
                    f <- factor
                    return (Round e f))
         <|> try (do n <- float lis
                     return (DoubleConst n))
         <|> try (do n <- integer lis
                     return (Const n))
         <|> try (do str <- identifier lis
                     return (Var str))

multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div

addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus

-----------------------------------
--- Parser de expresiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp lis "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp lis "&"
                                     return And))

boolexp3 = try (parens lis boolexp)
           <|> try (do reservedOp lis "~"
                       b <- boolexp3
                       return (Not b))
           <|> expcomp
           <|> boolvalue

expcomp = try (do i <- expParser
                  c <- compopp
                  j <- expParser
                  return (c i j))

compopp = try (do reservedOp lis "="
                  return Eq)
          <|> try (do reservedOp lis "<"
                      return Lt)
          <|> try (do reservedOp lis ">"
                      return Gt)

boolvalue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp lis ";"
                              return Seq))

comm2 = try (do reserved lis "skip"
                return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    reserved lis "then"
                    case1 <- comm
                    reserved lis "else"
                    case2 <- comm
                    reserved lis "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved lis "repeat"
                    c <- comm
                    reserved lis "until"
                    cond <- boolexp
                    reserved lis "end"
                    return (Repeat c cond))
        <|> try (do reserved lis "input"
                    str <- identifier lis
                    return (Input str))
        <|> try (do reserved lis "print"
                    txt <- optionMaybe (stringLiteral lis)
                    e <- printParse
                    return (Print txt e))
        <|> try (do reserved lis "tic"
                    op <- opParse
                    return (Tic op))
        <|> try (do str <- identifier lis
                    reservedOp lis ":="
                    e <- expParser
                    return (Let str e))

printParse = try opParse <|> try expParser

opParse = try (do reservedOp lis ":="
                  return (Var ":="))
          <|> try (do reserved lis "if"
                      return (Var "if"))
          <|> try (do reserved lis "repeat"
                      return (Var "repeat"))
          <|> try (do reserved lis "input"
                      return (Var "input"))
          <|> try (do reserved lis "print"
                      return (Var "print"))
          <|> try (do reservedOp lis "-"
                      return (Var "-"))
          <|> try (do reservedOp lis "+"
                      return (Var "+"))
          <|> try (do reservedOp lis "*"
                      return (Var "*"))
          <|> try (do reservedOp lis "/"
                      return (Var "/"))
          <|> try (do reserved lis "sin"
                      return (Var "sin"))
          <|> try (do reserved lis "cos"
                      return (Var "cos"))
          <|> try (do reserved lis "tan"
                      return (Var "tan"))
          <|> try (do reserved lis "ceil"
                      return (Var "ceil"))
          <|> try (do reserved lis "floor"
                      return (Var "floor"))
          <|> try (do reserved lis "pi"
                      return (Var "pi"))
          <|> try (do reservedOp lis "="
                      return (Var "="))
          <|> try (do reservedOp lis "<"
                      return (Var "<"))
          <|> try (do reservedOp lis ">"
                      return (Var ">"))
          <|> try (do reservedOp lis "&"
                      return (Var "&"))
          <|> try (do reservedOp lis "|"
                      return (Var "|"))
          <|> try (do reservedOp lis "~"
                      return (Var "~"))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
