{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module TH (mkVariableAdt) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import System.Directory (getCurrentDirectory)
import Control.Applicative
import Control.Monad.Trans.State (evalState, get, put)
import Data.List (groupBy, intercalate, intersperse, find)
import Data.Char (toUpper,toLower)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

-- R15 is 2000, R18 is 2004, R21 is 2007, R24 is 2010
data Version = Common | R13 | R14 | R15 | R18 | R21 | R24 deriving (Show,Eq,Enum)
type ConstructorName = String
type Field = String
type Constructor = (ConstructorName, [Field])

parseHeader :: Parser [Version]
parseHeader = ((string "R2007 Only" *> return [R21]) <|>
               (string "Common" *> return [Common]) <|>
               (string "R13-R14 Only" *> return [R13 .. R14]) <|>
               (string "R13-R15 Only" *> return [R13 .. R15]) <|>
               (string "Pre-2004 Only" *> return [R13 .. R15]) <|>
               (string "R2004+" *> return [R18 .. ]) <|>
               (string "R2007+ Only" *> return [R21 ..]) <|>
               (string "R2007+" *> return [R21 ..]) <|>
               (string "R13-R18" *> return [R13 .. R18]) <|>
               (string "R2000+ Only" *> return [R15 ..]) <|>
               (string "R2000+" *> return [R15 ..]) <|>
               (string "R2010+ Only" *> return [R24 ..]) <|>
               (string "R14+" *> return [R14 ..])) <* many (notChar '\n') <* endOfLine

parseType :: Parser Field
parseType = BS.unpack <$> (string "BS" <|> string "BD" <|> string "BLF" <|>
                           string "BL" <|> string "TV" <|> string "CMC" <|>
                           string "3BD" <|> string "2RD" <|>
                           string "RL" <|> string "RC" <|> string "WH" <|>
                           string "B" <|> string "H" <|> string "T")

parseName :: Parser ConstructorName
parseName = (\n e -> concat $ [n]++(if null e then e else ["_"]++e)) <$> many (letter_ascii <|> digit) <*> many parseExt
            where parsePar = skipSpace *> char '(' *> some (satisfy (inClass "A-Z")) <* char ')'
                  parseCaps = intercalate "_" <$> some (skipWhile (== ' ') *> some (satisfy (inClass "A-Z")))
                  parseExt = parsePar <|> parseCaps

parseLine :: Parser (ConstructorName, Field)
parseLine = (\t n -> (map toUpper n, t)) <$>
            (some space *> parseType <* parseSep) <*> (parseName <* many (notChar '\n') <* endOfLine)
            where parseSep = skipSpace *> char ':' <* skipSpace

parseFlags :: Parser (ConstructorName, Field)
parseFlags = do 
             (name, typ) <- parseLine
             case typ of
                -- basically just skip over flags
                "BL" -> some (do some space
                                 parseName
                                 skipSpace
                                 notChar ':'
                                 many (notChar '\n')
                                 endOfLine) *> return (name, typ)
                _ -> fail "cant parse flags"

parseSection :: Parser ([Version], [Constructor])
parseSection = do 
               let parseRecord = parseFlags <|> parseLine
                   foldMultilines = map (foldr (\(n, xs1) (_, xs2) -> (n, xs1:xs2)) ("",[]))
                   groupMultilines = groupBy (\(n, _) (n2, _) -> n2 == n)
               vs <- parseHeader 
               xs <- some parseRecord
               return (vs, foldMultilines $ groupMultilines xs)

parseSpec :: ByteString -> [([Version], [Constructor])]
parseSpec bs = case parseOnly (some parseSection) bs of
                Left e -> error e
                Right ss -> enumerateUnknowns ss
                    where enumerateUnknowns ss =
                             flip evalState 0 $ mapM (\(v, xs) -> (,) v <$>
                                  mapM (\(name, xs) -> case name of 
                                       "UNKNOWN" -> do n <- get
                                                       put (n + 1)
                                                       return $ (name ++ show n, xs)
                                       _ -> return (name, xs)) xs) ss

mkVariableAdt :: String -> String -> Q [Dec]
mkVariableAdt name version = do
    let parseVersion s = let (Just v) = find ((==) s . show) [R13 ..] in v
        mkCtor (name, fields) =
            normalC (mkName name) (map (\x -> strictType isStrict (conT (mkName ("DWG_"++x)))) fields)
        mkParser (name, fields) = foldr (\s b -> case s of 
                             "*" -> [|(<*>) $(b)|]
                             _   -> [|$(b) $(varE $ mkName "get")|]) [|fmap $(conE $ mkName name)|] $ intersperse "*" fields
        parserName = mkName $ "parse" ++ name ++ "s" ++ version

    curdir <- runIO $ getCurrentDirectory
    addDependentFile $ curdir ++ "/spec.txt"
    bs <- runIO $ BS.readFile "spec.txt"
    let spec = concat <$> map snd <$> filter (\(v, _) ->
               elem (parseVersion version) v || elem Common v) $ parseSpec bs
    [ValD _ body dec] <- [d|parse = $(listE $ map mkParser spec)|]
    let parser = ValD (VarP parserName) body dec
    decl <- dataD (cxt []) (mkName name) [] (map mkCtor spec) [''Show]
    return [decl, parser]
