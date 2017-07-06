module Parser
    ( play
    ) where


-- import Text.Parsec
-- import Text.Parsec.String
import Text.ParserCombinators.Parsec
import ParserDS


play :: String -> Either ParseError String
play s = parse rootParser "parameter" s
-- play s = parse pmain "parameter" s


colon :: GenParser Char st Char
colon  = char ':'

-- word :: Parser String
word :: GenParser Char st String
word = many1 letter
-- all-streams : streams
-- <label> : <type>

rootParser :: GenParser Char st String
rootParser = word >> space >> colon >> space >> typeParser -- return "hi"

typeParser :: GenParser Char st String
typeParser = (try $ string "stream"            >> return "Estate")
         <|> (try $ string "transition-stream" >> return "Duchy")
         <|> (try $ string "constraints"       >> return "Duchy")
         <|> (try $ string "block"             >> return "Duchy")
         <|> (try $ string "base-stream"       >> return "Duchy")
         <|> (try $ string "experiment"        >> return "Duchy")



-- TODO: represent blocks (as scopes probably)
-- They're a list with indicies to apply to and a list of constraints to apply



-- list of names that are in scope

-- root parser
-- everything we encounter at the root better be
-- <label> : <type>
-- label is a string, type is: stream, transition-stream, constraints, block, base-stream, experiment

-- stream Parser
-- label : stream
-- builds a list of lists (flattened rememeber)
-- everyline is <# whitespace>, <label>, <newline>

-- base-stream Parser
-- <label> : base-stream
-- next line, indent then, "cross" then space sep'd list of *top level* labels we know from from streams

-- constraint Parser
-- <label> : constraints
--     max-in-a-row 7 (allOf <stream>)  stream must be a name we know from before. Not sure how to handle root/ leaf always
-- valid keywords: max-in-a-row, etc more later


-- TODO: don't do this one for now
-- experiment Parser
-- <label> : experiment
-- next is these blocks but let's ignore them for now...
-- keywords: fully-cross, apply


-- so:
-- root parser gets type and calls the right parser after that on the next line
