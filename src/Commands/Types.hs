module Commands.Types where

-- no camelcase here, 
-- because the parsers depend on the string representations of these type constructors
data BotCmd = Submit 
            | Get
            | New
            | Signup deriving (Eq, Show, Enum, Read)
