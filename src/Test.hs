import Transformer

wordsToLines :: String -> String
wordsToLines s = unlines $ words s

getTransformer :: String -> (String -> String)

getTransformer "wl" = wordsToLines
getTransformer "1" = (++ "test1\n")
getTransformer "2" = (++ "test2\n")

-- you can also use '= \_ -> "help message"', this is just a transformer that always returns the same string
getTransformer "help" = const "This is a stub help message; help is implemented as a normal transformer\n"

getTransformer x = errorWithoutStackTrace $ "Error: Unknown option '" ++ x ++ "'"

main = transformerTable getTransformer
