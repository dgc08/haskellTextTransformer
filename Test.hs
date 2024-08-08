import Transformer

getTransformer :: String -> (String -> String)
getTransformer "1" = (++ "test1\n")
getTransformer "2" = (++ "test2\n")
getTransformer x = (\_ -> x)

main = transformerTable getTransformer
