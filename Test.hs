import Transformer

getTransformer "1" = (++ "test1\n")
getTransformer "2" = (++ "test2\n")

main = transformerTable getTransformer
