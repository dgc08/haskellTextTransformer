# Text Transformer -  A library to wrap your (String -> String) functions into a CLI interface
A small Library to manage the non-pure part of programs like compilers or parsers, so you can focus on the pure functions.

`String -> String` functions are called transformers in this documentation.

## Using the executable
`yourExecutable [inputFile] [outputFile] [command]`

`[inputFile]` can also be set to `-i`, which will read the input from `stdin` until EOF.
For `[outputFile]` you can use `-o` to output to `stdout`.

`[command]` is only needed if you use `transformTable`.

If you use `transformTable`, you can also use the command format `yourExecutable [command]` for `const` Strings, for example for a `help`-command. Take a look at `src/Test.hs` for an example.

## Using the library
For now, you can just download `src/Transformer.hs`, as the library is just one file.

## Usage of `transform`
`transform :: (String -> String) -> IO ()`
You supply just a transformer, which will then be executed with the specified input and output.

Example:

``` haskell
wordsToLines :: String -> String
wordsToLines s = unlines $ words s

main = transform wordsToLines
```

## Usage of `transformTable`
`transformerTable :: (String -> (String -> String)) -> IO ()`

Instead of supplying a transformer directly, you supply a function, which returns a transformer for a given command. 

Example:

Take a look at `src/Test.hs` for a full example.

``` haskell
wordsToLines :: String -> String
getTransformer :: String -> (String -> String)

wordsToLines s = unlines $ words s

getTransformer "wl" = wordsToLines
getTransformer "1" = (++ "test1\n")
getTransformer x = errorWithoutStackTrace $ "Error: Unknown option '" ++ x ++ "'"

main = transformerTable getTransformer

```
