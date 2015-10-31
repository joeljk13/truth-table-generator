import Data.List
import System.Environment

expr a b c = a `implies` b || c

table = [ [a, b, c, expr a b c] | a <- bools, b <- bools, c <- bools ]

bools :: [Bool]
bools = [True, False]

iff :: Bool -> Bool -> Bool
iff = (==)

implies :: Bool -> Bool -> Bool
a `implies` b = not a || b

separate :: a -> [a] -> [a]
separate x xs = x : intersperse x xs ++ [x]

text :: [[String]] -> String
text table = concat $ separate hline tableRows
  where
    tableRows = map (concat . (\l -> separate "|" l ++ ["\n"])) table
    hline = replicate (maximum (map length tableRows) - 1) '-' ++ "\n"

textBool :: Bool -> String
textBool b = if b then " T " else " F "

textTable :: [[Bool]] -> [[String]]
textTable table = map (map textBool) table

latex :: [[String]] -> String
latex table = header ++ (concat $ separate hline tableRows) ++ footer
  where
    tableRows = map (\l -> "\t" ++ concat (intersperse " & " l) ++ " \\\\\n") table
    hline = "\t\\hline\n"
    cols = length (table !! 0)
    header = "\\begin{tabular}{" ++ separate '|' (replicate cols 'c') ++ "}\n"
    footer = "\\end{tabular}\n"

latexBool :: Bool -> String
latexBool b = if b then "T" else "F"

latexTable :: [[Bool]] -> [[String]]
latexTable table = map (map latexBool) table

usage :: String
usage = "gen [--latex]\n"

textMain :: String
textMain = text (textTable table)

latexMain :: String
latexMain = latex (latexTable table)

main' :: [String] -> String
main' args = case args of
    ["--help"] -> usage
    ["--latex"] -> latexMain
    ["--text"] -> textMain
    [] -> textMain
    _ -> usage

main = do
    args <- getArgs
    putStr (main' args)
