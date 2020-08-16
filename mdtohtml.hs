import System.Environment

doTitle :: [String] -> [String]
doTitle title = [concat $ ["<title>"] ++ title ++ ["</title>"]]

doHeader :: [String] -> [String] -> [String]
doHeader title styles = placeAround (doTitle title ++ doStyles styles) "<head>" 

doBody :: [String] -> [String]
doBody xs = placeAround xs "<body>"

placeAround :: [String] -> String -> [String]
placeAround xs x = [x] ++ ["\t" ++ x | x <- xs] ++ [doEndTag x]

generateStyles :: String -> String
generateStyles ":dark" = "<style>* {background-color: black; color: white;}</style>"
generateStyles ":light" = "<style>* {background-color: white; color: black;}</style>"
generateStyles ":mono" = "<style>* {font-family: monospace;}</style>"
generateStyles x = "<link rel=\"stylesheet\" href=\"" ++ x ++ "\">"

doStyles :: [String] -> [String]
doStyles xs = [generateStyles x | x <- xs]

doHtmlFormat :: [String] -> [String] -> [String] -> [String]
doHtmlFormat title xs styles = ["<!DOCTYPE html>"] ++ placeAround (doHeader title styles ++ doBody xs) "<html>" 

doEndTag :: String -> String
doEndTag x = [head x] ++ "/" ++ tail x

switchTag :: String -> String
switchTag "p" = "<p>"
switchTag "#" = "<h1>"
switchTag "##" = "<h2>"
switchTag "###" = "<h3>"
switchTag "####" = "<h4>"
switchTag "#####" = "<h5>"
switchTag "######" = "<h6>"
switchTag "*" = "<i>"
switchTag "**" = "<b>"
switchTag "~" = "<s>"
switchTag "`" = "<q>"
switchTag "```" = "<blockquote>"
switchTag "br" = "<br>"
switchTag "" = "<br>"
switchTag xs = xs

doTag :: String -> String
doTag s = begtag ++ trim ++ endtag where
  xs = words s
  begtag = switchTag $ if null xs then "" else head xs
  endtag = if begtag == "<br>" then "" else doEndTag begtag
  conts = if begtag == "<br>" then "" else concat [x ++ " " | x <- tail xs]
  trim = if null conts then "" else init conts

doChain :: [String] -> [String] -> [String] -> [String]
doChain title ls styles = doNewLine $ doHtmlFormat title [doTag l | l <- ls] styles

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args in do 
    content <- readFile (head args)
    let linesOfFile = lines content in do
      writeFile (filePath ++ ".html") $ concat $ doChain [filePath] linesOfFile (tail args) 

-- doChain
   -- doTag
      -- switchTag
      -- doEndTag
   -- doHtmlFormat
      -- doHeader
         -- doTitle
         -- doStyles
            -- generateStyles
         -- placeAround
      -- doBody
         -- placeAround
      -- placeAround
   -- doNewLine
