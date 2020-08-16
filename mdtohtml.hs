import System.Environment

-- doChain
   -- doTaggedLine
      -- switchTag
      -- doEndTag
   -- doHtmlFormat
      -- doHeader
         -- doTitle
         -- doStyles
            -- generateStyles
         -- placeAround
      -- placeAround

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args in do
    content <- readFile (head args)
    -- "lof" means lines of file
    let lof = lines content in
      writeFile (filePath ++ ".html") $ concat $ doChain [filePath] lof (tail args)

doChain :: [String] -> [String] -> [String] -> [String]
doChain title xs styles = [x ++ "\n" | x <- doHtmlFormat title [doTaggedLine x | x <- xs] styles]

doTaggedLine :: String -> String
doTaggedLine line = begtag ++ trim ++ endtag where
  -- Break the string into tokens by ' '
  xs = words line
  -- Get the first token ('#' in "# Example") and create a tag out of it
  begtag = switchTag $ if null xs then "" else head xs
  -- Generate an end tag, if the tag requires one
  endtag = if begtag == "<br>" then "" else doEndTag begtag
  -- If the line has content (e.g. not a break) reassemble the tokens
  conts = if begtag == "<br>" then "" else concat [x ++ " " | x <- tail xs]
  -- If the line has content, trim the trailing whitespace
  trim = if null conts then "" else init conts

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

doEndTag :: String -> String
-- Create an end tag by inserting a '/' after the first character
doEndTag x = [head x] ++ "/" ++ tail x

doHtmlFormat :: [String] -> [String] -> [String] -> [String]
doHtmlFormat title xs styles = ["<!DOCTYPE html>"] ++ placeAround (doHeader title styles ++ placeAround xs "<body>") "<html>"

doHeader :: [String] -> [String] -> [String]
doHeader title styles = placeAround (doTitle title ++ doStyles styles) "<head>"

doTitle :: [String] -> [String]
doTitle title = [concat $ ["<title>"] ++ title ++ ["</title>"]]

doStyles :: [String] -> [String]
doStyles xs = [generateStyles x | x <- xs]

generateStyles :: String -> String
-- Add your own style macros here!
generateStyles ":dark" = "<style>* {background-color: black; color: white;}</style>"
generateStyles ":light" = "<style>* {background-color: white; color: black;}</style>"
generateStyles ":mono" = "<style>* {font-family: monospace;}</style>"
generateStyles x = "<link rel=\"stylesheet\" href=\"" ++ x ++ "\">"

placeAround :: [String] -> String -> [String]
-- Sorround an indented "block" with a tag and the end tag of that tag
placeAround xs x = [x] ++ ["\t" ++ x | x <- xs] ++ [doEndTag x]
