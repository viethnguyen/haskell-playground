-- This code gets information from a conference website. 

import Network.HTTP
import Text.HTML.TagSoup    

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

main = do
  src <- openURL "http://ubicomp.org/ubicomp2015/program/program.html"
  let tags = parseTags src
  let papers = sections (~== "<p>") $ 
               takeWhile (~/= "Poster") $
               dropWhile (~/=  "Papers and Notes") tags
  let titles = map getTitles $ papers
  let authors = map getAuthors $ papers
  putStr $ unlines $ take 20 titles
      where getAuthors :: [Tag String] -> String
            getAuthors = dequote . unwords . words . fromTagText . head . tail . filter isTagText
            getTitles :: [Tag String] -> String
            getTitles = dequote . unwords . words . fromTagText . head . filter isTagText
            dequote ('\"':xs ) | last xs == '\"' = init xs
            dequote x = x 






