{-# LANGUAGE QuasiQuotes #-}

module Main
  where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Filesystem as FS
import Filesystem.Path
import qualified Filesystem.Path.CurrentOS as F
import Network.CGI
import Prelude hiding (FilePath)
import Safe
import System.Argv0
import System.IO hiding (FilePath)
import Text.CSV
import Text.Parsec hiding (getInput, (<|>))

import Str

dataFile :: IO FilePath
dataFile = do
    argv0 <- getArgv0
    return $ directory argv0 </> F.decodeString "data.csv"

contents :: String -> String
contents body = header ++ body ++ footer
  where
    header = [str|<html>
<head>
<title>CSV View</title>
</head>
<body>
<h1>CSV View</h1>
<h2>List</h2>
|]
    footer = [str|</body>
</html>
|]

makeform :: Record -> Int -> String
makeform header num = [str|<form method="POST" action=".">
<h2>Register</h2>
<table border=1>
|] ++ (h $ tail header) ++ tr (f 1 num) ++ [str|</table>
<br/>
<input type="submit" value="Register">
<input type="reset" value="Clear">
</form>
|]
  where
    input n = "<input type=\"text\" name=\"f" ++ show n ++ "\">"
    f n m
        | n <= m    = td (input n) ++ f (n+1) m
        | otherwise = ""

showTable :: Record -> CSV -> String
showTable header dat = table $ foldl (++) (h header) (map g dat)
  where
    g [] = ""
    g (r:rs) = tr $ foldl (++) "" (map td $ radio r:rs)

    table e = [str|<form method="POST" action=".">
<table border=1>
|] ++ e ++ [str|</table>
<input type="hidden" name="delete" value="true">
<input type="submit" value="Delete">
</form>
|]
    radio v = "<input type=\"radio\" name=\"id\" value=\"" ++ v ++ "\">"

tr :: String -> String
tr e = "<tr>" ++ e ++ "</tr>\n"
td :: String -> String
td e = "<td>" ++ e ++ "</td>"
th :: String -> String
th e = "<th>" ++ e ++ "</th>"
h :: Record -> String
h [] = ""
h rs = tr $ foldl (++) "" (map th rs)

loadCSV :: IO (Record, CSV, Int)
loadCSV = do
    path <- dataFile
    FS.withFile path ReadMode $ \file -> do
        hline <- hGetLine file
        let hres = parse csv "header" $ encodeString hline
        header <- either (fail . show) (return . head) hres
        other <- hGetContents file
        let result = parse csv "csv parse" $ other
        csvret <- either (fail . show) (return . initIf (==[""])) result
        nid <- maybe (fail "newId Error") return $ newId csvret
        return (header, csvret, nid)
  where
    initIf :: (a -> Bool) -> [a] -> [a]
    initIf _ []     = []
    initIf f [x]
        | f x       = []
        | otherwise = [x]
    initIf f (x:xs) = x:initIf f xs

    newId :: CSV -> Maybe Int
    newId = newId' 0
      where
        newId' l []        = Just l
        newId' l (f:fs) = headMay f >>= readMay >>= \val ->
            if l >= val
                then newId' l fs
                else newId' val fs

getInput' :: MonadCGI m => String -> MaybeT m String
getInput' query = do
    val <- lift $ getInput query
    maybe (fail "query not found") return val

getNewRecord :: MonadCGI m => Int -> Int -> MaybeT m Record
getNewRecord newId num = do
    record <- getRecord num []
    return $ show newId:record
  where
    getRecord :: MonadCGI m => Int -> [Field] -> MaybeT m Record
    getRecord 0 ret = return ret
    getRecord n ret = do
        field <- getField n
        getRecord (n-1) (field:ret)

    getField :: MonadCGI m => Int -> MaybeT m Field
    getField n = getInput' $ "f" ++ show n

addRecord :: MonadCGI m => Int -> Int -> CSV -> MaybeT m CSV
addRecord num newId records = do
    new <- getNewRecord newId num
    return $ records ++ [new]

deleteRecord :: MonadCGI m => CSV -> MaybeT m CSV
deleteRecord records = do
    getInput' "delete"
    delId <- getInput' "id"
    maybe (fail "delete id not found") return $ delete records delId
  where
    delete :: CSV -> String -> Maybe CSV
    delete []     _      = Nothing
    delete (s:ss) delseq = do
        sseq <- headMay s
        if sseq == delseq
          then Just ss
          else (s:) <$> delete ss delseq

update :: Record -> CSV -> IO CSV
update header new = do
    let newData = header:new
    dataFile >>= (flip FS.writeFile $ BC.pack $ printCSV newData)
    return new

main :: IO ()
main = runCGI $ handleCGI (output . contents . show) $ do
    (header, dat, lastId) <- liftIO loadCSV
    let len = (length header) - 1
    ret <- runMaybeT $ addRecord len (lastId + 1) dat <|> deleteRecord dat
    dat2 <- maybe (return dat) (liftIO . update header) ret
    setHeader "Content-type" "text/html; charset=UTF-8"
    output $ contents $ showTable header dat2 ++ makeform header len
  where
    handleCGI = flip catchCGI

