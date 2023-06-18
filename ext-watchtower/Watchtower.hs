{-# LANGUAGE OverloadedStrings #-}

module Watchtower where

import Terminal
import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server
import qualified System.IO (hPutStrLn, stdout)
import qualified System.FilePath as Path
import qualified System.Directory as Dir

import qualified Reporting.Exit as Exit
import qualified Elm.Docs as Docs
import qualified Json.Encode
import qualified Ext.Dev
import qualified Terminal.Helpers (package, version)
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Stuff



main :: IO ()
main =
  Terminal.app intro outro
    [ start
    , docs
    ]



intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        [ "Hi,","thank","you","for","trying","out"
        , P.green "Elm Dev."
        , " I hope you like it!"
        ]
    , ""

    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Happy hacking!"



{- Get the docs for a package, a local project, or a specific file. -}



data DocFlags =
  DocFlags
    { _output :: Maybe String
    }

data DocsArgs
    = DocsCwdProject
    | DocsFile FilePath
    | DocsPackage Pkg.Name
    | DocsPackageVersion Pkg.Name V.Version 


{-|

You can provide:

  A package: mdgriffith/elm-ui
  An Elm file: Tests.elm
  Or an elm.json file


-}
docs :: Terminal.Command
docs =
  let
    summary =
      "Report the docs.json for the given package"

    details =
      "The `docs` command generates docs.json for :"

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:51213>\
        \ and ready to be connected to."

    docsFlags =
      flags DocFlags
        |-- flag "output" output_ "An optional file to write the JSON form of docs to.  If not supplied, the docs will be printed."

    docsArgs =
      oneOf 
        [ require0 DocsCwdProject
        , require1 DocsPackage Terminal.Helpers.package
        , require2 DocsPackageVersion Terminal.Helpers.package Terminal.Helpers.version
        , require1 DocsFile dir
        ]
  in
  Terminal.Command "docs" (Common summary) details example docsArgs docsFlags getDocs




output_ :: Parser String
output_ =
  Parser
    { _singular = "output"
    , _plural = "output"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json"]
    }




getDocs :: DocsArgs -> Watchtower.DocFlags -> IO ()
getDocs arg (Watchtower.DocFlags maybeOutput) =
  case arg of
    DocsCwdProject ->
      do
          let path = "src/Main.elm"
          maybeRoot <-  Dir.withCurrentDirectory (Path.takeDirectory path) Stuff.findRoot
          case maybeRoot of
            Nothing ->
              System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
            
            Just root -> do
              maybeDocs <- Ext.Dev.docsForProject root path
              case maybeDocs of
                Left err ->
                  System.IO.hPutStrLn System.IO.stdout 
                      (Exit.toString (Exit.reactorToReport err))
                
                Right docs ->
                  System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encodeUgly (Docs.encode docs)))

    DocsFile path ->
      if Path.takeExtension path == ".elm" then
        do
          maybeRoot <-  Dir.withCurrentDirectory (Path.takeDirectory path) Stuff.findRoot
          case maybeRoot of
            Nothing ->
              System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
            
            Just root -> do
              maybeDocs <- Ext.Dev.docs root path
              case maybeDocs of
                Nothing ->
                  System.IO.hPutStrLn System.IO.stdout "Docs are not available"

                Just docs ->
                  System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encodeUgly (Docs.encode (Docs.toDict [ docs ]))))
      else
        -- Produce docs as a project
        System.IO.hPutStrLn System.IO.stdout "Given file does not have an .elm extension"

    DocsPackage packageName ->
        -- Is there an `elm.json` file?  Read it for the exact version.
        -- Otherwise use the latest version.
        System.IO.hPutStrLn System.IO.stdout "Package docs"

    DocsPackageVersion name packageVersion ->
      do
        elmHome <- Stuff.getElmHome
        let filepath = 
              elmHome Path.</> "0.19.1" Path.</> "packages" 
                Path.</> (Pkg.toFilePath name)
                Path.</> (V.toChars packageVersion) Path.</> "docs.json"
        fileContents <- Data.ByteString.Builder.byteString <$> BS.readFile filepath
        System.IO.hPutStrLn System.IO.stdout (show fileContents)





 {-  Start Watchtower Server -}

data Flags =
  Flags
    { _port :: Maybe Int
    }



start :: Terminal.Command
start =
  let
    summary =
      "The Elm development experience dreams are made of."

    details =
      "The `start` command starts the Elm Dev server on your computer:"

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:51213>\
        \ and ready to be connected to."

    serverFlags =
      flags Flags
        |-- flag "port" port_ "The port of the Elm Dev server (default: 51213)"
  in
  Terminal.Command "start" (Common summary) details example (optional dir) serverFlags startServer


startServer :: Maybe FilePath -> Watchtower.Flags -> IO ()
startServer maybeRoot (Watchtower.Flags maybePort) =
  Watchtower.Server.serve maybeRoot (Watchtower.Server.Flags maybePort)





port_ :: Parser Int
port_ =
  Parser
    { _singular = "port"
    , _plural = "ports"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["51213"]
    }


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string




dir :: Parser FilePath
dir =
  Parser
    { _singular = "elm project directory"
    , _plural = "elm project directories"
    , _parser = parseDir
    , _suggest = \_ -> return []
    , _examples = exampleProjectDir
    }


parseDir :: String -> Maybe FilePath
parseDir chars =
  Just chars



exampleProjectDir :: String -> IO [String]
exampleProjectDir _ =
  return ["/path/to/my/project" ]

