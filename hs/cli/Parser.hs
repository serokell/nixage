module Parser where

-- |  Nixage cli command parsers
--
import Universum
import Options.Applicative ( Parser, info, fullDesc, progDesc, header
                           , hsubparser, subparser, command
                           , forwardOptions, strArgument, ParserInfo, Mod
                           , CommandFields, optional, long, metavar, value
                           , option, auto, helper, strOption, commandGroup)

import Types

nixageP :: Parser NixageCmd
nixageP = hsubparser $ mconcat
    [ command "stack" $ info (StackCmd <$> stackArgsP) $
           progDesc "Run stack on stack.yaml generated from project.yaml"
        <> forwardOptions
    , command "convert" $ info (ConvertCmd <$> convertArgsP) $
           progDesc "Convert between input formats (Yaml, Stack) "
    ]

-- | * Stack command parser
--
-- | Parse all arguments after 'stack' command as raw [Text]
stackArgsP :: Parser StackArgs
stackArgsP = many $ strArgument mempty



-- | * Convert command parsers

convertArgsP :: Parser ConvertArgs
convertArgsP = hsubparser convertArgsInMod

-- | ConvertArgs parser for first command (In or Out with default In)
-- For each (inP, outP) from (Is + [defI]) x Os:
--    apply (ConvertArgs <$> inP <*> outP) parser after nested commands
convertArgsInMod :: Mod CommandFields ConvertArgs
convertArgsInMod =
       (flip foldMap convertInPs $ \(inCmd, inP, defIn) ->
           command' inCmd $ hsubparser $ convertArgsOutMod inP defIn)
    <> convertArgsOutMod defConvertInP defConvertIn
    <> metavar "COMMAND"
    <> commandGroup "Input/output format command"

-- | ConvertArgs parser for second command (Out)
convertArgsOutMod :: Parser ConvertIn -> ConvertIn -> Mod CommandFields ConvertArgs
convertArgsOutMod inP defIn=
       (flip foldMap convertOutPs $ \(outCmd, outP, defOut) ->
            command' outCmd $ fmap (fromMaybe $ ConvertArgs defIn defOut) $
                optional $ ConvertArgs <$> inP <*> outP)
    <> metavar "OUT_COMMAND"
    <> commandGroup "Output format command"

-- | ConvertIn parsers for each ConvertIn constructor
-- Each command should specify its default value
convertInPs  :: [(String, Parser ConvertIn, ConvertIn)]
convertInPs =
    [ ( "from-yaml"
      , YamlConvertIn <$> (strArgument $ metavar "project-yaml")
      , defConvertIn
      )
    , ( "from-x"
      , XConvertIn
        <$> (strArgument $ metavar "x-in0" <> value "val-x-in0")
        <*> (strOption $ long "x-in1" <> value "val-x-in1")
      , XConvertIn "def-x-0" "def-x-1"
      )
    ]

-- Default ConvertIn
defConvertInP :: Parser ConvertIn
defConvertInP = pure defConvertIn

defConvertIn :: ConvertIn
defConvertIn = YamlConvertIn "project.yaml"


-- | ConvertOut parsers for each ConvertOut constructor
convertOutPs  :: [(String, Parser ConvertOut, ConvertOut)]
convertOutPs =
    [ ( "to-stack"
      , StackConvertOut
        <$> (strArgument $ metavar "stack-yaml")
        <*> (strArgument $ metavar "snapshot-yaml")
      , StackConvertOut "stack.yaml" "snapshot.yaml"
      )
    , ( "to-y"
      , YConvertOut <$> (strArgument $ metavar "y-out")
      , YConvertOut "def-y-out"
      )
    ]

-- | Helper function
command' :: String -> Parser a -> Mod CommandFields a
command' cmd subP = command cmd $ info subP $ mempty
