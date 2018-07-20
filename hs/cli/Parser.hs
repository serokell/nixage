module Parser where

-- |  Nixage cli command parsers
--
import Universum
import Options.Applicative ( Parser, info, fullDesc, progDesc, header
                           , hsubparser, command
                           , forwardOptions, strArgument, ParserInfo, Mod
                           , CommandFields, optional, long, metavar, value
                           , option, auto, helper)

import Types

nixageP :: Parser NixageCmd
nixageP = hsubparser $ mconcat nixageCmds

nixageCmds :: [Mod CommandFields NixageCmd]
nixageCmds =
    [ command "stack" $ info (StackCmd <$> stackP) $
           progDesc "Run stack on stack.yaml generated from project.yaml"
        <> forwardOptions
    , command "convert" $ info (ConvertCmd <$> convertP) $
           progDesc "Convert between input formats (Yaml, Stack) "
    ]

-- | Parse all arguments after 'stack' command as raw [Text]
stackP :: Parser StackArgs
stackP = many $ strArgument mempty

convertP :: Parser ConvertArgs
convertP = ConvertArgs
       <$> option auto (long "from" <> metavar "in-format" <> value YamlInFormat)
       <*> option auto (long "to" <> metavar "out-format")
       <*> inOutPathP
  where
      inOutPathP :: Parser (Maybe (Text, Text))
      inOutPathP = (,)
               <$> optional (strArgument $ metavar "in-path")
               <*> optional (strArgument $ metavar "out-path") <&> \case
               (Just inPath, Just outPath) -> Just (inPath, outPath)
               _ ->  Nothing

