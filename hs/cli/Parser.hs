module Parser where

-- |  Nixage cli command parsers
--
import Universum
import Options.Applicative ( Parser, info, fullDesc, progDesc, header
                           , hsubparser, command
                           , forwardOptions, strArgument, ParserInfo, Mod
                           , CommandFields, optional, long, metavar, value
                           , option, auto, helper, strOption)

import Types

nixageP :: Parser NixageCmd
nixageP = hsubparser $ mconcat
    [ command "stack" $ info (StackCmd <$> stackArgsP) $
           progDesc "Run stack on stack.yaml generated from project.yaml"
        <> forwardOptions
    , command "convert" $ info (ConvertCmd <$> convertArgsP) $
           progDesc "Convert between input formats (Yaml, Stack) "
    ]

-- | Parse all arguments after 'stack' command as raw [Text]
stackArgsP :: Parser StackArgs
stackArgsP = many $ strArgument mempty

-- | Convert command parsers
convertArgsP, yamlConvertInP, xConvertInP :: Parser ConvertArgs
convertArgsP = hsubparser $ mconcat
    [ command' "from-yaml" yamlConvertInP
    , command' "from-x"    xConvertInP
    , command' "to-stack"  $ ImplicitConvertIn <$> stackConvertOutP
    , command' "to-y"      $ ImplicitConvertIn <$> yConvertOutP
    ]

yamlConvertInP = YamlConvertIn
             <$> (strOption $ long "from" <> value "project.yaml" )
             <*> convertOutP

xConvertInP =  XConvertIn
          <$> (strOption $ long "x-in")
          <*> convertOutP


convertOutP, stackConvertOutP, yConvertOutP :: Parser ConvertOut
convertOutP = hsubparser $ mconcat
    [ command' "to-stack" stackConvertOutP
    , command' "to-y"     yConvertOutP
    ]

stackConvertOutP = StackConvertOut
              <$> (strArgument $ metavar "stack-yaml" <> value "stack.yaml")
              <*> (strArgument $ metavar "snapshot-yaml" <> value "snapshot.yaml")

yConvertOutP = YConvertOut
           <$> (strArgument $ metavar "y-out")

command' :: String -> Parser a -> Mod CommandFields a
command' cmd subP = command cmd $ info subP mempty
