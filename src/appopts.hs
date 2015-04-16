import Options.Applicative

data Sample = Sample
    { hello :: String
    , max   :: Int
    , quiet :: Bool }

maxCount :: Parser Int
maxCount = option auto
            ( long "max"
            <> short 'm'
            <> metavar "MAX"
            <> help "max number of glorps")

sample :: Parser Sample
sample = Sample
    <$> strOption
        ( long "hello"
        <> short 'H'
        <> metavar "TARGET"
        <> help "Target for the greeting")
    <*> maxCount
    <*> switch
        ( long "quiet"
        <> help "whether to be quiet")

greet :: Sample -> IO ()
greet (Sample h m False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

opts :: ParserInfo Sample
opts = info (helper <*> sample)
    ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
    options <- execParser opts
    greet options
