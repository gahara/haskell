-- the simpliest one
-- {-# LANGUAGE OverloadedStrings #-}
-- import Network.Wai
-- import Network.HTTP.Types
-- import Network.Wai.Handler.Warp (run)

-- app :: Application
-- app _ respond = do
--     putStrLn "Somebody touched me"
--     respond $ responseLBS
--         status200
--         [("Content-Type", "text/plain")]
--         "Test! Test! Test!"

-- main :: IO ()
-- main = do
--     putStrLn $ "http://localhost:8080/"
--     run 8080 app


--more complicate one
{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
 
main = do
    let port = 8000
    putStrLn $ "Listening on port " ++ show port
    run port app
 
app req respond = respond $
    case pathInfo req of
        ["route"] -> route
        x -> index x
 
route = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "It was a joke" ]
 
index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello! Press the link below</p>"
    , "<p><a href='/route'>Surprise</a></p>\n" ]