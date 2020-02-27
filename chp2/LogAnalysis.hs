
module LogAnalysis where
    
import Log

-- parseMessage :: String -> LogMessage

parseMessage msg = case msg of 
                  msg -> matchLetter(msgtype)
                  msg -> matchErrorCode(rest)
                  where msgtype = head(msg)
                        rest = tail(msg)

-- matchMsg :: String -> MessageType
matchLetter letter = case letter of
                'I' -> "it's info"
                'E' -> "it's an error"
                'W' -> "it's a warning"

matchErrorCode code = case xs of
                (' ':xs) -> matchErrorCode tail(code)
                (_:xs)-> "it's not an error"