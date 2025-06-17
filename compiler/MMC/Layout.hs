module Layout where

import Data.Text (Text)
import MMC.Token (LToken)

type IndentLevel = Int

runLayout :: Text -> [LToken] -> [LToken]
runLayout src tokens = undefined

offsideRule :: [LToken] -> [LToken]
offsideRule tokens = undefined