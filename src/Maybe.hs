module Maybe where

import Text.Read ( readMaybe )
maybe 0 (*2) (readMaybe "5")
maybe 0 (*2) (readMaybe "")

