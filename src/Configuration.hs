module Configuration where

import qualified Canvas
import Data.Text

-- Define a configuration type
data Configuration = Configuration
  { token :: Text,
    apiBase :: Text,
    canvasConfig :: Canvas.Configuration
  , courseId :: Text
  }

