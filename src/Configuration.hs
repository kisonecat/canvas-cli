module Configuration where

import qualified Canvas
import Data.Text

-- Define a configuration type
data Configuration = Configuration
  { canvasConfig :: Canvas.Configuration
  , courseId :: Text
  }

