#!/usr/bin/env stack
-- stack --resolver lts-22.33 script --package diagrams-lib --package diagrams-pgf --package texrunner --package diagrams-rasterific

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude

corners = map (fontSizeL 0.5 . text) ["Learning", "Creating", "Community"]

joyDiagram :: Diagram B
joyDiagram =
  mconcat
    [ mconcat $ zipWith moveTo (triangle 8) corners
    , text "JOY" # fontSizeL 0.8
    ]
    # frame 2

main :: IO ()
main = mainWith joyDiagram
