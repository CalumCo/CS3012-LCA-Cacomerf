module BigGraph where

import Data.Maybe
import Graph
import BigTree



bigGraph =
    let
        n62p = head $ paths bigTree 62
        n59p = head $ paths bigTree 59
        n58p = head $ paths bigTree 58
        n57p = head $ paths bigTree 57
        n52p = head $ paths bigTree 52
        n25p = head $ paths bigTree 25

        n62 = fromJust $ followNode bigTree n62p
        n58 = fromJust $ followNode bigTree n58p
        n57 = fromJust $ followNode bigTree n57p
        n52 = fromJust $ followNode bigTree n52p
        n25 = fromJust $ followNode bigTree n25p

        g1 = replaceNode (const n58) bigTree n59p
        g2 = replaceNode (const n52) g1 (n25p ++ [GraphLeft])
        g3 = replaceNode (const n58) g2 n59p

        supplement = Node 100 n62 n57
    in FullGraph [supplement, g3]
