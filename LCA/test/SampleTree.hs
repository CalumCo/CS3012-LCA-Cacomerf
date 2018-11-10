module SampleTree where

import Graph

sampleTree :: Graph Int
sampleTree =
    Node 0
        (Node 3
            Empty
            (Node 5
                (Node 100
                    Empty
                    (Node 1
                        Empty
                        Empty
                    )
                )
                (Node 7
                    Empty
                    (Node 42
                        (Node 451
                            Empty
                            Empty
                        )
                        Empty
                    )
                )
            )
        )
        (Node 8
            Empty
            (Node 43
                (Node 500
                    Empty
                    Empty
                )
                Empty
            )
        )

sampleTreeDoubled :: Graph Int
sampleTreeDoubled =
    Node 0
        (Node 6
            Empty
            (Node 10
                (Node 200
                    Empty
                    (Node 2
                        Empty
                        Empty
                    )
                )
                (Node 14
                    Empty
                    (Node 84
                        (Node 902
                            Empty
                            Empty
                        )
                        Empty
                    )
                )
            )
        )
        (Node 16
            Empty
            (Node 86
                (Node 1000
                    Empty
                    Empty
                )
                Empty
            )
        )
