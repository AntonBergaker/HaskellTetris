module Pieces(shapes, i, j, l, o, s, t, z) where

import TetrisTypes
import Graphics.Gloss

{- iC
    Colors the I block cyan
-}
iC :: Color
iC = makeColor (106/255) (236/255) (244/255) 1;


{- jC
    Colors the J block blue
-}
jC :: Color
jC = makeColor (69 /255) (115/255) (231/255) 1;


{- lC
    Colors the L block orange
-}
lC :: Color
lC = makeColor (255/255) (186/255) (109/255) 1;


{- oC
    Colors the O block purple
-}
oC :: Color
oC = makeColor (173/255) (97 /255) (237/255) 1;

{- sC
    Colors the S block green
-}
sC :: Color
sC = makeColor (178/255) (240/255) (104/255) 1;


{- tC
    Colors the T block pink
-}
tC :: Color
tC = makeColor (253/255) (166/255) (248/255) 1;


{- zC
    Colors the Z block red
-}
zC :: Color
zC = makeColor (236/255) (111/255) (134/255) 1;

{- i 
    The I block
-}
i :: Grid
i = [
    [Void, Void, Block iC, Void],
    [Void, Void, Block iC, Void],
    [Void, Void, Block iC, Void],
    [Void, Void, Block iC, Void]
    ]


{- j 
    The J block
-}
j :: Grid
j = [
    [Void    , Block jC, Void],
    [Void    , Block jC, Void],
    [Block jC, Block jC, Void]
    ]


{- l
    The L block
-}
l :: Grid
l = [
    [Void, Block lC, Void],
    [Void, Block lC, Void],
    [Void, Block lC, Block lC]
    ]

{- o
    The O block
-}
o :: Grid
o = [
    [Block oC, Block oC],
    [Block oC, Block oC]
    ]

{- s
    The S block
-}
s :: Grid
s = [
    [Void    , Void    , Void],
    [Void    , Block sC, Block sC],
    [Block sC, Block sC, Void]
    ]

{- t 
    The T block
-}
t :: Grid
t = [
    [Void    , Block tC, Void],
    [Block tC, Block tC, Block tC],
    [Void    , Void    , Void]
    ]

{- z
    The Z block
-}
z :: Grid
z = [
    [Void    , Void    , Void],
    [Block zC, Block zC, Void],
    [Void    , Block zC, Block zC]
    ]
{- shapes
    A list of all the blocks
-}
shapes :: [Grid]
shapes = [i, j, l, o, s, t, z]