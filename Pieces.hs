module Pieces(shapes, i, j, l, o, s, t, z) where

import TetrisTypes
import Graphics.Gloss

iC = cyan;
jC = blue;
lC = orange;
oC = yellow;
sC = green;
tC = magenta;
zC = red;

i = [
	[Void, Void, Block iC, Void],
	[Void, Void, Block iC, Void],
	[Void, Void, Block iC, Void],
	[Void, Void, Block iC, Void]
	]

j = [
	[Void    , Block jC, Void],
	[Void    , Block jC, Void],
	[Block jC, Block jC, Void]
	]

l = [
	[Void, Block lC, Void],
	[Void, Block lC, Void],
	[Void, Block lC, Block lC]
	]

o = [
	[Block oC, Block oC],
	[Block oC, Block oC]
	]

s = [
	[Void    , Void    , Void],
	[Void    , Block sC, Block sC],
	[Block sC, Block sC, Void]
	]

t = [
	[Void    , Block tC, Void],
	[Block tC, Block tC, Block tC],
	[Void    , Void    , Void]
	]

z = [
	[Void    , Void    , Void],
	[Block zC, Block zC, Void],
	[Void    , Block zC, Block zC]
	]

shapes = [i, j, l, o, s, t, z]