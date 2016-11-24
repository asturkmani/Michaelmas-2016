Module to define the type of a maze

> module MyMazeTrees (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography
> import Data.List
> import Data.Maybe

We'll represent a maze by its size and a list of its walls.

> data Maze = AMaze Size (STree Place) (STree Place) (STree Place) (STree Place)

The list of walls will be complete in the sense that we record both sides of
the wall; for example, if the list includes ((3,4), N), then it will also
include ((3,5),S).

This function creates a maze given its size and a list of walls; the list of
walls might not be complete in the above sense.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = 
>   let boundaries = -- the four boundaries
>         [((0,j),   W) | j <- [0..y-1]] ++ -- westerly boundary
>         [((x-1,j), E) | j <- [0..y-1]] ++ -- easterly boundary
>         [((i,0),   S) | i <- [0..x-1]] ++ -- southerly boundary
>         [((i,y-1), N) | i <- [0..x-1]]    -- northerly boundary
>       allWalls = walls ++ boundaries ++ map reflect (walls ++ boundaries)
>       northWalls = buildBalanced( sort (map fst ([x | x <- allWalls, snd(x)==N])))
>       southWalls = buildBalanced( sort (map fst ([x | x <- allWalls, snd(x)==S])))
>       eastWalls = buildBalanced( sort( map fst ([x | x <- allWalls, snd(x)==E])))
>       westWalls = buildBalanced( sort( map fst ([x | x <- allWalls, snd(x)==W])))
>  in AMaze (x,y) northWalls southWalls eastWalls westWalls

The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ northWalls southWalls eastWalls westWalls) pos d
>   | d == N = isIn pos northWalls
>   | d == S = isIn pos southWalls
>   | d == E = isIn pos eastWalls
>   | d == W = isIn pos westWalls
>   | otherwise = False

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size

> buildBalanced :: [Place] -> STree Place
> buildBalanced []   = Empty
> buildBalanced elts = Node (buildBalanced $ take half elts) 
>                          (elts !! half) 
>                          (buildBalanced $ drop (half+1) elts)
>    where half = length elts `quot` 2

> data STree a = Empty | Node (STree a) a (STree a)

> isIn :: Ord a => a -> STree a -> Bool
> isIn x Empty = False
> isIn x (Node t1 y t2)
> 	| x==y = True
> 	| x<y = isIn x t1
> 	| x>y = isIn x t2


============================= OUTPUT =================================


*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.04 secs, 6,156,920 bytes)

Again, about twice as fast!

