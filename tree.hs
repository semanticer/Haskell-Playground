import Data.List

-- CONSOLE OUTPUT FUNCTIONS --

-- print given tree
main = putStrLn (treeToSring (tree 4 2 3))

-- concatenate string from tree list structure 
treeToSring treeList = intercalate "" [ unlines block | block <- treeList] 


-- CORE FUNCTIONS -- 

-- creates tree in list structure
tree bc gf fbs = [block rows (treeMaxSp bc gf fbs) | rows <- take bc [fbs,(fbs+gf)..]]

-- counts maximal number of spaces for given tree
treeMaxSp bc gf fbs = ((bc-1)*gf)+(fbs-1)

-- creates one block with specified number of rows and maximal space 
block rowCount maxSp = [row st sp | (st,sp) <- take rowCount (zip [1,3..] [maxSp,maxSp-1..0])]

-- creates one row with specified number of stars and spaces
row st sp = take sp (repeat ' ') ++ take st (repeat '*')




