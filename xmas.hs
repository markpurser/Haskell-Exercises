import System.Environment
import Data.List


-- compute spaces running alongside the left of the tree
nspaces :: Int -> [Int]
nspaces a = reverse $ take a [0..]

-- compute stars making up the tree
nstars :: Int -> [Int]
nstars a = take a [x | x <- [1..], odd x]

-- convert tuple containing number of spaces and number of stars to a string
stringifytreerow :: (Int, Int) -> [Char]
stringifytreerow (nspaces, nstars) = (take nspaces $ repeat ' ') ++ (take nstars $ repeat '*')


main = do
	-- get single integer argument representing the height of the Christmas tree
	(heightstring:args) <- getArgs
	let height = read heightstring :: Int

	-- use the height to generate a model of the tree consisting of tuples of number of
	-- spaces and number of stars for each row
	let treemodel = zip (nspaces height) (nstars height)
	let treestrings = map stringifytreerow treemodel

	-- align the trunk of the tree with the top (head) of the tree
	let trunkstring = (take (head $ nspaces height) $ repeat ' ') ++ "|"

	-- display the tree
	mapM_ putStrLn $ treestrings
	putStrLn trunkstring

