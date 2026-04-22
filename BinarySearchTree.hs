import Data.List ( intercalate )

data BSTDict a b = NIL | NODE (BSTDict a b) (a,b) (BSTDict a b) deriving Eq
inorder :: BSTDict a b -> [(a,b)]

main :: IO ()
main = do
    bstActions NIL

inorder                 = go [] where 
    go l NIL            = l 
    go l (NODE tl x tr) = go (x:go l tr) tl

deleteMax :: BSTDict a b -> ((a,b),BSTDict a b)
deleteMax (NODE tl (k,v) NIL)=((k,v),tl)
deleteMax (NODE tl (k,v) tr) = let (p,tz) = deleteMax tr in (p, NODE tl (k,v) tz)

bstActions :: BSTDict Int String -> IO ()
bstActions tree = do
    cmd <- getLine
    let l = words cmd
    case l!!0 of
        "insert" -> do
            let t=tree
            let (k,v)= read (l!!1) :: (Int, String)
            let insTree = insertBST (k,v) t
                insertBST (k,v) NIL = NODE NIL (k,v) NIL
                insertBST (k,v) (NODE tl (k',v') tr)= case compare k k' of
                    LT -> NODE (insertBST (k,v) tl) (k',v') tr
                    EQ -> NODE tl (k',v') tr
                    GT -> NODE tl (k',v')  (insertBST (k,v) tr)
            putStrLn $ intercalate " " (map snd(inorder insTree))
            bstActions insTree
        "delete" -> do
            let t=tree
            let (k,v) = read (l!!1) :: (Int,String)
            let delTree= delete (k,v) t where
                delete (k,v) NIL = NIL
                delete (k,v) (NODE tl (k',v') tr) = case compare k k' of 
                    LT -> NODE (delete (k,v) tl) (k',v') tr
                    EQ -> if tl==NIL then tr else let (p,tz)=deleteMax tl in NODE tz p tr
            putStrLn $ intercalate " " (map snd(inorder delTree))
            bstActions delTree
        "bye" -> do
            putStrLn "Exiting! Good bye!"
        _ -> do
            putStrLn "Invalid operation!"
        