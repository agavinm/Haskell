# Example of use:

ghci BinaryTree.hs  <br /><br />
   testTree = tree "R" (tree "HI" (leaf "NII") (leaf "NID")) (tree "HD" (leaf "NDI") (leaf "NDD"))  
   print $ size testTree  
   print $ preorder testTree  
   print $ postorder testTree  
   print $ inorder testTree  
   print $ inorder $ foldl add empty [3, 2, 5, 1, 4]  
   print $ between (foldl add empty [3, 2, 2, 5, 1, 4, 4]) 2 4  
   print $ inorder $ foldl add empty ["Juan", "Pedro", "Adolfo", "Diego"]  
