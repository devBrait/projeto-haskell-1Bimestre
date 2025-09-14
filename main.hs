{-
    Exercício Programa 1 - Huffman em Haskell
    Paradigmas de Linguagens de Programação

    Guilherme Teodoro de Oliveira RA: 10425362
    Luís Henrique Ribeiro Fernandes RA: 10420079
    Vinícius Brait Lorimier - 10420046
-}

-- Definição da árvore de Huffman
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree
    deriving (Show)

-- Conta quantas vezes um caracter aparece em uma string
countCaracters :: [Char] -> Char -> Int
countCaracters [] _ = 0
countCaracters (a:x) char = if char == a then 1 + countCaracters x char else countCaracters x char

-- Frequência de cada caracter
frequencyAlreadyVisited :: [Char] -> [Char] -> [(Char, Int)]
frequencyAlreadyVisited [] _ = []
frequencyAlreadyVisited (x:xs) alreadyVisited
  | x `elem` alreadyVisited = frequencyAlreadyVisited xs alreadyVisited
  | otherwise = (x, countCaracters (x:xs) x) : frequencyAlreadyVisited xs (x:alreadyVisited)

frequencies :: [Char] -> [(Char, Int)]
frequencies word = frequencyAlreadyVisited word []

-- Ordenação a partir das frequências
sortByFreq :: [(Char, Int)] -> [(Char, Int)]
sortByFreq [] = []
sortByFreq (a:list) =
    sortByFreq [x | x <- list, snd x < snd a] ++ [a] ++ sortByFreq [x | x <- list, snd x >= snd a]

-- Construção inicial das folhas da árvore
createLeaves :: [(Char, Int)] -> [HuffmanTree]
createLeaves [] = []
createLeaves ((char,freq):xs) = Leaf char freq : createLeaves xs

-- Pega a frequência de um nó
weight :: HuffmanTree -> Int
weight (Leaf _ freq) = freq
weight (Node freq _ _) = freq

-- Ordenação a partir dos pesos das árvores
sortTrees :: [HuffmanTree] -> [HuffmanTree]
sortTrees [] = []
sortTrees (a:xs) =
    sortTrees [x | x <- xs, weight x < weight a] ++ [a] ++ sortTrees [x | x <- xs, weight x >= weight a]

-- Construção da árvore
buildTree :: [HuffmanTree] -> HuffmanTree  
buildTree [tree] = tree
buildTree (tree1:tree2:rest) =
    buildTree (sortTrees (Node (weight tree1 + weight tree2) tree1 tree2 : rest))

-- Construção da tabela de códigos usando a árvore como base
generateTable :: HuffmanTree -> [(Char, String)]
generateTable (Leaf char _) = [(char, "")]
generateTable (Node _ left right) =
    [(char, '0':code) | (char,code) <- generateTable left] ++
    [(char, '1':code) | (char,code) <- generateTable right]

-- Busca código de um caractere
searchCode :: Char -> [(Char, String)] -> String
searchCode _ [] = ""
searchCode c ((ch, code):rest)
  | c == ch = code
  | otherwise = searchCode c rest

-- Codificação da string de entrada
encode :: String -> [(Char, String)] -> String
encode [] _ = ""
encode (x:xs) table = searchCode x table ++ encode xs table

-- Função principal de Huffman usando where
huffman :: String -> String
huffman intput = encode intput table
  where
    freqList = sortByFreq (frequencies intput)
    leaves   = createLeaves freqList
    tree     = buildTree (sortTrees leaves)
    table    = generateTable tree

-- Verificação do arquivo in.txt e gera o arquivo out.txt
main :: IO ()
main = do
    content <- readFile "in.txt"
    let encoded = huffman content
    writeFile "out.txt" encoded
    putStrLn "Execução concluída. Arquivo out.txt disponível!"