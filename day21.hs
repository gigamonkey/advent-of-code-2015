import Data.List

data Player = Player { damage :: Int, armor :: Int, hitPoints :: Int } deriving (Show, Eq)

data Equipment = Equipment String Int Int Int deriving (Show)

takeInclusive p = (\(a, b) -> a ++ take 1 b) . span p

weapons = [ Equipment "Dagger" 8 4 0
          , Equipment "Shortsword" 10 5 0
          , Equipment "Warhammer" 25 6 0
          , Equipment "Longsword" 40 7 0
          , Equipment "Greataxe" 74 8 0
          ]

armors  = [ Equipment "Leather" 13 0 1
          , Equipment "Chainmail" 31 0 2
          , Equipment "Splintmail" 53 0 3
          , Equipment "Bandedmail" 75 0 4
          , Equipment "Platemail" 102 0 5
          ]

rings   = [ Equipment "Damage +1" 25 1 0
          , Equipment "Damage +2" 50 2 0
          , Equipment "Damage +3" 100 3 0
          , Equipment "Defense +1" 20 0 1
          , Equipment "Defense +2" 40 0 2
          , Equipment "Defense +3" 80 0 3
          ]

boss = Player 8 2 109

playerWins p1 p2 = b <= 0 where
    (_, b) = last $ takeInclusive (\(a, b) -> a > 0 && b > 0) $ fight (hitPoints p1) (hitPoints p2) (attacks p1 p2)
    attacks p1 p2 = cycle [max 1 (damage p1 - armor p2), max 1 (damage p2 - armor p1)]

fight h1 h2 (a1:a2:as) = (h1, h2') : (h1', h2') : fight h1' h2' as where
    h2' = h2 - a1
    h1' = h1 - a2

choices = foldl' cross [[]] [ choose 1 1 weapons, choose 0 1 armors, choose 0 2 rings ] where
    choose n m = filter ((\x -> n <= x && x <= m) . length) . subsequences
    cross a b = (++) <$> a <*> b

best = find (\(_, d, a) -> playerWins (Player d a 100) boss) $ sort $ map choice choices where
    choice = foldl' step (0, 0, 0)
    step (c, d, a) (Equipment _ cost damage armor) = (c + cost, d + damage, a + armor)

worst = find (\(_, d, a) -> not $ playerWins (Player d a 100) boss) $ sortBy (flip compare) $ map choice choices where
    choice = foldl' step (0, 0, 0)
    step (c, d, a) (Equipment _ cost damage armor) = (c + cost, d + damage, a + armor)

main = do
  print best
  print worst
