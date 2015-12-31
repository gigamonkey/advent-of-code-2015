import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe

type Effect = State -> State

data State = State { bossHitPoints   :: Int
                   , bossDamage      :: Int
                   , playerHitPoints :: Int
                   , playerMana      :: Int
                   , playerArmor     :: Int
                   , spells          :: [Spell]
                   , currentEffects  :: [[Effect]]
                   }

instance Show State where
    show s = "State " ++ (intercalate " " (map (show . ($ s)) [ bossHitPoints, playerHitPoints, playerMana, playerArmor])) ++ " " ++ (show $ spells s)

data Spell = MagicMissle | Drain | Shield | Poison | Recharge deriving (Eq, Show)

cost :: Spell -> Int
cost MagicMissle = 53  -- Magic Missile costs 53 mana.
cost Drain       = 73  -- Drain costs 73 mana.
cost Shield      = 113 -- Shield costs 113 mana.
cost Poison      = 173 -- Poison costs 173 mana.
cost Recharge    = 229 -- Recharge costs 229 mana.

effects :: Spell -> [Effect]
-- Magic Missile instantly does 4 damage.
effects MagicMissle = returning MagicMissle $ [ \s -> s { bossHitPoints = bossHitPoints s - 4 } ]

-- Drain costs instantly does 2 damage and heals you for 2 hit points.
effects Drain = returning Drain $ [ \s -> s { bossHitPoints = bossHitPoints s - 2 , playerHitPoints = playerHitPoints s + 2 } ]

-- Shield starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
effects Shield = returning Shield $ [ \s -> s { playerArmor = playerArmor s + 7 } ] ++ (take 5 $ repeat id) ++ [ (\s -> s { playerArmor = playerArmor s - 7 }) ]

-- Poison starts an effect that lasts for 6 turns. At the start of
-- each turn while it is active, it deals the boss 3 damage.
effects Poison = returning Poison $ id : (take 6 $ repeat (\s -> s { bossHitPoints = bossHitPoints s - 3 }))

-- Recharge starts an effect that lasts for 5 turns. At the start of
-- each turn while it is active, it gives you
effects Recharge = returning Recharge $ id : (take 5 $ repeat (\s -> s { playerMana = playerMana s + 101 }))

returning spell fns = init fns ++ [ last fns . (\s -> s { spells = spell : spells s }) ]

takeInclusive p = (\(a, b) -> a ++ take 1 b) . span p

heads :: [[a]] -> ([a], [[a]])
heads xxs = (hs, ts) where
    hs = map head xxs
    ts = filter (not . null) $ map tail xxs

start = State { bossHitPoints   = 51
              , bossDamage      = 9
              , playerHitPoints = 50
              , playerMana      = 500
              , playerArmor     = 0
              , spells          = [ MagicMissle, Drain, Shield, Poison, Recharge ]
              , currentEffects  = []
              }

search :: State -> Int -> Maybe Int -> Maybe Int
search state paid best
    | bossHitPoints state <= 0   = Just paid
    | playerHitPoints state <= 0 = Nothing
    | null affordable            = Nothing
    | otherwise                  = trySpells affordable best
    where
      affordable         = filter ((<= (playerMana state)) . cost) $ spells state
      minusOne state     = state { playerHitPoints = playerHitPoints state - 1 }
      trySpells [] b     = b
      trySpells (s:ss) b = maybeMin b (trySpells ss (if maybe False (paid >) b then b else next s b))
      next s b           = search (bossAttack (cast s state)) (paid + (cost s)) b

maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin Nothing Nothing = Nothing
maybeMin Nothing x = x
maybeMin x Nothing = x
maybeMin (Just x) (Just y) = Just (min x y)

cast :: Spell -> State -> State
cast spell state = applyEffects updated where
    updated = state { playerMana = playerMana state - (cost spell)
                    , spells = delete spell (spells state)
                    , currentEffects = effects spell : currentEffects state
                    }

bossAttack s = applyEffects (s { playerHitPoints = hp - max 1 (d - a) }) where
    hp = playerHitPoints s
    d = bossDamage s
    a = playerArmor s

applyEffects :: State -> State
applyEffects state = foldl (&) updated now where
    (now, later) = heads (currentEffects state)
    updated = state { currentEffects = later }

-- Scratch

search1 :: State -> Spell -> State
search1 state spell = bossAttack (cast spell state)

search' s | bossHitPoints s <= 0 = [s]
          | playerHitPoints s <= 0 = [s]
          | otherwise = s : [ s' | spell <- affordable s, s' <- search' (bossAttack (cast spell s)) ]
          where
            affordable s = filter ((< (playerMana s)) . cost) $ spells s

main = print $ search start 0 Nothing
