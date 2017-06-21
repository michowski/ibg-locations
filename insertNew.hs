import Data.List (intercalate)

type Prefix = String
type Depth = Int
type Level = Int
type Row = Int
type Location = (Prefix, Depth, Level, Row)

startingRow = 200

statement :: [[String]] -> String
statement xss = 
  "insert into locations (tower_id, label, row, lane, depth, level, products_allowed, sort_weight, is_active, is_equipment, created_on, created_by, modified_on, modified_by, creator_id, updater_id) values\n" ++ (intercalate ",\n" (groups xss)) ++ ";"
    where
      wrapVal x = if x == "null" then "null" else "'" ++ x ++ "'"
      groups = map $ \xs ->
        "(" ++ (intercalate ", " $ map wrapVal xs) ++ ")"

toNumber :: Int -> String
toNumber x
  | x < 10 = '0' : show x
  | otherwise = show x

prefixes :: [Prefix]
prefixes = ("C" ++) . return <$> ['A'..'K']

depths :: [Depth]
depths = [1..15]

levels :: [Level]
levels = [0..3]

locations :: [Location]
locations = zip [startingRow..] prefixes >>= \(r, p) ->
  depths >>= \d ->
    (<$> levels) $ \l ->
      (p, d, l, r)

values :: [[String]]
values = (<$> locations) $ \(p, d, l, r) ->
  let
    tower = p ++ "-" ++ toNumber d
  in
    [ tower
    , tower ++ "-" ++ toNumber (l * 10)
    , show r
    , show $ (r + 1) `div` 2 * 2
    , show d
    , show l
    , "10"
    , "1"
    , "1"
    , "0"
    , "null"
    , "777"
    , "null"
    , "777"
    , "777"
    , "777"
    ]

main = putStr $ statement values
