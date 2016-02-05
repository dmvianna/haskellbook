data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
                deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
               deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
  deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmHouse =
  BigFarmHouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmHouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
              deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                       deriving (Eq, Show)
                                
data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                           deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = os
                             , lang = lang }
                  | lang <- allLanguages,
                    os <- allOperatingSystems]

data ThereYet =
  There Integer Float String Bool
  deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "whoohoo"

yusss :: ThereYet
yusss = notQuite False

-- newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer =
  Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

eQuad = 8 -- Either, so no product, just sum
prodQuad = 16
funcQuad = 256
prodTBool = 8
gTwo = 16
fTwo = 65536

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
