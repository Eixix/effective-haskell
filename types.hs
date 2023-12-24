-- data CustomerInfo = CustomerInfo {firstName :: String, lastName :: String, widgetCount :: Int, money :: Int}

-- showCustomer CustomerInfo {..} = firstName <> lastName <> show widgetCount <> show money

-- customerFactory firstName lastName =
-- let widgetCount = 10
-- money = 100
-- in CustomerInfo {..}

-- data Direction = North | South | East | West

-- data Person
-- = Customer {name :: String, balance :: Int}
-- \| Employee {name :: String, managerName :: String, salary :: Int}

-- george = Customer {name = "Georgie", balance = 100}

-- porter = Employee {name = "Porter", managerName = "Remi", salary = 10}

-- test = balance porter

-- Better approach:
data CustomerInfo = CustomerInfo {customerName :: String, customerBalance :: Int}

data EmployeeInfo = EmployeeInfo {employeeName :: String, employeeManagerName :: String, employeeSalary :: Int}

data Person = Customer CustomerInfo | Employee EmployeeInfo

george :: Person
george = Customer $ CustomerInfo {customerName = "George", customerBalance = 100}

porter :: Person
porter = Employee $ EmployeeInfo {employeeName = "Porter", employeeManagerName = "Remi", employeeSalary = 10}

getPersonName :: Person -> String
getPersonName person =
  case person of
    Employee employee -> employeeName employee
    Customer customer -> customerName customer

data MaybeString = NoString | SomeString String

getPersonManager :: Person -> MaybeString
getPersonManager person =
  case person of
    Employee employee -> SomeString (employeeManagerName employee)
    _ -> NoString

data MaybeInt = NoInt | SomeInt Int

getPersonSalary :: Person -> MaybeInt
getPersonSalary person =
  case person of
    Employee employee -> SomeInt (employeeSalary employee)
    _ -> NoInt

-- data Either a b = Left a | Right b
eitherToMaybe :: Either a1 a2 -> Maybe a2
eitherToMaybe e =
  case e of
    Left _ -> Nothing
    Right val -> Just val

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool
eqPeano Z Z = True
eqPeano (S n) (S p) = eqPeano n p
eqPeano _ _ = False

addPeano :: Peano -> Peano -> Peano
addPeano Z n = n
addPeano (S n) p = addPeano n (S p)

data List a = Empty | Cons a (List a)

toList :: [a] -> List a
toList = foldr Cons Empty

fromList :: List a -> [a]
fromList = listFoldr (:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ b Empty = b
listFoldr f b (Cons x xs) = f x $ listFoldr f b xs

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ b Empty = b
listFoldl f b (Cons x xs) = listFoldl f (f b x) xs

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons x xs) = Just x

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons x xs) = xs

listReverse :: List a -> List a
listReverse Empty = Empty
listReverse (Cons x xs) = Cons (listHead xs) (Cons x Empty)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse xs <> [x]

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)