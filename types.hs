{-# LANGUAGE RecordWildCards #-}

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
