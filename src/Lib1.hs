{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lib1
  ( keywords,
    examples,
    Command (..),
    Dumpable (..),
  )
where

data Dumpable = Examples
  deriving (Show, Read)

keywords :: [String]
keywords =
  [ "add",
    "car",
    "repair",
    "task",
    "complete",
    "list",
    "cars",
    "repairs",
    "tasks",
    "remove",
    "dump",
    "examples",
    "parent"
  ]

data Command
  = AddCar
      { carId :: Integer,
        model :: String,
        plate :: String
      }
  | AddRepair
      { repairId :: Integer,
        title :: String,
        carRef :: Integer
      }
  | AddTask
      { taskId :: Integer,
        title :: String,
        parentId :: Maybe Integer
      }
  | CompleteTask
      { taskRef :: Integer
      }
  | ListCars
  | ListRepairs
  | ListTasks
  | RemoveCar
  | Dump Dumpable
  deriving (Show, Read)

examples :: [Command]
examples =
  [ AddCar 1 "BMW X5" "ABC123",
    AddRepair 1 "Engine diagnostics" 1,
    AddTask 1 "Check spark plugs" Nothing,
    AddTask 2 "Tighten plug 1" (Just 1), -- recursion
    CompleteTask 2,
    Dump Examples
  ]
