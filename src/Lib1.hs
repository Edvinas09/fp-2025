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
    "show",
    "cars",
    "repairs",
    "tasks",
    "remove",
    "dump",
    "examples",
    "parent",
    "part",
    "qty",
    "receive",
    "set",
    "time",
    "price",
    "mechanic",
    "client"
  ]
data Command
  = AddCar
      { carId :: Integer,
        model :: String,
        plate :: String,
        clientRef :: Maybe Integer
      }
  | AddRepair
      { repairId :: Integer,
        title :: String,
        carRef :: Integer
      }
  | AddTask
      { taskId :: Integer,
        title :: String,
        parentTaskId :: Maybe Integer,
        repairRef :: Integer,
        mechanicRef :: Maybe Integer,
        status :: Maybe TaskStatus,
        estTime :: Maybe Double
      }
  | RequestPart
      { partId :: Integer,
        partName :: String,
        qty :: Integer,
        taskRef :: Integer
      }
  | ReceivePart
      { partId :: Integer
      }
  | SetTime
      { taskRef :: Integer,
        hours :: Double
      }
  | SetPrice
      { repairRef :: Integer,
        price :: Double
      }
  | AddMechanic
      { mechanicId :: Integer,
        mechanicName :: String,
        taskRef :: Integer
      }
  | AddClient
      { clientId :: Integer,
        clientName :: String
      }
  | CompleteTask
      { taskRef :: Integer
      }
  | ShowCars
  | ShowRepairs
  | ShowTasks
  | ShowParts
  | ShowClients
  | RemoveCar
      { carId :: Integer
      }
  | Dump Dumpable
  deriving (Show, Read)

data TaskStatus = WaitingForPart | InProgress | Completed | Cancelled
  deriving (Show, Read)

examples :: [Command]
examples =
  [ AddClient 1 "Jonas Jonaitis",
    AddCar 1 "BMW X5" "ABC123" (Just 1),
    AddMechanic 1 "Jonas" 4,
    AddRepair 1 "Engine diagnostics" 1,
    AddTask 1 "Diagnose engine" Nothing 1 (Just 1) (Just InProgress) (Just 2.0),
    AddTask 2 "Check spark plugs" (Just 1) 1 (Just 1) (Just WaitingForPart) Nothing,
    RequestPart 1 "Spark plug" 1 4, -- Užsakoma nauja dalis
    ReceivePart 1, -- Gaunama užsakytą dalis
    AddTask 3 "Remove spark plug 1" (Just 2) 1 (Just 1) (Just InProgress) (Just 0.5),
    CompleteTask 3,
    AddTask 4 "Install new spark plug 1" (Just 2) 1 (Just 1) (Just InProgress) Nothing,
    SetTime 4 0.5, -- Nustatomas laikas užduočiai 4
    SetPrice 1 150.0, -- Nustatoma kaina remontui 1
    CompleteTask 4,
    RemoveCar 1,
    Dump Examples
  ]
