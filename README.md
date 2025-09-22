# fp-2025

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

# Domain: Garage Management DSL

## Domain description
This DSL models a comprehensive car garage management system.  

Main entities:
- **Clients** — Customer information (ID, name).
- **Cars** — Vehicle details (ID, model, license plate, linked to client).
- **Repairs** — Repair jobs linked to cars.
- **Tasks** — Work steps inside repairs. Tasks may have subtasks, creating recursion.
- **Parts** — Auto parts needed for tasks with quantity tracking.
- **Mechanics** — Workers assigned to specific tasks.

## DSL Commands Examples

### Client Management
```
AddClient 1 "Jonas Jonaitis"    -- Create a new client with ID 1 and name "Jonas Jonaitis"
ShowClients                     -- Display all registered clients
```

### Car Management
```
AddCar 1 "BMW X5" "ABC123" (Just 1)    -- Register car ID 1 (BMW X5, plate ABC123) for client 1
AddCar 2 "Audi A4" "XYZ789" Nothing    -- Register car ID 2 without linking to a client
ShowCars                                -- Display all registered cars
RemoveCar 1                             -- Remove car with ID 1 from the system
```

### Repair Management
```
AddRepair 1 "Engine diagnostics" 1    -- Create repair job ID 1 for car 1
AddRepair 2 "Brake replacement" 2      -- Create repair job ID 2 for car 2
ShowRepairs                            -- Display all repair jobs
```

### Task Management with Recursion
```
AddTask 1 "Diagnose engine" Nothing 1 (Just 1) (Just InProgress) (Just 2.0)        -- Main task for repair 1
AddTask 2 "Check spark plugs" (Just 1) 1 (Just 1) (Just WaitingForPart) Nothing    -- Subtask of task 1
AddTask 3 "Remove spark plug 1" (Just 2) 1 (Just 1) (Just InProgress) (Just 0.5)   -- Sub-subtask (recursion)
AddTask 4 "Install new spark plug 1" (Just 2) 1 (Just 1) (Just InProgress) Nothing -- Another sub-subtask
ShowTasks                                                                           -- Display all tasks
```

### Parts Management
```
RequestPart 1 "Spark plug" 1 4    -- Request 1 spark plug for task 4
RequestPart 2 "Oil filter" 2 1    -- Request 2 oil filters for task 1
ReceivePart 1                      -- Mark part 1 as received
ShowParts                          -- Display all parts and their status
```

### Mechanic Assignment
```
AddMechanic 1 "Jonas" 3     -- Assign mechanic "Jonas" (ID 1) to task 3
AddMechanic 2 "Petras" 4    -- Assign mechanic "Petras" (ID 2) to task 4
```

### Time and Pricing
```
SetTime 3 0.5      -- Set estimated time for task 3 to 0.5 hours
SetTime 4 1.0      -- Set estimated time for task 4 to 1.0 hour
SetPrice 1 150.0   -- Set total price for repair 1 to 150.0 EUR
```

### Task Completion
```
CompleteTask 3    -- Mark task 3 as completed
CompleteTask 4    -- Mark task 4 as completed
```

### Other Operations
```
Dump Examples    -- Display all example commands
```

## BNF Grammar
```
<program>      ::= <command> | <command> <program>

<command>      ::= "add" "client" <number> <string>
                | "add" "car" <number> <string> <string> "client" <number>
                | "add" "repair" <number> <string> "car" <number>
                | "add" "task" <number> <string> "parent" <number> "repair" <number>
                | "request" "part" <number> <string> "qty" <number> "task" <number>
                | "receive" "part" <number>
                | "add" "mechanic" <number> <string> "task" <number>
                | "set" "time" <number> <number>
                | "set" "price" <number> <number>
                | "complete" <number>
                | "list" ("cars" | "repairs" | "tasks" | "parts" | "clients")
                | "remove" "car"
                | "dump" "examples"

<number>       ::= <digit>
                | <digit> <number>

<digit>        ::= [0-9]

<string>       ::= <char>
                | <char> <string>

<char>         ::= [a-z]