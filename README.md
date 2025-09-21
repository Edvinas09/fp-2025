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
This DSL models a car garage.  
Main entities:
- **Cars** — ID, Model and license plate.
- **Repairs** — linked to cars, describes the repair.
- **Tasks** — describse work steps inside a repair. Tasks may have subtasks, creating recursion.

Operations:
- `add car <id> "<model>" "<plate>"` — register a car.
- `add repair <id> "<title>" car <carId>` — add a repair for a car.
- `add task <id> "<title>" [parent <taskId>]` — add a task, optionally as a subtask.
- `complete <id>` — mark a task as finished.
- `list <cars|repairs|tasks>` — list items.
- `remove <id>` — remove a finished repair after the client has taken the car.
- `dump examples` — show DSL example commands.

Recursion arises because **tasks can contain subtasks** arbitrarily deep.

---

## BNF Grammar
```
<program>      ::= <command> | <command> <program>

<command>      ::= "add" "car" <number> <string> <string>
                | "add" "repair" <number> <string> <number>
                | "add" "task" <number> <string>
                | "add" "task" <number> <string> "parent" <number>
                | "complete" <number>
                | "list" "cars"
                | "list" "repairs"
                | "list" "tasks"
                | "remove" "car"
                | "dump" "examples"

<number>       ::= <digit>
                | <digit> <number>

<digit>        ::= [0-9]

<string>       ::= <char>
                | <char> <string>

<char>         ::= [a-z]
```
