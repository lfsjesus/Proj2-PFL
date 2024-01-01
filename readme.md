# Claustro - Board Game

**Group:** T12_G04

Luís Filipe da Silva Jesus - up202108683 - 50%
<br>
Miguel Diogo Andrade Rocha - up202108720 - 50%

## Project Description

- This Haskell project is a comprehensive simulation of a programming language processor. It encompasses an `assembler` for executing low-level instructions, and a `compiler` system, complete with a `lexer` and `parser`, to translate and interpret high-level program constructs. The project effectively demonstrates the complete cycle of code processing, from high-level syntax to executable machine instructions.


## Part 1: Interpreter

- The interpreter is built on a `stack-based` architecture, a prevalent model known for its simplicity and efficiency in executing instructions.
- This architecture is characterized by the following key components:

### Instruction Set - (Code):
- This defines the set of operations that the interpreter can perform. The instruction set includes **arithmetic operations** (like addition, subtraction and multiplication), **boolean logic** (like AND operations), and **control flow** commands (such as loops and conditional branches). Each instruction is designed to manipulate the stack and state in a way that reflects the high-level intention of the code.

### Stack:
- The central data structure in this architecture. It's used dynamically during execution to **store** operands and intermediate results. This approach simplifies the evaluation of expressions, especially nested or complex ones.

- We define a custom data type to represent the stack, which is a list of `StackDataType` values. This data type can hold either an `IntValue` or a `BoolValue`. This allows us to store both integer and boolean values in the stack.

 ```haskell
 data StackDataType = IntValue Integer | BoolValue Bool deriving (Show, Eq)

 type Stack = [StackDataType]
 ```

### State:
- It represents the **current context** of the program being interpreted. This includes a mapping from variable names to their corresponding values. The state is crucial for maintaining and updating the values of variables as the program executes.

- We define a custom data type to represent the state, which is a list of `(String, StackDataType)` tuples. This data type can hold a variable name and its corresponding value.
 ```haskell
 type State = [(String, StackDataType)]
 ```

### Executing Instructions:
- The core of the interpreter's execution is encapsulated within the `run` function. This essential component is responsible for interpreting and executing the instructions, represented by the `Code` type. 

- The `run` function operates by sequentially processing the list of instructions contained within `Code`, one instruction at a time.

### Stack and State Manipulation
- For each instruction, we created auxiliary functions which manipulate the `Stack` and `State` as per the instruction's requirements.

- For example, for the `add` instruction, we created:

    ```haskell
    add :: Stack -> Stack 
    add ((IntValue elem1):(IntValue elem2):stack) = (IntValue  (elem1 + elem2)):stack
    add _ = error "Run-time error"
    ```
  - This function adds the top two `IntValues` on the stack, replacing them with their sum.

- For the `store` instruction, we created:

     ```haskell
    storeElem :: String -> Stack -> State -> State
    storeElem varName ((IntValue elem):stack) state = (varName, (IntValue elem)) : filter ((/= varName) . fst) state
    storeElem varName ((BoolValue elem):stack) state = (varName, (BoolValue elem)) : filter ((/= varName) . fst) state
    storeElem _ [] state = error "Run-time error"
    ```
   - This function stores the top element of the stack in the state, with the given variable name, updating the state accordingly.


### Control Flow Instructions
- The instructions `branch` and `loop` are used to control the flow of execution. These instructions are used to implement conditional statements and loops, respectively.

- For example, for the `branch` instruction, we created:

    ```haskell
    branch :: Code -> Code -> Stack -> Code
    branch code1 code2 ((BoolValue elem):stack)
    | elem = code1
    | otherwise = code2
    branch _ _ _ = error "Run-time error"
    ```
  - Combined with the run function, this function evaluates the top element of the stack, and executes the first code block if it's `True`, or the second code block if it's `False`.


### Error Handling

- The interpreter is designed to handle errors that may occur during execution, such as `wrong arguments` for instructions.

- These errors are detected by the interpreter, and are reported to the user.

- In these cases, an exception with the string `"Run-time error"` is thrown, and the program is terminated.


## Part 2: Compiler And Parser



