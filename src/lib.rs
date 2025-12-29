//! # How bare text convert to bytecode or machine code and run
//!
//! User's source code: `var average = (min + max) / 2;`

//! ## Scanning
//!
//! Also known as `lexing` or `lexical analysis`, takes in the characters and
//! converts them into tokens. Single characters `(`, `,`, `.`, numbers `123`,
//! string literals `"hi!"`, identifiers `min` are all tokens.
//!
//! Whitespaces and comments are ignored. So the tokens are
//! `["var", "average", "=", "(", "min", "+", "max", ")", "/", "2", ";"]`.

//! ## Parsing
//!
//! Where our syntax gets `syntactic`. A `parser` builds a tree structure of
//! tokens -- `parse tree` or `abstract syntax tree` or `AST`. The `parser` also
//! report `syntax errors`.
//!
//! ``` markdown
//! average (Stmt.Var)
//! └── / (Expr.Binary)
//!     ├── + (Expr.Binary)
//!     │   ├── min (Expr.Variable)
//!     │   └── max (Expr.Variable)
//!     └── 2 (Expr.Literal)
//! ```

//! ## Static analysis
//!
//! Where we get name refers to local variables or global or else.
//!
//! Most language first do `binding` or `resolution`. We find out where each
//! name defined and wire them together. The `scope` comes to play -- the region
//! of a certain name can be used to refer to a certain declaration.
//!
//! We also check types here, if types don't support being added to each other,
//! we report a `type error`
//!
//! We can decide how to store all the semantics here.
//!
//! Everything up to this point is the `front end`, in early days, there are
//! only `front end` and `back end`. The back end is the final architecture
//! where the program will run. Later researchers invented new phases to stuff
//! between the two halves -- `middle end`

//! ## Intermediate representations
//!
//! In the `middle end`, the code may be stored in `intermediate
//! representation(IR)` that isn't tied to either the source or destination
//! forms.
//!
//! With `IR`, you write one front end for each
//! language that produces the IR. Then one back end for each target
//! architecture.
//!
//! Another big reason we use `IR` is we want to transform the code more
//! apparent.

//! ## Optimization
//!
//! A simple example is constant folding:
//!
//! `pennyArea = 3.14159 * (0.75 / 2) * (0.75 / 2);` => `pennyArea =
//! 0.4417860938`

//! ## Code generation
//!
//! Where generates assembly-like instructions code a CPU runs.
//!
//! If we produce code for virtual machine, it is called `p-code` for portable
//! or `bytecode` because each instruction is often a single byte long.
//!
//! If you produces bytecode, since there is no chip that speaks that bytecode,
//! it's your job to translate. You can write a little mini-compiler to convert
//! bytecode for that chip. Or you can write a `virtual machine(VM)`, running
//! bytecode in a VM is slower than translating it to native code ahead of time
//! because every instruction must be simulated at runtime each item it executes
//! In return, you get simplicity and portability.

//! ## Runtime
//!
//! If we don't RAII, we need GC or we need meta object like `instanceof`, we
//! need to implement runtime.

//! # Single-pass compiler
//!
//! Who interleave parsing, analysis, and code generation without ever
//! allocating any syntax trees or IRs. ou have no intermediate data structures
//! to store global information about the program, and you don’t revisit any
//! previously parsed part of the code.

//! Tree-walk interpreter
//!
//! Execute code right after parsing it to AST(with maybe a bit of static
//! analysis).

//! Transpiler
//!
//! After front end, instead of lower the semantics, you produce a string of
//! valid source code for some other high-level language.

//! Just-in-time compilation
//!
//! On the end user's machine, when the program is loaded, you compile it to
//! native code for the chip. The most sophisticated JITs insert profiling hooks
//! into the generated code to see which regions are most performance critical
//! and what kind of data is flowing through them. Then, over time, they will
//! automatically recompile those hot spots with more advanced optimizations.

pub mod cli;
mod environment;
mod error;
mod interpreter;
mod loxer;
mod parser;
mod scanner;
mod statement;

pub use error::{LoxError, scanner::{ScanError, ScanErrorType, ScannerError}};
pub use loxer::Loxer;
