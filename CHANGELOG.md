# Changelog

All notable changes to this project will be documented in this file.

## [9.7.18] - 2025-01-13

### Features

- Show negative numbers in Inspector ([#304](https://github.com/Consensys/corset/issues/304))
- Update inspector key binding for "clear filter" ([#307](https://github.com/Consensys/corset/issues/307))

## [9.7.17] - 2024-11-13

### Bug Fixes

- Computed columns with negative domain ([#298](https://github.com/Consensys/corset/issues/298))

### Features

- Support checking for negative domain ([#296](https://github.com/Consensys/corset/issues/296))

## [9.7.16] - 2024-11-04

### Bug Fixes

- Column setters in trace template ([#290](https://github.com/Consensys/corset/issues/290))

### Features

- Support filtering of perspectives ([#281](https://github.com/Consensys/corset/issues/281))
- Enabling scrolling of tabs ([#282](https://github.com/Consensys/corset/issues/282))
- Add bit-decomposition to stdlib ([#286](https://github.com/Consensys/corset/issues/286))
- Perspective constancy function ([#288](https://github.com/Consensys/corset/issues/288))
- Allow empty `(begin ..)` ([#289](https://github.com/Consensys/corset/issues/289))
- Improved error reporting in generated `Trace.java` ([#291](https://github.com/Consensys/corset/issues/291))

## [9.7.15] - 2024-10-16

### Bug Fixes

- Fix `Trace.java` template ([#270](https://github.com/Consensys/corset/issues/270))
- Trace file template (([#278](https://github.com/Consensys/corset/issues/278))

### Features

- Update implementation of `eq!` ([#269](https://github.com/Consensys/corset/issues/269))
- Support custom class name for trace file. ([#272](https://github.com/Consensys/corset/issues/272))
- Setting column size factor manually ([#274](https://github.com/Consensys/corset/issues/274))

## [9.7.14] - 2024-10-03

### Bug Fixes

- Fixes for `ratatui` dependency issue ([#216](https://github.com/Consensys/corset/issues/216))
- Truncated Column Names ([#226](https://github.com/Consensys/corset/issues/226))
- Different Behavior for Expansion and Native Flag Combinations ([#228](https://github.com/Consensys/corset/issues/228))
- Update `check` reporting for perspectives ([#229](https://github.com/Consensys/corset/issues/229))
- Update Github Release Action ([#237](https://github.com/Consensys/corset/issues/237))
- Wrong Evaluation of Constraint Expressions using Expansion ([#242](https://github.com/Consensys/corset/issues/242))
- Rework `ifs` transform ([#245](https://github.com/Consensys/corset/issues/245))
- Remove vector operations

### Features

- Support `RON` and `JSON` bin formats ([#223](https://github.com/Consensys/corset/issues/223))
- Support Perspective "blanking" ([#224](https://github.com/Consensys/corset/issues/224))
- Switch default `bin` format from `RON` to `JSON` ([#225](https://github.com/Consensys/corset/issues/225))

## [9.7.12] - 2024-06-20

### Bug Fixes

- Custom `Serializer` for `ColumnRef` ([#184](https://github.com/Consensys/corset/issues/184))
- Fix for Singleton Reduce ([#192](https://github.com/Consensys/corset/issues/192))
- Error when missing reduce ([#194](https://github.com/Consensys/corset/issues/194))
- Fix for unexpected list on `*` ([#196](https://github.com/Consensys/corset/issues/196))
- Unused Declaration for Array Columns (`define.go`) ([#197](https://github.com/Consensys/corset/issues/197))
- Fix #178 ([#202](https://github.com/Consensys/corset/issues/202))
- Incorrect Handling of Perspective Selectors ([#204](https://github.com/Consensys/corset/issues/204))
- Incorrect Trace Construction ([#208](https://github.com/Consensys/corset/issues/208))
- Fix #209 ([#210](https://github.com/Consensys/corset/issues/210))

### Features

- Implement `byte@prove` using range constraints ([#182](https://github.com/Consensys/corset/issues/182))

## [9.7.11] - 2024-06-05

### Bug Fixes

- Fix `ifs::raise_ifs` #87 ([#97](https://github.com/Consensys/corset/issues/97))
- Remove old `to_string` invokation ([#109](https://github.com/Consensys/corset/issues/109))
- Fix Default Base for Interleaved Constraints ([#114](https://github.com/Consensys/corset/issues/114))
- Rendering of columns in `convert` ([#110](https://github.com/Consensys/corset/issues/110))
- Erroneous inversion optimization ([#108](https://github.com/Consensys/corset/issues/108))
- Fix for `eq!` typing in `stdlib.lisp` ([#145](https://github.com/Consensys/corset/issues/145))
- Fix for `corset compute` ([#156](https://github.com/Consensys/corset/issues/156))
- Fix #158 ([#159](https://github.com/Consensys/corset/issues/159))
- Incorrect typing of vector operations ([#175](https://github.com/Consensys/corset/issues/175))

### Features

- Add missing bigInt package ([#137](https://github.com/Consensys/corset/issues/137))

### Miscellaneous Tasks

- Release `corset` v9.7.11

### Build

- Bump the cargo group across 1 directory with 2 updates ([#103](https://github.com/Consensys/corset/issues/103))

## [9.7.10] - 2024-03-07

### Features

- Add force-bin

### Miscellaneous Tasks

- Release corset version 9.7.10

### Refactor

- :nowarn -> :force

## [9.7.9] - 2024-03-06

### Bug Fixes

- Symbol resolving in array indexing operation
- Refactor perspectives in interleavings & sorts

### Features

- Better error message on erroneous module access

### Miscellaneous Tasks

- Release corset version 9.7.9

## [9.7.8] - 2024-03-05

### Bug Fixes

- Allow the use of array columns in perspectivized interleaving

### Miscellaneous Tasks

- Release corset version 9.7.8

## [9.7.7] - 2024-03-03

### Bug Fixes

- Permutations & interleave over expressions

### Miscellaneous Tasks

- Release corset version 9.7.7

## [9.7.6] - 2024-03-02

### Bug Fixes

- Carry typing information over interleaving & permutations

### Miscellaneous Tasks

- Release corset version 9.7.6

## [9.7.5] - 2024-02-28

### Bug Fixes

- Refuse to import erroneous values for Fr elements
- Allow for unknown columns in binary trace

### Features

- Clearer lookup error messages
- Add conversion to JSON
- Make mismatching column lengths a non-recoverable error

### Miscellaneous Tasks

- Release corset version 9.7.5

## [9.7.4] - 2024-02-26

### Bug Fixes

- Wrong table layout obsucring elements
- Bool -> loob
- Display full file path on compile errors
- Wrong perspective colors

### Features

- Add a high contrast setting
- Add boolean version of chronologic testing operators
- Inverse pure expressions in place

### Miscellaneous Tasks

- Release corset version 9.7.4

### Refactor

- Streamline function declaration parsing

## [9.7.3] - 2024-02-13

### Bug Fixes

- Specialization selection mechanism

### Miscellaneous Tasks

- Release corset version 9.7.3

## [9.7.2] - 2024-02-12

### Bug Fixes

- Stepped ranges inclusion check

### Miscellaneous Tasks

- Release corset version 9.7.2

## [9.7.1] - 2024-02-12

### Bug Fixes

- Correct in-boundness check for stepped ranges

### Miscellaneous Tasks

- Release corset version 9.7.1

## [9.7.0] - 2024-02-12

### Features

- Allow for any const. expr. in domains

### Miscellaneous Tasks

- Release corset version 9.7.0

## [9.6.0] - 2024-02-07

### Bug Fixes

- Column sizes
- Better error messages when function arguments count is wrong
- Text representation of binary magma
- Type error message

### Features

- Expose constant as public
- Use fitting types to avoid wasting space in trace files

### Miscellaneous Tasks

- Update dependencies
- Release corset version 9.6.0

## [9.5.0] - 2024-01-24

### Bug Fixes

- Homogeneize arrays and columns

### Miscellaneous Tasks

- Release corset version 9.5.0

## [9.4.0] - 2024-01-23

### Bug Fixes

- Ensure all backings have the correct length
- Message on conditioning error
- Architecture-dependent build

### Features

- Generate normalization constraints
- Generate binarity constraints for `@prove`-annotated type

### Miscellaneous Tasks

- Clippy
- Release corset version 9.3.0
- Release corset version 9.4.0

### Refactor

- Homogeneize column types proving

## [9.2.3] - 2024-01-17

### Bug Fixes

- Sort constraints auxiliary columns computation
- Fr/BigInt criss-crossing
- Parsed value can be negative

### Features

- Better error messages on erroneous value updating

### Miscellaneous Tasks

- Release corset version 9.2.3

### Refactor

- Move columns_len to ConstraintSet
- Streamline concretization

### Fest

- Add debug information for binary trace map

### Hack

- Circumvent expression-backed computation issue

## [9.2.2] - 2024-01-12

### Bug Fixes

- Interleaved columns definition in WizardIOP

### Miscellaneous Tasks

- Release corset version 9.2.2

## [9.2.1] - 2024-01-12

### Bug Fixes

- Type parsing

### Miscellaneous Tasks

- Release corset version 9.2.1

## [9.2.0] - 2024-01-12

### Bug Fixes

- Error handling
- Exo-column conversion criterion
- Fail on erroneous types

### Features

- Add a CSV conversion option

### Miscellaneous Tasks

- Update the WizardIOP exporter
- Release corset version 9.2.0

## [9.1.5] - 2024-01-11

### Bug Fixes

- Handle all errors while checking
- Handle Integer magma in Besu export
- Type compatibily error

### Miscellaneous Tasks

- Release corset version 9.1.3
- Release corset version 9.1.4
- Release corset version 9.1.5

### Styling

- Only use `@loob` and `@bool` ([#62](https://github.com/Consensys/corset/issues/62))

### Build

- Feature-gate sqlite export
- Generate binaries on release
- Fix git-cliff configuration

## [9.1.2] - 2023-12-21

### Bug Fixes

- Correct types for `~or!` and `~and!` ([#57](https://github.com/Consensys/corset/issues/57))
- Conditioning should never be mixed

### Documentation

- Add inspector demo

### Features

- The `convert` command converts trace files to sqlite database

### Miscellaneous Tasks

- Update dependencies
- Release corset version 9.1.2

### Build

- Bump zerocopy from 0.7.26 to 0.7.31 ([#54](https://github.com/Consensys/corset/issues/54))

## [9.1.1] - 2023-12-06

### Bug Fixes

- Superfluous logs
- Unwrap

### Features

- Sort module tabs
- Add `o/open` flag to directly go to a module tab

### Miscellaneous Tasks

- Release corset version 9.1.1

## [9.1.0] - 2023-12-01

### Miscellaneous Tasks

- Release corset version 9.1.0

## [9.0.0] - 2023-11-27

### Bug Fixes

- Globally rename plookup to lookup
- Ensure that inspector colors are readable
- Sort constraints
- Importing traces as native values
- Conditioning of guards
- Concretize computations
- Escaping function
- Correctly mark used columns
- Render permutations & domains
- Apply all auto-constraints
- Adapt default Value to native/non-native
- Columns import/export for BLS
- Lookup checking on non-expanded constraints
- Build process
- Improve checking error reporting

### Features

- Add an option do display module spillings
- Switch from BN to BLS
- Implement :comp columns
- Adapt for MMAP export in Besu
- Flat binary file parser

### Miscellaneous Tasks

- Update dependencies
- Clippy
- Update dependencies
- Update tui-textarea
- Release corset version 9.0.0

### Performance

- Improve flat file importing performances
- Optimize register parsing

### Build

- Streamline the build process

## [9.0.0-alpha] - 2023-11-02

### Bug Fixes

- Ensure that generated Besu files implement `ModuleTrace`

### Features

- Add vertical scrolling
- Prettier plookups
- Introduce field-agnosticity and related type-system revision ([#40](https://github.com/Consensys/corset/issues/40))

### Miscellaneous Tasks

- Release corset version 9.0.0-alpha

### Besu

- Pre-allocate column buffers
- Fine-typing of constants
- Update to new packages

## [8.3.9] - 2023-09-29

### Bug Fixes

- Do not ignore explicit base annotation on column declarations

### Miscellaneous Tasks

- Release corset version 8.3.9

## [8.3.8] - 2023-09-28

### Features

- Add `:truthiness` as a display option

### Miscellaneous Tasks

- Release corset version 8.3.8

## [8.3.7] - 2023-09-28

### Bug Fixes

- Use appropriate type for constants in Besu
- Downgrade ron due to stack overflow issue

### Features

- Remove setter methods in generated traces
- Add an option to display columns
- Corset can now parse module following the standard hierarchy

### Miscellaneous Tasks

- Clippy
- Release corset version 8.3.7

### Build

- Update to Rust 1.70.0

## [8.3.6] - 2023-09-16

### Bug Fixes

- Besu templates
- Make register allocation stable

### Features

- Colorize numbers
- Columns or register can both be loaded from trace files
- Colorize perspective columns only when active
- Add a `iota` keyword to create distinct constants
- Accept STDIN as an input with `-`

### Miscellaneous Tasks

- Update dependencies
- Release corset version 8.3.6

## [8.3.5] - 2023-08-30

### Bug Fixes

- Dim/non-dim formatting

### Miscellaneous Tasks

- Release corset version 8.3.5

## [8.3.4] - 2023-08-30

### Bug Fixes

- Homogeneize shift display

### Miscellaneous Tasks

- Release corset version 8.3.4

## [8.3.3] - 2023-08-30

### Bug Fixes

- Correct format for subponents

### Miscellaneous Tasks

- Release corset version 8.3.3

## [8.3.2] - 2023-08-30

### Bug Fixes

- Only show column names in checker

### Miscellaneous Tasks

- Release corset version 8.3.2

## [8.3.1] - 2023-08-30

### Bug Fixes

- Erroneous display of checker reports

### Miscellaneous Tasks

- Release corset version 8.3.1

## [8.3.0] - 2023-08-30

### Bug Fixes

- Stop using STDIN

### Miscellaneous Tasks

- Release corset version 8.3.0

## [8.2.0] - 2023-08-28

### Bug Fixes

- OoB in Combinator display
- Incoherences in the type system regarding cyclic validation

### Features

- Add `corset inspect` to visualize trace files
- Add a Forth micro-language to select rows in the inspector
- Specify columns potentially missing bool annotation
- Opcodes can be used as immediate values
- Always save valid scan expressions
- Corset code can be provided from STDIN

### Miscellaneous Tasks

- Rename jobs
- Github++
- Clippy
- Clippy
- Release corset version 8.2.0

### Refactor

- Split inputs in their own modules

### Build

- Fix conditional compilation
- Drop dependency on names

## [8.1.3] - 2023-08-22

### Miscellaneous Tasks

- Release corset version 8.1.3

### Build

- Fix release.yml

## [8.1.2] - 2023-08-22

### Miscellaneous Tasks

- Release corset version 8.1.2

### Build

- Fix release.yml

## [8.1.1] - 2023-08-22

### Miscellaneous Tasks

- Release corset version 8.1.1

### Refactor

- Revamp domain implementation

### Build

- Add release.yml

## [8.1.0] - 2023-08-18

### Features

- Allow for dropping individual columns
- Add a formatter

### Miscellaneous Tasks

- Release corset version 8.1.0

## [8.0.2] - 2023-08-10

### Bug Fixes

- Erroneous implementation of perspective scopes
- Erroneous parsing of column type annotations

### Features

- Add warning on potentially overflowing `if` conditions
- `for` can iterate over ranges, compile-time values, or arrays
- Change besu java format trace generation format
- Unclutch debug report from log level, add trace span options
- Add a `size` method to zkBesu traces
- `fillRemaining` zero-fills empty columns in zkBesu

### Miscellaneous Tasks

- Comments & re-organization
- Release corset version 8.0.2

## [8.0.1] - 2023-07-26

### Bug Fixes

- Columns have a magma, not a type
- Add SPDX field in zkBesu template file ([#19](https://github.com/Consensys/corset/issues/19))
- Perspective guard should not alter constraint type ([#21](https://github.com/Consensys/corset/issues/21))
- AVX-specific argument type ([#17](https://github.com/Consensys/corset/issues/17))

### Features

- Add a flag to the debugger to display modules infos

### Miscellaneous Tasks

- Prune old code
- Release corset version 8.0.1

### Refactor

- Add syntactic sugar for job-DAG creation

## [8.0.0] - 2023-07-18

### Bug Fixes

- Extend permutation signs; do not fail parsing on empty modules
- Issue #11
- Inverse constraint generation
- Mark used arrays as used
- Re-enable SIMD
- Handles/ID confusion in lib
- JSON export in conflated traces
- Cbindgen update
- When loading columns, pad both at top and bottom
- Booleans are stable by inversion
- Dynamically type user-defined function calls when possible
- All column expression must be typed
- Tests
- Correctly compute padding for double-past spilling
- Use local rayon thread pools in the FFI
- Missing argument on x86/AVX2 build
- Do not crash on Void constraints
- Only takes own constraints into account when computing spilling
- If applicable, pad imported columns with their defined value
- Typing of `eq!`
- Embed handle information when available
- Hash computation for `ColumnRef`
- "namespace-ize" perspective columns in zkGeth export
- Tests
- Concurrent borrow
- Update zkBesu exporter to reigsters
- Do not print *all* columns in debug
- Discern casting and operation result typing
- Always sort handles while debugging
- Don't output too long lines in the conflater
- Better error messages
- Besu export
- Erroneous symbols in WizardIOP
- Cgo antics
- Mixup between computations ID & columns ID
- Revert to old serialization
- Enable sorts in wizard
- Expand string purification

### Features

- Add cross-perspective references
- Better error message on columns loading
- Implement binary column representations
- Improve debugging outputs
- Add optional typing to user-defined functions
- Implement loobeans
- The debugger can now display types
- Add debug information on mismatching columns length
- Show as many compile error as possible at once
- Implement the new perspective behavior
- Colorize debugger output
- Add function overloading
- Condense debugging output
- Add the `--debug-src` flag
- Allow for a debug-oriented representation of the AstNodes
- Warn on potentially mistyped columns

### Miscellaneous Tasks

- N-hood columns do not need to be viewed as binary
- Update the Kotlin template
- Clippy
- Release corset version 8.0.0

### Performance

- Optimize self-inverts of booleans as identity
- Pre-compute i64->Fr user-set padding values

### Refactor

- Use a product type for handles for clearer output
- Colored -> owo_colors
- Not & Eq are builtin rather than intrinsics
- `Node` members are now private
- Homogeneize stdlib function names
- Stdlib renaming
- Homogeneize column create API
- Drop the `--allow-dups` flag
- Implement `SelfInv` in Corset
- Replace `:interleaved` by `definterleaved` ([#14](https://github.com/Consensys/corset/issues/14))
- Implement the new sizes system in WizardIOP exporter
- Go -> zkGeth

### Build

- Hide parser & exporters behind feature flags
- Simplify AVX feature gate

### Ffi

- Add an error to string converter

### Hack

- Temporary revert to old size scheme
- Disable target-cpu

### Reverse

- No CPU-specific code

## [8.0.0-rc] - 2023-05-25

### Bug Fixes

- Do not hard fail on missing columns
- Missing spills
- Better implementation of pretty_with_base
- Fix parsing of hex & bin values
- Postgres feature
- Do not output SIZE constants for the wizard
- Flatten if within other function calls
- Missing case
- `defpermutation` failing on aliases
- Fail when defining invalid alias
- Insert constant symbols before computing them in a later pass
- Forgotten debug
- Besu templates
- Misunderstood SSE documentation
- Also convert padding to BE
- &str -> &[u8]
- Cross-module symbol referencing
- Insert array subcolumns earlier for easier lookup
- Global symbols lookup must start at the root scope
- Can address foreign symbols
- Introduce global/local scopes
- Ensure that JSON names are exported to the conflater
- Do not crash on empty loop bodies
- Warn when a constraint will not be checked

### Documentation

- Explicit linking modes in Go example

### Features

- Optimize trace expansion speed
- Add caching to JSON import/export
- Add a `--fail-on-missing` flag
- Add a `:padding` setting to columns
- Add a `force-bool` form
- Expose libcorset through FFI
- Leverage errno for error codes
- Implement `reduce`; re-organize function hierarchy
- Add `todo` form
- Add a `debug` command
- Accept hexadecimal and binary constants
- `todo` can take any number of arguments
- Add `did-inc` and `did-dec` to the stdlib
- Add `either` to the standard library
- Add a setting to set display base
- Improve debug output
- Add a trace-checking function to the FFI
- Expose a function to load the constraint system from a string
- Expand the FFI interface
- Add java export for zkBesu
- Improve LaTeX export
- Add more time-related logging
- Add (SIMD) big-endianness conversion in FFI
- Use simd_json for better performances when available
- Render plookups in debug mode
- Implement partially sorted permutation constraints
- Add a conflater-specific exporter
- Export AllColumns for zkGeth
- Warn when a for loop body evals to empty
- Add columns to the debugging output
- Emit warnings for useless use of force-bool
- Implement perspectives
- Add the `~` self-inversion operator

### Miscellaneous Tasks

- Cleanup
- Formatting
- Clippy
- Update dependencies
- Clippy
- Update dependencies
- Add git-cliff as dependency
- Prettier debugger
- Clippy
- Update dependencies
- Release corset version 8.0.0-rc

### Performance

- Parallelize columns rendering in FFI

### Refactor

- Move read_trace to the compute module
- Split zkEVM parsing from trace computation in FFI
- Move symbol & computation tables to their own module
- Drop useless code
- Simplify unfallible function
- Simplify the zkGeth exporter
- Modularize errors
- Convert Expression::(Array)Column from tuple to struct
- Use sorbus to implement the scope tree

### Build

- Always optimize dependencies

### Debug

- Add logs

## [7.1.0] - 2023-02-17

### Bug Fixes

- Range number formatting in WizardIOP
- Programatically filled columns need a way to set a size factor
- Disable n-hood constraints in Wizard
- Pad columns of modules containing range proofs
- Padding value for Eq columns
- Padding for Eq columns
- The WizardIOP range constraints are <, not <=

### Miscellaneous Tasks

- Clippy
- Release corset version 7.1.0

### Refactor

- Simplify Wizard export of interleaved columns

## [7.0.0] - 2023-02-13

### Bug Fixes

- Double borrow
- Missing ID updates
- Edge cases in @ constraints for sorted columns
- `Not` typing
- Guard system
- Typing of `Not`

### Features

- Add an alternative `[X i]` notation for array indexing
- Type now implements Ord
- Add private symbol tables
- Interleave can now take expression arguments
- Improve error handling for incorrect numeric values
- Enable more transformations for `check-loop`
- CheckLoop test all extensions

### Miscellaneous Tasks

- Typo
- Release corset version 7.0.0

### Refactor

- Streeamline types verification
- Minor factorization
- Add new errors
- New error type
- Re-organize transformations

### Testing

- Fix regression

## [6.2.0] - 2023-02-05

### Bug Fixes

- Alias natural to integer
- Regression on supernumerary `DEFUN` arguments
- Wrong typing of array columns

### Features

- Add the `LET` form
- Add the `LEN` builtin function
- Compute static expressions at compile time

### Miscellaneous Tasks

- Release corset version 6.2.0

### Refactor

- Introduce testing helpers

## [6.1.0] - 2023-02-03

### Miscellaneous Tasks

- Release corset version 6.1.0

### Refactor

- Streamline the parsing phase
- Switch from `Err(anyhow!(...))` to `bail!(...)`

## [6.0.0] - 2023-02-02

### Bug Fixes

- Unclutter lookup errors

### Features

- Introduce a new :guard/:domain feature

### Miscellaneous Tasks

- Reformat the stdlib
- Release corset version 6.0.0

### Refactor

- Better variable name
- Drop ComputeLoop

## [5.1.0] - 2023-01-28

### Bug Fixes

- Inverted permutation definition
- Sort constraint should not fail when there is no padding
- Do not fail while validating too short cyclic columns
- Better error messages on empty columns
- Overflow in powers of 256 projected on isize

### Documentation

- Add documentation to common types

### Features

- Avoid useless shift in WizardIOP local constraints
- Implement the n-hood constraints
- Detailed debugging now works with negative indices
- Add a --pretty flag to the compiler
- Check in-range constraints
- Implement sort constraint proving
- Remove data generation not required by zkGeth anymore

### Miscellaneous Tasks

- Renaming
- Add helper functions
- Clippy
- Go export of constraints is not required anymore
- Release corset version 5.1.0

### Refactor

- Drop the make-decomposition builtin
- Consolidate post-compilation transformations

## [5.0.1] - 2023-01-22

### Bug Fixes

- Erroneous padding for Phantom columns

### Miscellaneous Tasks

- Release corset version 5.0.1

## [5.0.0] - 2023-01-20

### Bug Fixes

- Do not crash at build if git fails

### Features

- Sort expressions in failure traces
- Add a better debugging view
- Highlight failing path when debugging constraints
- Bold-ize the failing columns in traces
- Add a --rerun option to CheckLoop
- CheckLoop can now report
- Add the --debug-dim flag
- Implement the new generalist padding system

### Miscellaneous Tasks

- Clippy
- Release corset version 5.0.0

### Refactor

- Generalize the eval function
- Unify the debugging settings
- Tracing handling
- Split generator.rs

## [4.0.0] - 2023-01-12

### Bug Fixes

- Sparse checking output
- Forgotten arguments
- WizardIOP export
- Plookup rendering in WizardIOP
- In range constraints in WizardIOP
- Interleaved columns should actually exist
- Ensure that columns involved in permuations are marked as used
- Latest WizardIOP API
- Clearer names in WizardIOP
- Handle should be hashed on their name
- Revamp the padding system to handle dependencies
- Implement spilling in padding
- Solve constraint names overlaps

### Features

- Improve SQL processing
- Keep track of wheter columns are being used in exporters
- Check inversion constraints when expanding
- Prettier WizardIOP names
- Add git hash in `--version`

### Miscellaneous Tasks

- No need to export inlined constant in the WizardIOP
- Stderrlog -> buche
- Clippy
- Update dependencies
- Release corset version 4.0.0

### Refactor

- Constraints are now identified by handle
- Cleanup

## [3.2.1] - 2023-01-09

### Features

- Only show error context when logLevel >= warn

### Miscellaneous Tasks

- Release corset version 3.2.1

## [3.2.0] - 2023-01-09

### Bug Fixes

- Symbols with path should be resolved from the root context
- Clarify mangling

### Features

- Specify which column failed importation

### Miscellaneous Tasks

- Formatting
- Release corset version 3.2.0

## [3.1.2] - 2023-01-07

### Bug Fixes

- Remove use of bool::then_some for compatibility motives

### Miscellaneous Tasks

- Release corset version 3.1.2

## [3.1.1] - 2022-12-30

### Bug Fixes

- Array columns are not double-declared anymore

### Documentation

- Update README

### Features

- Primes are accepted in symbol names

### Miscellaneous Tasks

- Release corset version 3.1.1

## [3.1.0] - 2022-12-29

### Features

- Sort elements in WizardIOP export
- Pure functions can access constants

### Miscellaneous Tasks

- Release corset version 3.0.0
- Release corset version 3.1.0

### Refactor

- Overhaul implementation of typing system

### Testing

- Add an if-eq-else test

## [2.0.0] - 2022-12-18

### Miscellaneous Tasks

- Release corset version 2.0.0

## [1.2.0] - 2022-12-18

### Bug Fixes

- Fix DEFUNALIAS
- Fix non-deterministic behavior due to hashmaps
- Fix Defconst
- Fix BranchIfElse
- Fix Go export
- Fix
- Fix module
- Fix multiple constants definition
- Fixed missing comma
- Fix naming conventions
- Fix inverted columns
- Fix columns computation steps ordering
- Fix mistake for in-range
- Fix relative indexing
- Fix relative indexing
- Fix constraint checking
- Fixes
- Fix negative moduli
- Fix ‶unreachable″ code
- Fix wrapping
- Fix go exporter
- Fix off-by-one error
- Fix wrong indices
- Fix Go export
- Fix permutation
- Fix padding
- Fix Binary/NOT padding
- Fix JSON export
- Fix
- Fix
- Fix missing initialization
- Fix
- Fix didnt-change
- Fix postgres
- Fix compile-time evaluation of constants
- Fix unused symbols detection
- Fix mangling
- Fix(?) JSON export
- Fix partial padding
- Fix binary/not padding
- Fix hashing of Handles
- Fix computation & export of columns
- Fix erroneous static if folding
- Fix
- Fix the computation not being shared across contexts
- Fix if-eq
- Fix padding of computed columns

### Miscellaneous Tasks

- Release corset version 1.2.0

<!-- generated by git-cliff -->
