# Revision history for haskell-candid

## 0.4.0.1 -- 2023-01-01

* Bump dependencies
* GHC-9.4 and transformer-0.6 compat

## 0.4 -- 2022-11-05

* Fix did file parsing bug: Allow underscores in unicode escapes
* Implement the new subtyping rules from spec version 0.1.4
  https://github.com/dfinity/candid/pull/311

## 0.3.2.1 -- 2022-12-01

* GHC-9.2 compatibility

## 0.3.2 -- 2022-10-07

* Candid hash reversal: Also try upper case

## 0.3.1 -- 2022-01-10

* GHC-9.0 compatibility
* Import type definitions, not just service types from `.did` files
* The Candid file parser ignores init arguments

## 0.3 -- 2021-10-01

* Candid pretty-printing: Try to invert field names using word list

## 0.2 -- 2021-06-17

* Guess field named when only the hash is known
* Better support for reference types
* Implement the “new” subtyping rules in Candid
* Template Haskell: Recognize tuples

## 0.1 -- 2020-11-21

* First version.
