# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:
- Export `mapCont` and `withCont` functions originally added in #70 by @parsonmatt (#139 by @JordanMartinez)

Bugfixes:

Other improvements:
- Fix warnings revealed by v0.14.1 PS release (#139 by @JordanMartinez)

## [v5.0.0](https://github.com/purescript/purescript-transformers/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#133)
- Replaced `ComonadEnv` constraint with `ComonadAsk` in the `asks` function (#131)

New features:
- Added `Semigroup` and `Monoid` instances to `ContT`, `ExceptT`, `MaybeT`, `RWST`, `StateT`, `WriterT` (#115)
- Added `IdentityT` (#121)

Bugfixes:
- Added `Semigroup` and `Monoid` superclasses to `MonadTell` and `MonadWriter`, respectively, to enforce the type class laws (#126)

Other improvements:
- Replaced `void $ modify` with `modify_` in example code (#119)
- Fixed documentation to use `Effect` instead of `Eff` (#124)
- Fixed documentation for `callCC` (#132)
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#134)
- Added a changelog and pull request template (#136, #137)

## [v4.2.0](https://github.com/purescript/purescript-transformers/releases/tag/v4.2.0) - 2019-02-03

- Added `MonadThrow` and `MonadError` instances for `Effect` (@safareli)
- Fixed a typo in documentation (@Thimoteus)

## [v4.1.0](https://github.com/purescript/purescript-transformers/releases/tag/v4.1.0) - 2018-05-27

- Added `modify_` for the old-style `modify`

## [v4.0.0](https://github.com/purescript/purescript-transformers/releases/tag/v4.0.0) - 2018-05-23

- Updated for PureScript 0.12
- The `MonadState` `modify` function now returns the modified state

## [v3.6.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.6.0) - 2018-04-07

- Added `ComonadStore` instances for `EnvT` and `TracedT` (@colin-passiv)

## [v3.5.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.5.0) - 2017-11-23

- Added `Foldable` and `Traversable` instances for `EnvT` (@cryogenian)

## [v3.4.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.4.0) - 2017-06-03

- Added `Monoid` instance for `ReaderT` (@safareli)

## [v3.3.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.3.0) - 2017-05-25

- Added the `writer` function

## [v3.2.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.2.0) - 2017-04-08

- Added `try` for `MonadError` (@rightfold)

## [v3.1.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.1.0) - 2017-03-30

- Added `runListT`, `runListTRec` and `foldlRec` (@matthewleon)

## [v3.0.0](https://github.com/purescript/purescript-transformers/releases/tag/v3.0.0) - 2017-03-26

- Updated for PureScript 0.11
- The `MonadError` class has been split into `MonadThrow` and `MonadError` (@natefaubion)
- Fixed export of `ListT` constructors (@matthewleon)

## [v2.3.0](https://github.com/purescript/purescript-transformers/releases/tag/v2.3.0) - 2017-03-21

- `Lazy` instance for `RWST` (@mlang)
- Added `withResource` (@rightfold)

## [v2.2.0](https://github.com/purescript/purescript-transformers/releases/tag/v2.2.0) - 2017-01-20

- Added `Alternative` and `Plus` instances for `RWST` (@mlang)

## [v2.1.0](https://github.com/purescript/purescript-transformers/releases/tag/v2.1.0) - 2017-01-03

- Added `Alt` instance for `RWST` (@mlang)

## [v2.0.2](https://github.com/purescript/purescript-transformers/releases/tag/v2.0.2) - 2016-11-09

- Fixed name shadowing warnings in PureScript 0.10.2 (@clayrat)

## [v2.0.1](https://github.com/purescript/purescript-transformers/releases/tag/v2.0.1) - 2016-10-11

- Fixed overly-applied type synonyms for `Cont`, `Except`, and `Store`

## [v2.0.0](https://github.com/purescript/purescript-transformers/releases/tag/v2.0.0) - 2016-10-10

This release features a number of breaking changes:
- The signature for `callCC` is now rank-2 to allow for any result to be discarded #74 (@jqyu)
- Functional dependencies have been added to all classes
- `Newtype` instances have been derived for all transformers
- `ListT.scanl` implementation has been fixed #72 (@DanielGronau)
- `MonadReader` has been split into `MonadAsk` and `MonadReader`
- `MonadWriter` has been split into `MonadTell` and `MonadWriter`
- `ComonadEnv` has been split into `ComonadAsk` and `ComonadEnv`
- `MonadTrans` class is now in `Control.Monad.Trans.Class`
- `ComonadTrans` class is now in `Control.Comonad.Trans.Class`
- `ExceptT`'s `apply` and `bind` now behave consistently, at the cost of tightening the `m` constraint to `Monad` for the `Apply` and `Applicative` instance
- The `MonadRWS` class has been removed

## [v1.0.0](https://github.com/purescript/purescript-transformers/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

- Added `Cont` (@parsonsmatt)

## [v0.8.4](https://github.com/purescript/purescript-transformers/releases/tag/v0.8.4) - 2015-11-19

- Fixed issue with re-exports

## [v0.8.3](https://github.com/purescript/purescript-transformers/releases/tag/v0.8.3) - 2015-11-19

- Fixed import warnings raised in psc 0.7.6

## [v0.8.2](https://github.com/purescript/purescript-transformers/releases/tag/v0.8.2) - 2015-11-01

Relax constraints for `WriterT` instances (@xuwei-k)

## [v0.8.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.8.1) - 2015-09-16

- Fixed unused type variable warnings.

## [v0.8.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.8.0) - 2015-09-10

- Added `MonadRec` instance for `RWST`. This changes some details of the RWST implementation so may be a breaking change. (@ethul)

## [v0.7.2](https://github.com/purescript/purescript-transformers/releases/tag/v0.7.2) - 2015-09-03

- Added more instances for `ReaderT` and `RWST` (@ethul)

## [v0.7.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.7.1) - 2015-08-25

This release requires the 0.7.4.0 release of the PureScript compiler. Previous versions of this library will _not_ work with `psc` versions <= 0.7.3.0.
- Simplified instance constraints.
- Module reexports of `Class` modules.
- More instances for `ExceptT`
- `ErrorT` has been removed in favor of `ExceptT`.

## [v0.6.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.6.1) - 2015-07-03

- Added `Distributive` instance for `ReaderT` (@freebroccolo)

## [v0.6.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.6.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

- Added `MonadEff` instances

## [v0.5.5](https://github.com/purescript/purescript-transformers/releases/tag/v0.5.5) - 2015-04-17

- Added `ExceptT` (@hdgarrood)

## [v0.5.4](https://github.com/purescript/purescript-transformers/releases/tag/v0.5.4) - 2015-03-24

- More documentation updates.

## [v0.5.3](https://github.com/purescript/purescript-transformers/releases/tag/v0.5.3) - 2015-03-24

- Updated docs

## [v0.5.2](https://github.com/purescript/purescript-transformers/releases/tag/v0.5.2) - 2015-03-23

- Added `MonadError` instance for `Maybe` (@pseudonom)

## [v0.5.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.5.1) - 2015-02-26

- Fixed `RWST` `Apply` instance. (@joneshf)

## [v0.5.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.5.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.4.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.4.1) - 2015-02-19

- `Error` constraint removed where possible (@pseudonom)

## [v0.4.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.4.0) - 2015-01-10

- Updated `purescript-identity` dependency (@garyb)

## [v0.3.2](https://github.com/purescript/purescript-transformers/releases/tag/v0.3.2) - 2014-12-16

- Added `execWriterT` (@MichaelXavier)

## [v0.3.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.3.1) - 2014-12-11

- Update array dependency (#25)

## [v0.3.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.3.0) - 2014-11-08

- Updates for extracted `Identity` (@garyb)

## [v0.2.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.2.1) - 2014-09-04

- Simplify superinstance constraints for `ErrorT`

## [v0.2.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.2.0) - 2014-09-03

- Moved `Free` and `Trampoline` back to [`purescript-free`](https://github.com/purescript-contrib/purescript-free) (@ethul)

## [v0.1.2](https://github.com/purescript/purescript-transformers/releases/tag/v0.1.2) - 2014-08-31

- `Lazy1` instance for `StateT` (@paf31)

## [v0.1.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.1.1) - 2014-08-27

- `Comonad` and `Extend` instances for `Identity` (@joneshf)

## [v0.1.0](https://github.com/purescript/purescript-transformers/releases/tag/v0.1.0) - 2014-08-11

- Add `Alt`, `Plus`, `MonadPlus`, update `Alternative` (@garyb)

## [v0.0.6](https://github.com/purescript/purescript-transformers/releases/tag/v0.0.6) - 2014-08-05

- All transformers are now defined as `newtype`s (@garyb)

## [v0.0.5](https://github.com/purescript/purescript-transformers/releases/tag/v0.0.5) - 2014-08-02

- Added `Control.Monad.RWS` and `Control.Monad.RWST` (@joneshf)
- Updated FFI code to work for changes in codegen (@garyb)

## [v0.0.4](https://github.com/purescript/purescript-transformers/releases/tag/v0.0.4) - 2014-06-24

- Dropped unnecessary var from type synonyms to prevents problems with partially applied type synonyms later on (garyb)

## [v0.0.3](https://github.com/purescript/purescript-transformers/releases/tag/v0.0.3) - 2014-06-14

- Now uses "proper" `Unit` type instead of `{}` (garyb)

## [v0.0.2](https://github.com/purescript/purescript-transformers/releases/tag/v0.0.2) - 2014-06-08

- Now includes `Control.Monad.Free` (ethul)

## [v0.0.1](https://github.com/purescript/purescript-transformers/releases/tag/v0.0.1) - 2014-05-25

- Initial release
