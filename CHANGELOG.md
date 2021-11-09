# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v5.0.1](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v5.0.1) - 2021-11-09

Other improvements:
- Fixed typos in the LICENSE file that made it impossible for `licensee` to detect the license (#29 by @maxdeviant)

## [v5.0.0](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#24)

New features:
- Added roles declarations to forbid unsafe coercions (#20) 

Bugfixes:

Other improvements:
- Updated directory structure to match module name for `Effect.Aff.Bus` (#16)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#21)

## [v4.0.0](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v4.0.0) - 2018-11-26

- Updated for PureScript 0.12.x (@safareli)
- Fixed leak in make loop (@safareli)
- Fixed `read` to not block forever when a bus is killed (@safareli)

## [v3.1.0](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v3.1.0) - 2018-04-12

- Relaxed `make` to use a `MonadEff` constraint rather than concrete `Aff` (@safareli)

## [v3.0.0](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v3.0.0) - 2017-11-22

- Updated for Aff v4 and new AVar

## [v2.0.0](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v2.0.0) - 2017-04-04

- Updated for PureScript 0.11

## [v1.0.0](https://github.com/purescript-contrib/purescript-aff-bus/releases/tag/v1.0.0) - 2016-11-23

- Initial release.
