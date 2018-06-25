## Reflex Native

### Fully native apps using Reflex

_Caution:_ This README contains forward looking statements. See [the project status](#project-status)

Reflex Native is a framework for writing fully native apps using [Reflex](https://github.com/reflex-frp/reflex/), a
[Functional Reactive Programming](https://wiki.haskell.org/Functional_Reactive_Programming) library for Haskell.

It provides a [cross-platform layer](#cross-platform) on top of several platform specific libraries for writing components and applications which run on iOS
or Android fully native with little to no compromise in resulting app quality - that is, executing as native ARM binaries and using the platform's UI toolkit,
no JavaScript runtime nor web views.

### iOS prerequisites

Reflex Native UIKit and apps that use it can only be built on a Mac with Xcode installed at `/Applications/Xcode.app` with an iPhoneOS SDK of the version that
`reflex-platform` expects, currently 10.2.

You can get this version by downloading Xcode 8.2.1, unpacking it, and copying

`Xcode 8.2.1.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS10.2.sdk`

to

`/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS10.2.sdk`

You don't need to actually use Xcode 8.2.1, but that's the version of Xcode that has the 10.2 SDK in it.

### Android prerequisites

TBD :'(

### Getting started

First, see [the project status](#project-status) for cautions about the current immature state of this project.

#### Building the examples for iOS

Use `nix-build` with the derivations under `examples` in `default.nix`, for example `nix-build -A examples.ios.draggy` to build `examples/draggy` as an iOS app.
`nix-build` by default makes a link to the build result called `result`, and the apps include packaging and deployment scripts, so continuing the example you
could deploy the draggy example app to an attached iPhone using `result/bin/deploy <team>` where `<team>` is your Apple developer team ID.

#### Building the examples for Android

TBD :'(

#### Using in your own project

TBD :'(

### Developing Reflex Native itself

tl;dr: use `make host`, `make android`, or `make ios`.

Shells are provided for each of the platforms as attributes in `default.nix`:

* `shells.host` for headless UI testing and the cross-platform components only.
* `shells.ios` for iOS and the cross-platform components. See [iOS preqrequisites](#ios-prerequisites).
* `shells.android` for Android and the cross-platform components.

You can enter each of these with `nix-shell`, e.g. `nix-shell -A shells.ios`, and then use `cabal new-build` to do incremental builds within the shell.
`cabal new-build` uses a project file to determine what packages to build and any configuration overrides to use when building them, and one is provided for
each platform:

* `host.project`
* `ios.project`
* `android.project`

So for example to do an incremental build of the iOS components:

1. `nix-shell -A shells.ios`
2. `cabal --project-file=ios.project --builddir=_build/ios/dist new-build all`

However as a development environment this leaves some things to be desired:

* It's tedious to type every time
* It doesn't work well with editor build functions which are typically not running inside the `nix-shell`
* `nix-shell` takes a few moments to start even when it has nothing to build (exacerbating the previous issue)

So, a `Makefile` is provided with targets for each platform which also builds each shell once and caches the environment.

Make targets:

* `make host` makes `_build/host/shell` by caching the `shells.host` `nix-shell` environment and runs
`cabal --project-file=host.project --builddir=_build/ios/dist new-build all` in that environment. `host` is also the default Make target. You can instruct
cabal to only build some target(s) by passing `cabal_target=...` to Make.
* `make ios` and `make android` do the same for iOS and Android respectively.
* `make all` is equivalent to `make host ios android` in the unlikely circumstance your machine is capable of building all platforms.
* `make clean` removes the `_build` directory where all the intermediate build products go.

### Cross-platform

Reflex Native provides a cross-platform view building abstraction which allows components to be written once and operate identically across the supported
platforms. This abstraction is intentionally conservative; any functionality which can't be equally supported should not be in the cross-platform abstraction.

Any substantial app requires some amount of platform-specific behavior, such as varying navigation, platform-specific functionality or libraries, or
specializations to fit the platform's native look and feel. To that end, Reflex Native is intended to provide the cross-platform tools to write components that
work everywhere but the overall app is intended to be platform-specific and reuse the cross-platform components.

Using a combination of platform-specific code, cross-platform code, and Haskell's excellent features for reuse you can assemble an app with maximum code sharing
among platforms while avoiding the uncanny valley of cross-platform apps; views created either using the platform-specific `reflex-native-*` or cross-platform
`reflex-native` packages create and maintain actual platform views and not simulacrums, and the platform-specific code you write can complete the product.

### Project status

This project is in its very early stages and is probably not suitable for building a production application on immediately. In particular:

- It has not been thoroughly tested, nor been used in a production application yet.
- Android support is still missing.
- Cross-platform layout support is still TODO.

It is being open sourced early in order to foster community involvement or in the hopes that it will be useful, and is still under active development. If you're
interested in building an application using it, please [contribute](CONTRIBUTING.md)! We're actively soliciting volunteers to work on it and make it better.

As it's in active development this README talks about intended features as if they exist, most notably Android support.

