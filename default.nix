rec {
  # Functions which extend a haskellPackages with the packages local to this repository and appropriate for the given platform using
  # haskellPackages.callPackage. Used later to make augmented platform-specific package sets, but also useful for integrating Reflex Native into your Nix
  # build environment.
  packages = {
    common = haskellPackages: {
      reflex-native = haskellPackages.callPackage ./reflex-native {};
      reflex-native-draggy = haskellPackages.callPackage ./examples/draggy {};
    };

    host = packages.common;

    android = haskellPackages: packages.common haskellPackages // {
    };

    ios = haskellPackages: packages.common haskellPackages // {
      hs-uikit = haskellPackages.callPackage ./hs-uikit {};
      reflex-native-uikit = haskellPackages.callPackage ./reflex-native-uikit {};
    };
  };

  # Version of reflex-platform we use for iteration on Reflex Native and compiling the examples
  reflex-platform-src = (import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./reflex-platform-version.json));

  # reflex-platform for iteration on Reflex Native and compiling the examples
  reflex-platform = import reflex-platform-src {};

  # Host nixpkgs from reflex-platform
  nixpkgs = reflex-platform.nixpkgs;

  # Alias to the iOS cross-building nixpkgs from reflex-platform. Useful when nix REPLing.
  iosArm64 = reflex-platform.nixpkgsCross.ios.arm64.pkgs;

  # What overrides we make to a haskellPackages for each platform, both external dependencies that we adjust and local packages.
  overrides = {
    common = self: super: {
      rank2classes = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "rank2classes" (nixpkgs.fetchFromGitHub {
        owner = "Dridus";
        repo = "grampa";
        rev = "389ac8ead37fa44e5e746e6e7817da3a3f69222c";
        sha256 = "1ssv0lrbbj694rficrka56l628ha9l61wrnxqxy6yn9dawk6h6n8";
      } + /rank2classes) {});
    };

    host = nixpkgs.lib.composeExtensions overrides.common (self: super: packages.common self);

    android = nixpkgs.lib.composeExtensions overrides.common (self: self: packages.android self);

    ios = nixpkgs.lib.composeExtensions overrides.common (self: self: packages.ios self);
  };

  # haskellPackages for the host extended with our local overrides.
  ghcHost = reflex-platform.ghc8_2_1.override { overrides = overrides.host; };

  # haskellPackages for Android extended with our local overrides.
  ghcAndroidArm64 = reflex-platform.ghcAndroidArm64.override { overrides = overrides.android; };

  # haskellPackages for iOS extended with our local overrides.
  ghcIosArm64 = reflex-platform.ghcIosArm64.override { overrides = overrides.ios; };

  # Shell environments for the various platforms
  shells = {
    # Shell environment for working on the cross-platform bits only, notably the test framework.
    host = (reflex-platform.workOnMulti' {
      env = ghcHost;
      packageNames = ["reflex-native" "reflex-native-draggy"];
    });

    # Shell environment for working on the Android side with Android related packages and common packages.
    android = (reflex-platform.workOnMulti' {
      env = ghcAndroidArm64;
      packageNames = ["reflex-native" "reflex-native-draggy"];
    });

    # Shell environment for working on the iOS side with the UIKit related packages, common packages, and any special environmental magics to get iOS cross
    # building working in a shell
    ios = (reflex-platform.workOnMulti' {
      env = ghcIosArm64;
      packageNames = ["hs-uikit" "reflex-native" "reflex-native-draggy" "reflex-native-uikit"];

      # special magics to get the preConfigureHook which adds the framework search paths for iOS frameworks
      # ideally this would not be necessary, and it isn't if haskellPackages generic-builder is doing the work, but since we're running cabal manually it's
      # needed
      tools = env: [ iosArm64.buildPackages.osx_sdk ];
    }).overrideAttrs (_: { shellHook = "runHook preConfigureHooks"; });
  };

  # Derivations for building each of the examples, grouped by the target platform
  examples = {
    # Derivations for building iOS app examples
    ios = {
      # Derivation for building the reflex-native-draggy example as a packaged iOS app.
      draggy = (reflex-platform.iosWithHaskellPackages ghcIosArm64).buildApp {
        package = p: p.reflex-native-draggy;
        executableName = "reflex-native-draggy-uikit";
        bundleIdentifier = "org.reflexfrp.reflex-native-draggy";
        bundleName = "Reflex Native Draggy";
      };
    };
  };
}

