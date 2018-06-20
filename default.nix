rec {
  # Function which extends a haskellPackages with the packages local to this repository using haskellPackages.callPackage. Used later to make augmented
  # platform-specific package sets, but also useful for integrating Reflex Native into your Nix build environment.
  packages = haskellPackages: {
    hs-uikit = haskellPackages.callPackage ./hs-uikit {};
    reflex-native = haskellPackages.callPackage ./reflex-native {};
    reflex-native-draggy = haskellPackages.callPackage ./examples/draggy {};
    reflex-native-uikit = haskellPackages.callPackage ./reflex-native-uikit {};
  };

  # Version of reflex-platform we use for iteration on Reflex Native and compiling the examples
  reflex-platform-src = (import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./reflex-platform-version.json));

  # reflex-platform for iteration on Reflex Native and compiling the examples
  reflex-platform = import reflex-platform-src {};

  # What overrides we make to a haskellPackages such as adding the local packages.
  overrides = self: super: packages self;

  # Alias to the iOS cross-building nixpkgs from reflex-platform. Useful when nix REPLing.
  iosArm64 = reflex-platform.nixpkgsCross.ios.arm64.pkgs;

  # haskellPackages for iOS extended with our local overrides. Useful when nix REPLing.
  ghcIosArm64 = reflex-platform.ghcIosArm64.override { inherit overrides; };

  # Shell environments for the various platforms
  shells = {
    # Shell environment for working on the UIKit side with the UIKit related packages, common packages, and any special environmental magics to get iOS cross
    # building working in a shell
    uikit = (reflex-platform.workOnMulti' {
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
        bundleIdentifier = "org.reflexfrp.todomvc";
        bundleName = "Reflex Native Draggy";
      };
    };
  };
}

