SHELL := /bin/bash

platforms = host ios android
cabal_files = $(shell find . -type f -a -name '*.cabal' | grep -v '^[.]/_build' | grep -v '^[.]/[.]')
nix_files = default.nix $(shell find . -type f -a -name default.nix | grep -v '^[.]/_build' | grep -v '^[.]/[.]')

# this hack is here to work around a shortcoming with cabal new-build where error and warning messages get output with paths that are relative to the package
# being built, not the project root, and so vim (or similar) which try to parse those messages to allow quick navigation to the source line get bamboozled.
# it's worse because at this position outside of cabal new-build it's hard to tell which package is currently being build, so we use heuristics by knowing
# which modules hierarchies are in which packages. tl;dr:  :'(
canonicalize_error_paths = sed \
  -e 's,^test/,reflex-native-test/test/,g' \
  -e 's,^src/Reflex/Native/Android,reflex-native-android/src/Reflex/Native/Android,g' \
  -e 's,^src/Reflex/Native/Examples/Draggy,examples/draggy/src/Reflex/Native/Examples/Draggy,g' \
  -e 's,^src/Reflex/Native/Test,reflex-native-test/src/Reflex/Native/Test,g' \
  -e 's,^src/Reflex/Native,reflex-native/src/Reflex/Native,g' \
  -e 's,^src/Reflex/UIKit,reflex-native-uikit/src/Reflex/UIKit,g'

.PHONY: all clean $(platforms)

$(platforms): %: _build/%/shell
	set -eo pipefail ; _build/$*/shell cabal --project-file=$*.project --builddir=_build/$*/dist new-build all 2>&1 | $(canonicalize_error_paths)
	set -eo pipefail ; _build/$*/shell cabal --project-file=$*.project --builddir=_build/$*/dist new-test all 2>&1 | $(canonicalize_error_paths)

clean:
	rm -rf _build

all: $(platforms)

_build/%/shell: $(nix_files) $(cabal_files)
	mkdir -p $(dir $@)
	mkdir -p _build/$*/nix-root
	rm -f $@
	nix-shell --pure --add-root _build/$*/nix-root/nix-gc-root --indirect -A shells.$* --run 'set | grep -v -E "^(BASH_[^=]*|BASHOPTS|EUID|GROUPS|PPID|SHELLOPTS|UID)="' > $@
	echo 'runHook shellHook' >> $@
	echo '"$$@"' >> $@
	echo 'exit $$?' >> $@
	chmod +x $@
