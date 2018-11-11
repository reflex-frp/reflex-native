SHELL := /bin/bash

platforms = host ios android
bash = $(shell nix-instantiate --eval -E '(import <nixpkgs> {}).bash + /bin/bash')
cabal_files = $(shell find . -type f -a -name '*.cabal' | grep -v '^[.]/_build' | grep -v '^[.]/[.]')
nix_files = default.nix $(shell find . -type f -a -name default.nix | grep -v '^[.]/_build' | grep -v '^[.]/[.]')

# this sed hackery is here to work around a shortcoming with cabal new-build where error and warning messages get output with paths that are relative to the
# package being built, not the project root, and so vim (or similar) which try to parse those messages to allow quick navigation to the source line get
# bamboozled.

.PHONY: all clean $(platforms)

host: _build/host/shell host.project
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build hs-uikit 2>&1 | sed -e 's,^src/,hs-uikit/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build reflex-native 2>&1 | sed -e 's,^src/,reflex-native/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build reflex-native-test 2>&1 | sed -e 's,^src/,reflex-native-test/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build reflex-native-draggy 2>&1 | sed -e 's,^src/,examples/draggy/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-test reflex-native-test 2>&1 | sed -e 's,^test/,reflex-native-test/test/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-test reflex-native-draggy 2>&1 | sed -e 's,^test/,examples/draggy/test/,g'

ios: _build/ios/shell ios.project
	set -eo pipefail ; env -i $(bash) _build/ios/shell cabal --project-file=ios.project --builddir=_build/ios/dist new-build hs-uikit 2>&1 | sed -e 's,^src/,hs-uikit/src/,g'
	set -eo pipefail ; env -i $(bash) _build/ios/shell cabal --project-file=ios.project --builddir=_build/ios/dist new-build reflex-native 2>&1 | sed -e 's,^src/,reflex-native/src/,g'
	set -eo pipefail ; env -i $(bash) _build/ios/shell cabal --project-file=ios.project --builddir=_build/ios/dist new-build reflex-native-draggy 2>&1 | sed -e 's,^src/,examples/draggy/src/,g'

clean:
	rm -rf _build

all: $(platforms)

_build/%/shell: $(nix_files) $(cabal_files)
	mkdir -p $(dir $@)
	mkdir -p _build/$*/nix-root
	rm -f $@
	nix-shell --pure --add-root _build/$*/nix-root/nix-gc-root --indirect -A shells.$* --run 'declare -p | grep -v -E "^declare( -[^ ]* )?(BASH_[^=]*|BASHOPTS|BASHPID|EUID|GROUPS|PPID|SHELLOPTS|UID)="' > $@
	echo 'runHook shellHook' >> $@
	echo '"$$@"' >> $@
	echo 'exit $$?' >> $@
	chmod +x $@
