platforms = host uikit android
cabal_target ?= all
cabal_files = $(shell find . -type f -a -name '*.cabal' | grep -v '^[.]/_build' | grep -v '^[.]/[.]')
nix_files = default.nix $(shell find . -type f -a -name default.nix | grep -v '^[.]/_build' | grep -v '^[.]/[.]')

.PHONY: all clean $(platforms)

$(platforms): %: _build/%/shell
	_build/$*/shell cabal --project-file=$*.project --builddir=_build/$*/dist new-build $(cabal_target)

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
	chmod +x $@
