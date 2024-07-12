SHELL = /bin/bash
SBT = sbt
PREFIX = $(HOME)/.local
SCALA_VERSION = 2.13
HYPERMAKE_VERSION = 0.1.0
JAR = target/scala-$(SCALA_VERSION)/hypermake-assembly-${HYPERMAKE_VERSION}.jar

all: $(JAR)

$(JAR):
	$(SBT) assembly

clean:
	$(SBT) clean

install: $(JAR)
	@echo "Installing hypermake to $(PREFIX)"
	@mkdir -p $(PREFIX)/lib/hypermake
	cp $(JAR) $(PREFIX)/lib/hypermake/hypermake.jar
	@mkdir -p $(PREFIX)/share/hypermake
	cp -r src/main/hypermake/* $(PREFIX)/share/hypermake
	@mkdir -p $(PREFIX)/bin
	cp bin/hypermake $(PREFIX)/bin/hypermake
	chmod +x $(PREFIX)/bin/hypermake

build-docs:
	mdbook build

# Credit to https://github.com/kg4zow/mdbook-template/blob/main/Makefile
gh-deploy: build-docs
	set -ex ; \
	WORK="$$( mktemp -d )" ; \
	VER="$$( git describe --always --tags --dirty )" ; \
	git worktree add --force "$$WORK" gh-pages ; \
	rm -rf "$$WORK"/* ; \
	rsync -av book/ "$$WORK"/ ; \
	if [ -f CNAME ] ; then cp CNAME "$$WORK"/ ; fi ; \
	pushd "$$WORK" ; \
	git add -A ; \
	git commit -m "Updated gh-pages $$VER" ; \
	popd ; \
	git worktree remove "$$WORK" ; \
	git push origin gh-pages

.PHONY: all clean install build-docs gh-deploy
