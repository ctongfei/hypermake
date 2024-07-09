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

.PHONY: all clean install
