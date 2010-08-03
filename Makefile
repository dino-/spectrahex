SRCDIR=src
BINDIR=bin
LIBDIR=lib
JARFILE=colorjump.jar

COMPILER=scalac
CLASSPATH='$(SRCDIR):$(BINDIR)'
DEBUG_FLAG=
#DEBUG_FLAG=-g
COMPILER_FLAGS=-classpath $(CLASSPATH) -make:changed $(DEBUG_FLAG) -sourcepath $(SRCDIR) -d $(BINDIR)

VPATH=$(SRCDIR):$(BINDIR):$(LIBDIR)


jar: $(JARFILE);

bytecodes:
	find $(SRCDIR) -name '*.scala' | xargs $(COMPILER) $(COMPILER_FLAGS)

$(JARFILE): bytecodes
	cd $(BINDIR); \
	find . -name '*.class' | xargs jar -cf ../$(LIBDIR)/$(JARFILE) ; \
	cd ..

clean:
	rm -rfv bin/* ; \
	rm -v $(LIBDIR)/$(JARFILE)


.PHONY: bytecodes clean
