# Unix makefile for tigermain example

LUGAR = Mariano

ifeq ($(LUGAR),Facultad)
# Facultad
HOME=/home/alumno

MOSMLHOME=${HOME}/mosml
MOSMLBIN=$(MOSMLHOME)/bin
MOSMLTOOLS=$(MOSMLBIN)/camlrunm $(MOSMLHOME)/tools
MOSMLLEX=$(MOSMLBIN)/mosmllex
MOSMLYACC=$(MOSMLBIN)/mosmlyac -v
MOSMLC=$(MOSMLBIN)/mosmlc -c -liberal
MOSMLL=$(MOSMLBIN)/mosmlc
endif

ifeq ($(LUGAR),DCC)
# DCC
HOME=/usr/local

MOSMLHOME=${HOME}/lib/mosml
MOSMLBIN=${HOME}/bin
MOSMLTOOLS=$(MOSMLBIN)/camlrunm $(HOME)/share/mosml/tools
MOSMLLEX=$(MOSMLBIN)/mosmllex
MOSMLYACC=$(MOSMLBIN)/mosmlyac -v
MOSMLC=$(MOSMLBIN)/mosmlc -c -liberal
MOSMLL=$(MOSMLBIN)/mosmlc
endif

ifeq ($(LUGAR),Mariano)
# Mariano
MOSMLTOOLS=camlrunm /usr/share/mosml/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v
MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc
endif

ifeq ($(LUGAR),Margarita)
#Margarita
HOME=/home/margy/Escritorio/Compiladores
MOSMLHOME=${HOME}/mosml
MOSMLBIN=$(MOSMLHOME)/bin
MOSMLTOOLS=$(MOSMLBIN)/camlrunm $(MOSMLHOME)/tools
MOSMLLEX=$(MOSMLBIN)/mosmllex
MOSMLYACC=$(MOSMLBIN)/mosmlyac -v
MOSMLC=$(MOSMLBIN)/mosmlc -c -liberal
MOSMLL=$(MOSMLBIN)/mosmlc
endif

GCC=gcc
CFLAGS= -g


# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigertemp.uo \
	tigertips.uo tigerseman.uo tigertopsort.uo tigermuestratipos.uo tigerpp.uo \
	tigertemp.uo tigertree.uo tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo \
        tigercanon.uo tigerinterp.uo tigerassem.uo tigercodegen.uo tigergraph.uo \
	tigerflow.uo tigerliveness.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o
	$(REMOVE) tiger

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml tigerinterp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerit.uo: tigertree.uo tigertab.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerpp.uo: tigersres.uo tigertab.ui tigertips.uo tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigercodegen.ui: tigertree.uo tigerassem.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigertrans.ui tigerpp.uo 
tigerassem.uo: tigertemp.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigerflow.uo: tigerflow.ui tigertab.ui tigergraph.ui tigerassem.uo \
    tigertemp.ui 
tigerescap.ui: tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigergraph.uo: tigergraph.ui tigertab.ui tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertree.uo: tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigertab.uo: tigertab.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigercanon.ui tigerassem.uo tigerinterp.uo tigertemp.ui \
    tigerlex.uo tigertrans.ui tigerpp.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigerliveness.uo: tigerliveness.ui tigergraph.ui tigertemp.ui tigerflow.ui 
tigerliveness.ui: tigerflow.ui 
tigertemp.uo: tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigertemp.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigermuestratipos.ui: tigertips.uo 
