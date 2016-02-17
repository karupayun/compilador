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
	tigerflow.uo tigerliveness.uo tigerspill.uo tigercolor.uo

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
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerpp.uo: tigersres.uo tigertab.ui tigertips.uo tigerabs.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigertree.uo: tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigercolor.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigerassem.uo: tigertemp.ui 
tigercodegen.ui: tigertree.uo tigerassem.uo 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigergraph.uo: tigergraph.ui tigertab.ui tigertemp.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigertrans.ui tigerpp.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigertemp.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerspill.uo: tigerspill.ui tigerframe.ui tigerassem.uo tigertemp.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigermuestratipos.ui: tigertips.uo 
tigerspill.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigercolor.ui tigercanon.ui tigerassem.uo tigerinterp.uo \
    tigerlex.uo tigertrans.ui tigerpp.uo 
tigercolor.uo: tigercolor.ui tigergraph.ui tigerframe.ui tigertemp.ui \
    tigerspill.ui tigerliveness.ui tigerflow.ui 
tigertemp.uo: tigertemp.ui 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigertab.uo: tigertab.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigerliveness.uo: tigerliveness.ui tigergraph.ui tigertemp.ui tigerflow.ui 
tigerpila.uo: tigerpila.ui 
tigerescap.ui: tigerabs.uo 
tigerflow.ui: tigergraph.ui tigerassem.uo tigertemp.ui 
tigerflow.uo: tigerflow.ui tigertab.ui tigergraph.ui tigerassem.uo \
    tigertemp.ui 
tigerseman.ui: tigerabs.uo 
tigergrm.ui: tigerabs.uo 
