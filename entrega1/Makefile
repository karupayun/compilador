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
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigertemp.uo topsort.uo \
	tigertips.uo tigerseman.uo tigertopsort.uo tigermuestratipos.uo tigerpp.uo

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

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigermuestratipos.ui: tigertips.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigergrm.ui: tigerabs.uo 
tigertemp.uo: tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigertopsort.ui \
    tigerabs.uo tigerpp.uo 
tigerpp.uo: tigersres.uo tigertab.ui tigertips.uo tigerabs.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo \
    tigermuestratipos.ui 
tigerescap.ui: tigerabs.uo 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigerlex.uo \
    tigerpp.uo 
tigertab.uo: tigertab.ui 
