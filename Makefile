#; ---------------------------------------------------------------------- ;#
#; FICHIER               : Makefile                                       ;#
#; AUTEUR                : Dominique Boucher                              ;#
#; DATE DE CREATION      : Thu May 25 10:56:01 1995                       ;#
#; DERNIERE MODIFICATION : Thu May 25 15:11:53 1995                       ;#
#; ---------------------------------------------------------------------- ;#
#; Makefile for IDyl. Can be used to compile IDyl with Bigloo or          ;#
#; Gambit-C.                                                              ;#
#; ---------------------------------------------------------------------- ;#

#; --- The name of the executable --------------------------------------- ;#
A.OUT		= idyl

#; --- The location of the source code ---------------------------------- ;#
SCMCONFIGDIR	= ${shell pwd}

#; --- Where the executable must be installed --------------------------- ;#
INSTALL_DIR	= ~/bin

#; --- Where the libraries must be installed ---------------------------- ;#
LIB_DIR		= ~/lib/idyl

#; --- The install program ---------------------------------------------- ;#
INSTALL	 	= install


#; ---------------------------------------------------------------------- ;#
#; The compilers ...                                                      ;#
#; ---------------------------------------------------------------------- ;#
CC		= gcc
CFLAGS		= -D___SINGLE_HOST -I/usr/local/Gambit-C/include

GAMBIT		= gsc
CGAMBITFLAGS	= -c
LGAMBITFLAGS	= -I/usr/local/Gambit-C/include -L/usr/local/Gambit-C/lib -lgambc -lm -ldl -lutil -lX11

BIGLOO		= /home/saguenay3/serrano/house/bin/alpha/bigloo
MKAFILE		= /home/saguenay3/serrano/house/bin/alpha/afile
AFILE		= .afile

CBIGLOOFLAGS	= -v -c -afile $(AFILE) -O3 -unsafe -farithmetic
LBIGLOOFLAGS	= -v -farithmetic -unsafe -O3 -lX11

#; ---------------------------------------------------------------------- ;#
#; The shell ...                                                          ;#
#; ---------------------------------------------------------------------- ;#
SHELL		= sh

#; ---------------------------------------------------------------------- ;#
#;              ***** DO NOT EDIT BELOW THIS LINE *****                   ;#
#; ---------------------------------------------------------------------- ;#

#; ---------------------------------------------------------------------- ;#
#; Source files ...                                                       ;#
#; ---------------------------------------------------------------------- ;#
SOURCE		= lr-dvr.scm lexer.scm parser.scm cleanup.scm         \
                  class.scm env.scm generic.scm gen.scm               \
		  error.scm global.scm util.scm x.scm repl.scm

GAMBITSOURCE	= structs.scm
BIGLOOSOURCE	=
BIGLOOINCLUDE	= structs.scm prologue.bigloo

LIBRARIES	= class.dylan coll.dylan error.dylan fun.dylan \
		  numbers.dylan x11.dylan
#; ---------------------------------------------------------------------- ;#
#; The default compiler ...                                               ;#
#; ---------------------------------------------------------------------- ;#
DEFAULT_COMPILER= bigloo

#; ---------------------------------------------------------------------- ;#
#; The compiler ...                                                       ;#
#; ---------------------------------------------------------------------- ;#
COMPILER	= `if [ -f .compiler ] ; then               \
                     cat .compiler;                         \
                  else                                      \
		     $(MAKE) $(DEFAULT_COMPILER);           \
		  fi`

#; ---------------------------------------------------------------------- ;#
#; The compiler-specific sources ...                                      ;#
#; ---------------------------------------------------------------------- ;#
SOURCE_GSC	= $(GAMBITSOURCE:%.scm=gambit-src/%.saux)   \
                  $(SOURCE:%.scm=gambit-src/%.saux)
O_GSC		= $(SOURCE_GSC:%.saux=%.o) gambit-src/x11.o 
C_GSC		= $(SOURCE_GSC:%.saux=%.c)

BIGLOO_SAUX	= $(BIGLOOSOURCE:%.scm=%.saux) $(SOURCE:%.scm=%.saux)
SOURCE_BGL	= $(BIGLOO_SAUX:%=bigloo-src/%)
O_BGL		= $(SOURCE_BGL:%.saux=%.o) bigloo-src/x11.o

#; ---------------------------------------------------------------------- ;#
#; No implicit rule for .c.o                                              ;#
#; ---------------------------------------------------------------------- ;#
%.o : %.c

#; ---------------------------------------------------------------------- ;#
#; The suffixes ...                                                       ;#
#; ---------------------------------------------------------------------- ;#
.SUFFIXES:
.SUFFIXES: .saux .c .s .S .scm .sch .gsc .bgl .o .com

#; ---------------------------------------------------------------------- ;#
#; all:                                                                   ;#
#; ---------------------------------------------------------------------- ;#
all:	
	@ case  $(COMPILER) in					\
		gambit)	$(MAKE) mkdir; $(MAKE) all.gambit;;	\
		bigloo)	$(MAKE) mkdir; $(MAKE) all.bigloo;;	\
		*) : ;; esac

gambit: 
	@ echo gambit > .compiler
	$(MAKE) COMPILER=gambit all

bigloo: 
	@ echo bigloo > .compiler
	$(MAKE) COMPILER=bigloo all

all.gambit: $(SOURCE_GSC)
	@ $(MAKE) all.gambit.o

all.gambit.o: $(O_GSC)
	@ echo gambit linking:
	@ $(GAMBIT) -link $(C_GSC)
	$(CC) $(O_GSC) $(subst .c,_.c,$(word $(words $(C_GSC)), $(C_GSC))) -o $(A.OUT).gambit $(LGAMBITFLAGS)

all.bigloo: $(SOURCE_BGL)
	@ $(MAKE) all.bigloo.o

all.bigloo.o: $(O_BGL) 
	@ $(BIGLOO) -o $(A.OUT).bigloo $(LBIGLOOFLAGS) $(O_BGL)

.afile: $(SOURCE_BGL) 
	@ (cd bigloo-src; $(MKAFILE) -o $(AFILE) $(BIGLOO_SAUX))

#; ---------------------------------------------------------------------- ;#
#; configure:                                                             ;#
#; ---------------------------------------------------------------------- ;#
config:
	@echo "** Configuring IDyl ..."
	@rm -f global.scm
	@sed "s=LIB_DIR=$(LIB_DIR)=" global.skel > global.scm

#; ---------------------------------------------------------------------- ;#
#; install:                                                               ;#
#; ---------------------------------------------------------------------- ;#
install: 
	@ echo "** Moving $(A.OUT) to $(INSTALL_DIR)"
	@ case  $(COMPILER) in					\
		gambit)	cp $(A.OUT).gambit $(A.OUT)	;;	\
		bigloo)	cp $(A.OUT).bigloo $(A.OUT)	;;	\
		*) : ;; esac
	@ $(INSTALL) -m 755 $(A.OUT) $(INSTALL_DIR) > /dev/null
	@ rm -f $(A.OUT)
	@echo "** Installing libraries ..."
	@[ -d $(LIB_DIR) ] || mkdir $(LIB_DIR)
	@for p in $(LIBRARIES); 				\
	 do 							\
		($(INSTALL) -m 644 libraries/$$p $(LIB_DIR) > /dev/null)	\
	 done;
	@rm -f $(LIB_DIR)/startup.dylan
	@sed "s=LIB_DIR=$(LIB_DIR)=g" libraries/startup.dylan > $(LIB_DIR)/startup.dylan
	@chmod 644 $(LIB_DIR)/startup.dylan
	@echo "** Installation completed"


#; ---------------------------------------------------------------------- ;#
#; mkdir:                                                                 ;#
#; ---------------------------------------------------------------------- ;#
mkdir:
	@ [ -d $(COMPILER)-src ] || mkdir $(COMPILER)-src
	@ case $(COMPILER) in					\
		bigloo)	for p in $(BIGLOOINCLUDE);              \
                         do 					\
                           (cd bigloo-src; ln -fs ../$$p $$p)   \
                         done;                                	\
			$(MAKE) .afile;;                        \
		*) : ;; esac

#; ---------------------------------------------------------------------- ;#
#; clean:                                                                 ;#
#; ---------------------------------------------------------------------- ;#
clean:
	@ $(MAKE) clean.$(COMPILER)

clean.all:
	@- $(MAKE) clean.gambit
	@- $(MAKE) clean.bigloo

clean.gambit:
	@- echo "**  Cleaning [gambit]"
	@- \rm -rf gambit-src
	@- \rm -f $(A.OUT).gambit

clean.bigloo:
	@- echo "**  Cleaning [bigloo]"
	@- \rm -rf bigloo-src
	@- \rm -f $(A.OUT).bigloo

#; ---------------------------------------------------------------------- ;#
#; .scm -> .saux                                                          ;#
#; ---------------------------------------------------------------------- ;#
gambit-src/x.saux: x.scm x.gsc
	@ rm -f gambit-src/x.saux
	@ cat prologue.gambit x.gsc x.scm > gambit-src/x.saux

bigloo-src/x11.o: x11.c x11.h
	@ (cd bigloo-src/ ; ln -sf ../x11.c . ; \
	  $(CC) $(CFLAGS) -c x11.c)

gambit-src/x11.o: x11.c x11.c
	@ (cd gambit-src/ ; ln -sf ../x11.c . ; \
	  $(CC) $(CFLAGS) -c x11.c)

gambit-src/structs.saux: structs.scm
	@ rm -f gambit-src/structs.saux
	@ cat prologue.gambit structs.gsc structs.scm > gambit-src/structs.saux

%.saux: ../%.scm $(SCMCONFIGDIR)/prologue.gambit $(SCMCONFIGDIR)/prologue.bigloo
	@ case $(COMPILER) in					           \
	     gambit)  echo "gambit -> $*.saux";                            \
                      if [ -f $(SCMCONFIGDIR)/prologue.gambit ] ; then     \
	                 cat $(SCMCONFIGDIR)/prologue.gambit $< > $*.saux; \
	        	      else                                         \
			 echo "*** WARNING: cant find prologue.gambit";    \
	                 cp $< $*.saux;                                    \
                      fi;                                                  \
                      \rm -f $*.c $*.o;;                                   \
	     bigloo)  echo "bigloo -> $*.saux";                            \
                      if [ -f $(SCMCONFIGDIR)/prologue.bigloo ] ; then     \
	                    cat $(notdir $*).bgl                           \
                            $<                                             \
                            > $*.saux;                                     \
                        else                                               \
			    echo "*** WARNING: cant find prologue.bigloo"; \
	                    cat $(notdir $*).bgl $< > $*.saux;             \
                      fi;                                                  \
                      \rm -f $*.c $*.o;;                                   \
             *) : ;; esac

#; ---------------------------------------------------------------------- ;#
#; .saux -> .o                                                            ;#
#; ---------------------------------------------------------------------- ;#
.saux.o:
	@ case  $(COMPILER) in					       \
		gambit)	(cd gambit-src;                                \
			echo $*.saux:;				       \
			ln -sf $(notdir $*).saux $(notdir $*).scm;      \
			$(GAMBIT) $(CGAMBITFLAGS) $(notdir $*).scm ||  \
                             (\rm -f $(notdir $*).scm; exit -3);       \
                        echo $*.c:;			               \
                        $(CC) $(CFLAGS) -c $(notdir $*).c -o $(notdir $*).o);; \
		bigloo)	(cd bigloo-src;                                \
                         $(BIGLOO) $(CBIGLOOFLAGS) -c $(notdir $*).saux\
                                  -o $(notdir $*).o);;                 \
		*) : ;; esac

#; --- end of Makefile -------------------------------------------------- ;#
