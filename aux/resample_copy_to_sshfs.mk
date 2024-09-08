#!/usr/bin/gmake -f
# Resample FLAC to 48000 and copy to SSHFS (c) 2024 Ma_Sys.ma <info@masysma.net>

PREFIX = /data/programs/music2
SRCA   = $(PREFIX)/album
SRCF   = $(PREFIX)/single $(PREFIX)/epic $(PREFIX)/track
SSHFS  = u0_162@192.168.1.102:/storage/3BD7-948E/masysma/music2
MOUNTP = /home/linux-fan/pmnt/sshfs

TARGET_RATE = 48000
TARGET_FIND = 24 bit, stereo, 48 kHz

TMPSTRUCT      := /tmp/resample_copy_to_sshfs
IDENT          := $(TMPSTRUCT)/identification.mk
VALS           := $(TMPSTRUCT)/vals
SPCSRC          = $(shell find $(SRCA) $(SRCF) -name "* *")
SPCDST          = $(shell find $(MOUNTP) -name "* *")
TESTD           = $(MOUNTP)/$(notdir $(firstword $(SRCA) $(SRCF)))
SRC_FLACFILES   = $(shell find $(SRCF) -type f -name "*.flac")
SRC_AFLACFILES  = $(shell find $(SRCA) -type f -name "*.flac")
SRC_OGGFILES    = $(shell find $(SRCF) -type f -name "*.ogg")
SRC_MP3FILES    = $(shell find $(SRCF) -type f -name "*.mp3")
DST_FLACFILES   = $(SRC_FLACFILES:$(PREFIX)/%=$(MOUNTP)/%)
DST_AFLACFILES  = $(SRC_AFLACFILES:$(PREFIX)/%=$(MOUNTP)/%)
DST_OGGFILES    = $(SRC_OGGFILES:$(PREFIX)/%=$(MOUNTP)/%)
DST_MP3FILES    = $(SRC_MP3FILES:$(PREFIX)/%=$(MOUNTP)/%)
DST_FILEFILES   = $(DST_FLACFILES) $(DST_OGGFILES) $(DST_MP3FILES)
DST_HAVEFILES   = $(shell find $(MOUNTP) -name "*.flac" -or -name "*.ogg" -or \
								-name "*.mp3")
DST_ALBUMS      = $(SRC_ALBUMS:$(PREFIX)/%=$(MOUNTP)/%)
DST_DELFILES_P  = $(filter-out $(DST_AFLACFILES) $(DST_FILEFILES),$(DST_HAVEFILES))

-include $(IDENT)

all: $(IDENT) $(DST_ALLFILES) $(DST_ALBUMS)
	$(if $(DST_DELFILES),-rm $(DST_DELFILES),:)
	-fusermount -u $(MOUNTP)
	-rm -r $(TMPSTRUCT)

# source directory must not contain file names with spaces!
# establish that the target directory is mounted
# destination directory must not contain file names with spaces!
.PHONY: precondition
precondition:
	[ -d "$(TMPSTRUCT)" ] || mkdir "$(TMPSTRUCT)"
	[ -z "$(SPCSRC)"    ]
	[ -d "$(TESTD)"     ] || sshfs "$(SSHFS)" "$(MOUNTP)"
	[ -z "$(SPCDST)"    ]

# assemble list of albums (slow but independent of list length)
# seems to be most practical way to achieve newline-separated results...
$(VALS):
	$(foreach I,$(SRC_AFLACFILES),$(file >>$(VALS),SRC_ALBUMS += $(dir $(I))))

# create identity file conditionally because otherwise we take a loop and this
# operation is not idempotent hence we don't want to to the looping
$(IDENT): precondition $(VALS)
	$(if $(DST_ALLFILES),,$(file >$(IDENT),DST_DELFILES ?= $(DST_DELFILES_P)))
	$(if $(DST_ALLFILES),:,sort -u < $(VALS) >> $(IDENT))
	$(if $(DST_ALLFILES),,$(file >>$(IDENT),DST_ALLFILES = $$(DST_FILEFILES) $$(DST_ALBUMS)))
	$(if $(DST_ALLFILES),,$(file >>$(IDENT),DST_AFLACFILES := ))

# -- convert albums --
# albums are always handled as one unit as to allow for replaygain to work!
# we define individual rules for the subprocesses, allowing to take full
# advantage of parallelization when using -j4 or such.
define CONVALBUM
$(1:$(PREFIX)/%=$(TMPSTRUCT)/%)/%.flac: $(1)/%.flac
	mkdir -p $$(dir $$@)
	$$(if $$(findstring $(TARGET_FIND),$$(shell file $$<)),cp \
		-v $$< $$@,ReSampler -i $$< -o $$@ -r $(TARGET_RATE) -b 24)

.INTERMEDIATE: $(patsubst $(PREFIX)/%,$(TMPSTRUCT)/%,$(wildcard $(1)/*.flac))

$(1:$(PREFIX)/%=$(MOUNTP)/%): \
		$(patsubst $(PREFIX)/%,$(TMPSTRUCT)/%,$(wildcard $(1)/*.flac))
	mkdir -p $$@
	loudgain -q -a -k -s e $(1:$(PREFIX)/%=$(TMPSTRUCT)/%)/*.flac
	mv $(1:$(PREFIX)/%=$(TMPSTRUCT)/%)/*.flac $$@

endef

# -- convert individual files --
define CONVFLAC
$(1:$(PREFIX)/%=$(MOUNTP)/%): $(1)
	mkdir -p $$(dir $$@) $$(dir $(1:$(PREFIX)/%=$(TMPSTRUCT)/%))
	$$(if $$(findstring $(TARGET_FIND),$$(shell file $$<)),cp \
		-v $$< $(1:$(PREFIX)/%=$(TMPSTRUCT)/%),ReSampler \
		-i $$< -o $(1:$(PREFIX)/%=$(TMPSTRUCT)/%) \
		-r $(TARGET_RATE) -b 24)
	loudgain -q -r -k -s e $(1:$(PREFIX)/%=$(TMPSTRUCT)/%)
	mv $(1:$(PREFIX)/%=$(TMPSTRUCT)/%) $$@

endef

define CONVCPY
$(1:$(PREFIX)/%=$(MOUNTP)/%): $(1)
	mkdir -p $$(dir $$@)
	cp $$< $$@

endef

$(eval $(foreach SRC,$(SRC_ALBUMS),$(call CONVALBUM,$(SRC))))
$(eval $(foreach SRC,$(SRC_FLACFILES),$(call CONVFLAC,$(SRC))))
$(eval $(foreach SRC,$(SRC_OGGFILES),$(call CONVCPY,$(SRC))))
$(eval $(foreach SRC,$(SRC_MP3FILES),$(call CONVCPY,$(SRC))))
