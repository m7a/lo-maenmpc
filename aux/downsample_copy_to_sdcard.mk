#!/usr/bin/gmake -f
# Downsample OGG and FLAC to MP3 (c) 2024 Ma_Sys.ma <info@masysma.net>

# ! Before using this makefile, ensure there are no spaces in src dir
# ! find $(PREFIX) | grep -F " "

PREFIX = /data/programs/music2
SRC    = $(PREFIX)/album $(PREFIX)/epic $(PREFIX)/track
DST    = /media/sdc1/music

SRC_FLACFILES = $(shell find $(SRC) -name '*.flac')
SRC_OGGFILES  = $(shell find $(SRC) -name '*.ogg')
SRC_MP3FILES  = $(shell find $(SRC) -name '*.mp3')

DST_FLACFILES = $(SRC_FLACFILES:$(PREFIX)/%.flac=$(DST)/%.mp3)
DST_OGGFILES  = $(SRC_OGGFILES:$(PREFIX)/%.ogg=$(DST)/%.mp3)
DST_MP3FILES  = $(SRC_MP3FILES:$(PREFIX)/%=$(DST)/%)

# $(1) - source file

define FLACTOMP3
$(1:$(PREFIX)/%.flac=$(DST)/%.mp3): $(1)
	mkdir -p $$(dir $$@)
	ffmpeg -i $$< -qscale:a 0 $$@

endef

define OGGTOMP3
$(1:$(PREFIX)/%.ogg=$(DST)/%.mp3): $(1)
	mkdir -p $$(dir $$@)
	ffmpeg -i $$< -qscale:a 0 $$@

endef

define COPYMP3
$(1:$(PREFIX)/%=$(DST)/%): $(1)
	mkdir -p $$(dir $$@)
	cp $$< $$@

endef

all: $(DST_FLACFILES) $(DST_OGGFILES) $(DST_MP3FILES)

$(eval $(foreach SRC,$(SRC_FLACFILES),$(call FLACTOMP3,$(SRC))))
$(eval $(foreach SRC,$(SRC_OGGFILES),$(call OGGTOMP3,$(SRC))))
$(eval $(foreach SRC,$(SRC_MP3FILES),$(call COPYMP3,$(SRC))))
