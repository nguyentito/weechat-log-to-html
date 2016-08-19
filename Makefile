HS = ghc
HS_FLAGS = -O
TARGET = weechat-log-to-html
SRC = weechat-log-to-html.mod.hs

HTML_HEAD = head.html

###############################################################################

all: $(TARGET)

$(TARGET): $(SRC)
	$(HS) --make -o $@ $(SRC)

%.mod.hs: %.hs $(HTML_HEAD)
	cutline=$$(grep -nm 1 -- "-- INSERT FROM HERE" $< | cut -f1 -d:) ;\
		head -n $$(expr $$cutline - 1) $< > $@
	echo "header = unlines [" >> $@
	cat $(HTML_HEAD) | sed -e 's/"/\\"/g' -e 's/^/    "/g' -e 's/$$/",/g' >> $@
	echo '    ""]' >> $@
	cutlineEnd=$$(grep -nm 1 -- "-- INSERT TO HERE" $< | cut -f1 -d:) ;\
		tail --lines=+$$(expr $$cutlineEnd + 1) $< >> $@


clean:
	rm -f *.o *.hi *.mod.hs

cleanall: clean
	rm -f $(TARGET)
