CC = avr-gcc
DFU = dfu-programmer
MCU = atmega32u4
OBJCOPY = avr-objcopy
STACK = stack

CFLAGS += -DF_CPU=16000000UL
CFLAGS += -Os
CFLAGS += -mmcu=$(MCU)
CFLAGS += -std=c11

haskell = cadet.cabal stack.yaml
ivory-gen = cadet.c ivory.h ivory_asserts.h ivory_templates.h
ivory-src = app/Main.hs src/Lib.hs
objects = cadet.o main.o print.o usb.o

cadet.hex: cadet.elf
	$(OBJCOPY) -O ihex $< $@

cadet.elf: $(objects)
	$(CC) $(CFLAGS) -o $@ $(objects)

cadet.o: $(ivory-gen)
main.o: main.c print.h usb.h cadet.h
print.o: print.c print.h usb.h
usb.o: usb.c usb.h cadet.h

cadet.c: $(ivory-src) $(haskell)
	$(STACK) build
	$(STACK) exec cadet -- --src-dir=.

clean: 
	$(RM) $(objects) $(ivory-gen) cadet.hex cadet.elf

erase:
	$(DFU) $(MCU) erase

flash: cadet.hex
	$(DFU) $(MCU) flash $<

launch:
	$(DFU) $(MCU) launch --no-reset

all: cadet.hex

.PHONY: all clean erase flash launch
