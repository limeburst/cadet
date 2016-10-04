MCU = atmega32u4

CFLAGS += -DF_CPU=16000000UL
CFLAGS += -Os
CFLAGS += -mmcu=$(MCU)
CFLAGS += -std=c11

LDFLAGS += -Wl,--gc-sections

all: cadet.hex

cadet.hex: cadet.elf
	avr-objcopy -O ihex cadet.elf cadet.hex

cadet.elf: cadet.o usb.o print.o
	avr-gcc -gcc $(CFLAGS) $^ -o cadet.elf $(LDFLAGS)

%.o : %.c
	avr-gcc -c $(CFLAGS) $< -o $@ 

clean: 
	rm -f *.o *.elf *.hex

erase:
	dfu-programmer $(MCU) erase

flash: cadet.hex
	dfu-programmer $(MCU) flash cadet.hex

launch:
	dfu-programmer $(MCU) launch --no-reset

.PHONY: all clean erase flash launch
