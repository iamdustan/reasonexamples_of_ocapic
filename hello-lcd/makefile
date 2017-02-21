
emulate:
	./hw 'ocapic_lcd_simulator 16x2 e=RD0 rs=RD2 rw=RD1 bus=PORTB'

build:
	ocapic \
		18f4620 \
		config.asm \
		-pp "refmt -parse re -print ml" hw.re.ml \
		-o hw.asm

assemble: build
	gpasm -y hw.asm

compile: build
default: build
