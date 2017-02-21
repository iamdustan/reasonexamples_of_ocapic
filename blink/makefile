
emulate:
	./led ocapic_dip40_simulator

build:
	ocapic \
		18f4620 \
		config.asm \
		-pp "refmt -parse re -print ml" led.re.ml \
		-o led.asm

assemble: build
	gpasm -y led.asm

compile: build
default: build

