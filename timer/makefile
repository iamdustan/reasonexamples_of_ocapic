
emulate:
	./timer 'ocapic_circuit_simulator circuit.txt'

build:
	ocapic \
		18f4620 \
		config.asm \
		-pp "refmt -parse re -print ml" timer.re.ml \
		-o timer.asm

assemble: build
	gpasm -y timer.asm

compile: build
default: build

