

lcvm.bin: lcvm.o
	v3-elf-ld --oformat binary -o lcvm.bin lcvm.o	

lcvm.o:
	v3-elf-gcc -Wa,-a=lst -c -o lcvm.o lcvm.S
