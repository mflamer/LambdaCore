

lcvm.bin: lcvm.o
	ft32-elf-ld --oformat binary -o lcvm.bin lcvm.o	

lcvm.o:
	ft32-elf-gcc -Wa,-a=lst -c -o lcvm.o lcvm.S

clean:
	$(RM) lcvm.o
	$(RM) lcvm.bin

distclean: clean