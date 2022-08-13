%.o: %.c
	gcc -c -g -o $@ $<
libaes_ref.a: aes256.o blowfish.o
	ar r $@ $?
