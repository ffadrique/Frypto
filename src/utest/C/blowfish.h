/*
blowfish.h:  Header file for blowfish.c

Copyright (C) 1997 by Paul Kocher

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


See blowfish.c for more information about this file.
*/

#include <stdio.h>
  
typedef struct {
  unsigned long P[16 + 2];
  unsigned long S[4][256];
  unsigned char *key;
} BLOWFISH_CTX;

void blowfish_init0(BLOWFISH_CTX *ctx);
void blowfish_init(BLOWFISH_CTX *ctx, unsigned char *key, int keyLen);
void blowfish_encrypt(BLOWFISH_CTX *ctx, unsigned long *xl, unsigned long *xr);
void blowfish_decrypt(BLOWFISH_CTX *ctx, unsigned long *xl, unsigned long *xr);

unsigned long f(BLOWFISH_CTX *ctx, unsigned long x);


