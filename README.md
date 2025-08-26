# Frypto — Cryptographic Primitives for Modern Fortran

Secure hashing, encryption, and UUID generation in Fortran.



## Overview

Frypto is a collection of cryptographic modules written in modern Fortran. It provides secure, low-level components for hashing, encryption, and unique identifier generation. Designed to integrate with [Fommons](https://github.com/ffadrique/Fommons) and [XFunit](https://github.com/ffadrique/XFunit).



## Modules

- `m_md5`, `m_sha1`, `m_sha256`, `m_sha512`: Hashing algorithms  
- `m_aes`, `m_des`, `m_3des`: Symmetric encryption  
- `m_uuid`: RFC 4122-compliant UUID generation  
- `m_crypto_util`: Utility functions for padding, encoding, and byte manipulation  



## Dependencies

- [Fommons](https://github.com/ffadrique/Fommons): String handling and utilities  
- [XFunit](https://github.com/ffadrique/XFunit): Unit testing framework  



## Testing

Unit tests are written using XFunit and cover:

- Hash correctness  
- Encryption/decryption round-trips  
- UUID format validation  

Tests are located in the `unit_test_driver/` directory.



## Build Instructions

### Windows
Fommons is provided with a Visual Studio 2019 configured solution that allows building and testing the entire suite. The solution provides a C# project that integrates the unit test in Fortran with the unit test detection feature of Visual Studio. This allows the execution of all unit tests from the Test Explorer menu.

Create the environment variable `XFUNIT_ROOT_DIR=$(ProjectDir)..` for each unit testing project to store the test output (.jxml file) in the `utest` directory. This configuration depends on the windows user in file `unit_m_xfunit_<functionname>.vfproj.<windows user>.user`

### Linux
Frypto is provided with `gmake` makefiles to build and test the entire suite. 
To build the Frypto library and use modules files execute the following command in the `src` directory
```make
gmake libs
```
To build the Frypto library, use modules files and unit tests execute the following command in the `src` directory
```make
gmake all
```
To execute the unit tests execute the following command in the `src` or `utest` directory
```make
gmake units
```
The default compiler is `gfortran` but can be overridden from the command line to use Intel Fortran
```make
gmake F90COMP=ifort
```
The ifort or gfortran commands must be in the execution path.
Alternatively, the makefile can be edited to provide the full path to the compiler.
Compiler options are automatically selected for the active compiler; only Debug configuration is provided in the make files.

### Directory Structure

Frypto/
+-- documentation/
+-- src/
¦   +-- aes/
¦   +-- sha/
¦   +-- md5/
¦   +-- des/
¦   +-- uuid/
+-- unit_test_driver/
+-- LICENSE
+-- README.md

## Documentation

This readme page is the main user documentation. In addition, documentation generated with FORD and Doxygen can be found in the `documentation` directory

## Sample Usage

### SHA-256 Hash

```fortran
use m_sha256
type(t_sha256) :: sha
character(len=64) :: digest

call sha%init()
call sha%update("hello world")
call sha%final(digest)

print *, "SHA-256:", trim(digest)
```

### AES Encryption

```fortran
use m_aes
type(t_aes) :: aes
character(len=32) :: key = "0123456789abcdef0123456789abcdef"
character(len=16) :: plaintext = "secret message"
character(len=16) :: ciphertext, decrypted

call aes%set_key(key)
call aes%encrypt_block(plaintext, ciphertext)
call aes%decrypt_block(ciphertext, decrypted)

print *, "Encrypted:", trim(ciphertext)
print *, "Decrypted:", trim(decrypted)
```

### UUID Generation

```fortran
use m_uuid
type(t_uuid) :: uuid_gen
character(len=36) :: uuid

call uuid_gen%generate_v4(uuid)
print *, "UUID:", trim(uuid)
```

## Licensing

## ?License
Frypto is open-source software, licensed under the GNU Lesser General Public License (LGPL).

## Related Projects

- [Fommons](https://github.com/ffadrique/Fommons): String handling and utilities  
- [XFunit](https://github.com/ffadrique/XFunit): Unit testing framework  
