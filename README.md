# 🔐 Frypto — Cryptographic Primitives for Modern Fortran

**Frypto** is a collection of cryptographic modules written in modern Fortran. It provides secure, low-level components for hashing, encryption, and unique identifier generation. Frypto is part of a broader ecosystem that includes [Fommons](https://github.com/ffadrique/Fommons) and [XFunit](https://github.com/ffadrique/XFunit), and is designed to integrate seamlessly with them.

---

## 📚 Modules Included

Frypto encompasses the following Fortran modules:

- `m_md5`, `m_sha1`, `m_sha256`, `m_sha512`: Hashing algorithms  
- `m_aes`, `m_des`, `m_3des`: Symmetric encryption  
- `m_uuid`: RFC 4122-compliant UUID generation  
- `m_crypto_util`: Utility functions for padding, encoding, and byte manipulation  

Each module defines a `t_` type encapsulating its logic and state, enabling object-oriented usage patterns.

---

## 🧰 Dependencies

- [Fommons](https://github.com/ffadrique/Fommons): For string handling, file I/O, and utility functions  
- [XFunit](https://github.com/ffadrique/XFunit): For unit testing  

Frypto depends on Fommons for runtime support and XFunit for testing. The dependency graph is designed to avoid circular references.

---

## 🧪 Testing

Unit tests are written using XFunit and cover:

- Hash correctness (test vectors)  
- Encryption/decryption round-trips  
- UUID uniqueness and format validation  

Tests are located in the `unit_test_driver/` directory.

### Linux

```bash
cd src
gmake all        # Builds the library
gmake units      # Runs unit tests
# 🔐 Frypto — Cryptographic Primitives for Modern Fortran

**Frypto** is a collection of cryptographic modules written in modern Fortran. It provides secure, low-level components for hashing, encryption, and unique identifier generation. Frypto is part of a broader ecosystem that includes [Fommons](https://github.com/ffadrique/Fommons) and [XFunit](https://github.com/ffadrique/XFunit), and is designed to integrate seamlessly with them.

---

## 📚 Modules Included

Frypto encompasses the following Fortran modules:

- `m_md5`, `m_sha1`, `m_sha256`, `m_sha512`: Hashing algorithms  
- `m_aes`, `m_des`, `m_3des`: Symmetric encryption  
- `m_uuid`: RFC 4122-compliant UUID generation  
- `m_crypto_util`: Utility functions for padding, encoding, and byte manipulation  

Each module defines a `t_` type encapsulating its logic and state, enabling object-oriented usage patterns.

---

## 🧰 Dependencies

- [Fommons](https://github.com/ffadrique/Fommons): For string handling, file I/O, and utility functions  
- [XFunit](https://github.com/ffadrique/XFunit): For unit testing  

Frypto depends on Fommons for runtime support and XFunit for testing. The dependency graph is designed to avoid circular references.

---

## 🧪 Testing

Unit tests are written using XFunit and cover:

- Hash correctness (test vectors)  
- Encryption/decryption round-trips  
- UUID uniqueness and format validation  

Tests are located in the `unit_test_driver/` directory.

### Linux

```bash
cd src
gmake all        # Builds the library
gmake units      # Runs unit tests
