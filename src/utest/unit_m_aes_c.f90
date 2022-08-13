module unit_m_aes_c

!-------------------------------------------------------------------------------
! License   : This file is part of Frypto.
!
!             Frypto is free software: you can redistribute it and/or modify
!             it under the terms of the GNU Lesser General Public License as
!             published by the Free Software Foundation, either version 3 of
!             the License, or (at your option) any later version.
!
!             Frypto is distributed in the hope that it will be useful,
!             but WITHOUT ANY WARRANTY; without even the implied warranty of
!             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!             See the GNU Lesser General Public License for more details.
!
!             You should have received a copy of the GNU Lesser General Public
!             License along with Frypto.  
!             If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------

  use, intrinsic :: iso_c_binding

! Local variables
  type, bind(c) :: c_aes
    integer(kind=1), dimension(32) :: key = 0_1
    integer(kind=1), dimension(32) :: enckey = 0_1
    integer(kind=1), dimension(32) :: deckey = 0_1
  end type c_aes


! C operation interfaces

  interface
    subroutine aes256_init( c_ctx, key ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_aes
      type(c_aes),                   intent(inout) :: c_ctx
      integer(kind=1), dimension(*), intent(in)    :: key
    end subroutine aes256_init
  end interface

  interface
    subroutine aes256_done( c_ctx ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_aes
      type(c_aes),                intent(inout) :: c_ctx
    end subroutine aes256_done
  end interface

  interface
    subroutine aes256_encrypt_ecb( c_ctx, text ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_aes
      type(c_aes),                   intent(inout) :: c_ctx
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes256_encrypt_ecb
  end interface

  interface
    subroutine aes256_decrypt_ecb( c_ctx, text ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_aes
      type(c_aes),                   intent(inout) :: c_ctx
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes256_decrypt_ecb
  end interface

  interface
    subroutine aes_expandenckey( key, rc ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: key
      integer(kind=1),               intent(inout) :: rc
    end subroutine aes_expandenckey
  end interface

  interface
    subroutine aes_expanddeckey( key, rc ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: key
      integer(kind=1),               intent(inout) :: rc
    end subroutine aes_expanddeckey
  end interface

  interface
    subroutine aes_shiftrows( text ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes_shiftrows
  end interface

  interface
    subroutine aes_shiftrows_inv( text ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes_shiftrows_inv
  end interface

  interface
    subroutine aes_mixcolumns( text ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes_mixcolumns
  end interface

  interface
    subroutine aes_mixcolumns_inv( text ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes_mixcolumns_inv
  end interface

  interface
    subroutine aes_subbytes( text ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes_subbytes
  end interface

  interface
    subroutine aes_subbytes_inv( text ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
    end subroutine aes_subbytes_inv
  end interface

  interface
    subroutine aes_addroundkey( text, key ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
      integer(kind=1), dimension(*), intent(in)    :: key
    end subroutine aes_addroundkey
  end interface

  interface
    subroutine aes_addroundkey_cpy( text, key, cpk ) bind(c)
      use, intrinsic :: iso_c_binding
      integer(kind=1), dimension(*), intent(inout) :: text
      integer(kind=1), dimension(*), intent(in)    :: key
      integer(kind=1), dimension(*), intent(inout) :: cpk
    end subroutine aes_addroundkey_cpy
  end interface

end module unit_m_aes_c
