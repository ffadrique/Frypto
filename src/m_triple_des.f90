module m_triple_des

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : (Triple) Data Encryption Standard (3DES) implementation
! Reference : National Insitute of Standards and Technology (NIST)
!             Federal Information Processing Standards Publication (FIPS) 46
!             https://csrc.nist.gov/publications/detail/fips/46/3/archive/1999-10-25
!
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
!------------------------------------------------------------------------------

!- Start of use statements ----------------------------------------------------

  use m_block_cipher
  use m_des

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_triple_des
  public triple_des

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! 3DES type
  type, extends(t_block_cipher) :: t_triple_des
    private

!     Encryption key
      integer(kind=1), dimension(:), allocatable :: key

!     DES algorithm context
      type(t_des), dimension(:), allocatable :: des

    contains

!     Block size
      procedure, nopass :: block_size => triple_des_block_size
      
!     Key size
      procedure, nopass :: key_size => triple_des_key_size

!     Encryption interface
      procedure :: encrypt => triple_des_encrypt

!     Decryption interface
      procedure :: decrypt => triple_des_decrypt

  end type t_triple_des


! The DES block size in bytes
  integer, parameter :: triple_des_block_size_in_bytes = 8

! The DES key size in bytes
  integer, parameter :: triple_des_key_size_in_bytes = 24

!- End of module variable declarations ----------------------------------------

contains

! Constuctor from key
! Assumes that the key is either 16 or 24 bytes
pure function triple_des( key ) result(res)

! The initialisation key
  integer(kind=1), dimension(:), intent(in) :: key

! The DES context
  type(t_triple_des) :: res

! Local variables
  integer :: nkeys, ikey, jkey
  type(t_des) :: des0
  integer :: des_key_size

! Initialise key
  allocate( res%key, source=key )
  des_key_size = des0%key_size()
  
! Get the number of keys
  nkeys = size(key) / des_key_size

! Allocate the DES structures
  allocate( res%des(nkeys) )

! Initialise the DES structures
  do ikey = 1, nkeys
    jkey = des_key_size * ( ikey - 1 ) + 1
    res%des(ikey) = des( key(jkey:jkey+des_key_size-1) )
  end do

end function triple_des


! 3DES block size
pure function triple_des_block_size() result(res)

! 3DES block size
  integer :: res
   
! Return the 3DES block size
  res = triple_des_block_size_in_bytes
   
end function triple_des_block_size


! 3DES key size
pure function triple_des_key_size() result(res)

! 3DES key size
  integer :: res
   
! Return the 3DES key size
  res = triple_des_key_size_in_bytes
   
end function triple_des_key_size


! 3DES encryption (from 8 bytes buffer)
pure subroutine triple_des_encrypt( this, text, cipher )

! The DES context
  class(t_triple_des), intent(in) :: this

! The plain text to encrypt
  integer(kind=1), dimension(:), intent(in) :: text

! The encrypted text
  integer(kind=1), dimension(size(text)), intent(out) :: cipher

! Local variables
  integer(kind=1), dimension(size(text)) :: buffer

! Encrypt
  call this%des(1)%encrypt( text,   buffer )
  call this%des(2)%decrypt( buffer, buffer )
  if( size(this%des) == 2 ) then
    call this%des(1)%encrypt( buffer, cipher )
  else
    call this%des(3)%encrypt( buffer, cipher )
  end if

end subroutine triple_des_encrypt


! 3DES decryption (from 8 bytes buffer)
pure subroutine triple_des_decrypt( this, cipher, text )

! The DES context
  class(t_triple_des), intent(in) :: this

! The encrypted text
  integer(kind=1), dimension(:), intent(in) :: cipher

! The plain decrypted text
  integer(kind=1), dimension(size(cipher)), intent(out) :: text

! Local variables
  integer(kind=1), dimension(size(text)) :: buffer1, buffer2

! Decrypt
  if( size(this%des) == 2 ) then
    call this%des(1)%decrypt( cipher, buffer1 )
  else
    call this%des(3)%decrypt( cipher, buffer1 )
  end if
  call this%des(2)%encrypt( buffer1, buffer2 )
  call this%des(1)%decrypt( buffer2, text )

end subroutine triple_des_decrypt

end module m_triple_des
