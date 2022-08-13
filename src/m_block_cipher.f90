module m_block_cipher

! -----------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsys  : Abstract class interface for block ciphers (AES, DES, ...)
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
! -----------------------------------------------------------------------------

! Dependencies ----------------------------------------------------------------

  use m_object

  implicit none

! Public/Private declarations -------------------------------------------------

  private
  public t_block_cipher

! Module declarations ---------------------------------------------------------

! Abstract class interface for block ciphers (AES, DES, ...)
  type, extends(t_object), abstract :: t_block_cipher
    private

    contains

!     Block size
      procedure(i_block_cipher_size), nopass, deferred :: block_size
      
!     Key size
      procedure(i_block_cipher_size), nopass, deferred :: key_size
      
!     Encryption interfacce
      procedure(i_block_cipher_encrypt), deferred :: encrypt
    
!     Decryption interfacce
      procedure(i_block_cipher_decrypt), deferred :: decrypt
    
  end type t_block_cipher

  
! Deferred interfaces
  interface
  
!   Interface for sizes
    pure function i_block_cipher_size() result(res)
      integer :: res
    end function i_block_cipher_size
  
!   Interfae for encryption
    pure subroutine i_block_cipher_encrypt( this, text, cipher )
      import t_block_cipher
      class(t_block_cipher), intent(in) :: this
      integer(kind=1), dimension(:), intent(in) :: text
      integer(kind=1), dimension(size(text)), intent(out) :: cipher
    end subroutine i_block_cipher_encrypt
    
!   Interfae for decryption
    pure subroutine i_block_cipher_decrypt( this, cipher, text )
      import t_block_cipher
      class(t_block_cipher), intent(in) :: this
      integer(kind=1), dimension(:), intent(in) :: cipher
      integer(kind=1), dimension(size(cipher)), intent(out) :: text
    end subroutine i_block_cipher_decrypt
    
  end interface
  
! Implementation --------------------------------------------------------------

contains
end module m_block_cipher
