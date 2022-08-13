module m_crypto_padding

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Encryption/decryption padding
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

  use m_object

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_crypto_padding
  public crypto_padding

  public crypto_padding_zero
  public crypto_padding_ansix923
  public crypto_padding_pkcs7

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Encryption padding scheme enumeration
  integer, parameter :: crypto_padding_zero     = 0
  integer, parameter :: crypto_padding_ansix923 = 2
  integer, parameter :: crypto_padding_pkcs7    = 3

! The encryption type
  type, extends(t_object) :: t_crypto_padding
    private

    contains

!     Generate the cryto padding text
      procedure, nopass :: text => crypto_padding_text

!     Remove the cryto padding from text
      procedure, nopass :: remove => crypto_padding_remove

  end type t_crypto_padding

!- End of module variable declarations ----------------------------------------

contains

! Constuctor from key
pure function crypto_padding() result(res)

! The crypto context
  type(t_crypto_padding) :: res

end function crypto_padding


! Generate the cryto padding text
pure subroutine crypto_padding_text( text, scheme, block_size, padding_text )

! The text to process
  integer(kind=1), dimension(:), intent(in) :: text

! The padding scheme (from enumeration)
  integer, intent(in) :: scheme
  
! The block size
  integer, intent(in) :: block_size

! The padding text
  integer(kind=1), dimension(:), allocatable, intent(out) :: padding_text

! Local variables
  integer :: padlen

! Compute the padding length
  padlen = mod( size(text), block_size )
  padlen = mod( block_size - padlen, block_size )
  if( padlen > 0 ) then

!   Allocate padding text
    allocate( padding_text(padlen) )

!   Select the padding scheme
    select case( scheme )

      case( crypto_padding_zero )
        padding_text = padding_zero_text(padlen)

      case( crypto_padding_ansix923 )
        padding_text = padding_ansix923_text(padlen)

      case( crypto_padding_pkcs7 )
        padding_text = padding_pkcs7_text(padlen)

   end select

  end if

end subroutine crypto_padding_text


! Zero padding scheme
pure function padding_zero_text( padlen ) result(res)

! The padding size
  integer, intent(in) :: padlen

! The padding text
  integer(kind=1), dimension(padlen) :: res

! Generate the padding text
  res = 0_1

end function padding_zero_text


! ANSI X.923 padding scheme
pure function padding_ansix923_text( padlen ) result(res)

! The padding size
  integer, intent(in) :: padlen

! The padding text
  integer(kind=1), dimension(padlen) :: res

! Generate the padding text
  res(:padlen-1) = 0_1
  res(padlen) = int(padlen,kind=1)

end function padding_ansix923_text


! PKCS7 padding scheme
pure function padding_pkcs7_text( padlen ) result(res)

! The padding size
  integer, intent(in) :: padlen

! The padding text
  integer(kind=1), dimension(padlen) :: res

! Generate the padding text
  res = int(padlen,kind=1)

end function padding_pkcs7_text


! Remove the cryto padding from text
pure subroutine crypto_padding_remove( block, text )

! The text to process
  integer(kind=1), dimension(:), intent(in) :: block

! The text without the padding
  integer(kind=1), dimension(:), allocatable, intent(out) :: text
  
! Local variables
  integer :: k, n, nbytes
  integer(kind=1) :: last
  integer(kind=1), dimension(size(block)) :: local

! Initialise
  local = block
  n = size(block)
  last = local(n)
  
! Assume no padding by default
  nbytes = n

! Check the last value with respect to the block size
  if( last > 0_1 .and. last < n ) then

!   Check the padding for PKC7
    if( all( local(n-last+1:n) == last ) ) then

!     This is PKC7
      nbytes = n - last

!   This may be ANSI X.923
    else if( all( local(n-last+1:n-1) == 0_1 ) ) then

!     This is ANSI X.923
      nbytes = n - last

    end if

  else if( last == 0_1 ) then

!   This may be zero-padding
    do k = 1, n
      if( local(n - k + 1_1) /= 0_1 ) exit
    end do

!   Check trailing zeroes
    if( k > 1 ) then

!     This is zero padding
      nbytes = n - k + 1

    end if

  end if

! Return unpadded buffer
  allocate( text(nbytes), source=local(:nbytes) )

end subroutine crypto_padding_remove

end module m_crypto_padding
