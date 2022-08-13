module m_crc32

! -----------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Cyclic redundancy code/checksum; 4 bytes
!             Ref: https://rosettacode.org/wiki/CRC-32#Fortran
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
  use m_util_convert
  
  implicit none

! Public/Private declarations -------------------------------------------------

  private
  public t_crc32, crc32

! Module declarations ---------------------------------------------------------

! Cyclic redundancy code/checksum; 4 bytes
  type, extends(t_object) :: t_crc32
    private

!     Lookup table for CRC polynomial
      integer, dimension(0:255) :: table = 0

    contains

!     Initialse the lookup table
      procedure :: init_table => crc32_init_table

!     Cenerate the CRC
      generic :: encode => crc32_encode_string, crc32_encode_buffer
      procedure, private :: crc32_encode_string
      procedure, private :: crc32_encode_buffer

  end type t_crc32

! Constructor interface
  interface crc32
    module procedure crc32_default
  end interface crc32

! Implementation --------------------------------------------------------------

contains

! Default constructor
elemental function crc32_default() result(res)

! Returned object
  type(t_crc32) :: res

! Initialise the lookup table
  call res%init_table()

end function crc32_default


! Initialise CRC lookup table
! Algorithm from https://rosettacode.org/wiki/CRC-32#Fortran
pure subroutine crc32_init_table( this )

! Calling object
  class(t_crc32), intent(inout) :: this

! Local variables
  integer :: i, j, k

! Loop on the table elements
  do i = 0, 255

!   Initilise value for this table element
    k = i

!   Loop on the 8 bits of a byte
    do j = 1, 8

!     Table element algorithm
      if (btest(k, 0)) then
        k = ieor(shiftr(k, 1), -306674912)
      else
        k = shiftr(k, 1)
      end if

    end do

!   Store table element
    this%table(i) = k

  end do

end subroutine crc32_init_table


! Compute the CRC32 for a byte buffer array
pure function crc32_encode_buffer( this, bytes ) result(res)

! Calling object
  class(t_crc32), intent(in) :: this

! Bytes to encode
  integer(kind=1), dimension(:), intent(in) :: bytes

! The resulting CRC32
  integer :: res

! Local variables
  integer :: i, n, idx

! Initialise
  res = 0
  n = size(bytes)

! Initialise CRC
  res = not(res)

! Loop on the input bytes
  do i = 1, n

!   Compute index for the lookup table
    idx = iand( ieor( res, int(bytes(i),4) ), 255 )

!   Compute intermedate value of CRC
    res = ieor( shiftr( res, 8 ), this%table(idx) )

  end do

! Final CRC value
  res = not(res)

end function crc32_encode_buffer


! Compute the CRC32 for a character string
pure function crc32_encode_string( this, s ) result(res)

! Calling object
  class(t_crc32), intent(in) :: this

! Bytes to encode
  character(len=*), intent(in) :: s

! The resulting CRC32
  integer :: res

! Call the byte buffer subroutine
  res = this%encode( character_to_bytes(s) )

end function crc32_encode_string

end module m_crc32
