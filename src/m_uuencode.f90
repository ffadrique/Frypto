module m_uuencode

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : uuencode enconding
!
! Algorithm
!  - Divide the input bytes stream into blocks of 3 bytes.
!  - Divide the 24 bits of a 3-byte block into 4 groups of 6 bits.
!  - If the last 3-byte block has only 1 byte of input data, pad 2 bytes of 1 (\x0101).
!  - If the last 3-byte block has only 2 bytes of input data, pad 1 byte of 1 (\x01).
!  - Expand each group of 6 bits to 8 bits and add 32, \x20, so the resulting
!    bit map is representing an ASCII printable character.
!
! This implementation is for uuencoding only.
! Line braking into 45 byte block and the leading byte with the number of
! bytes encoded per line must be implemented in the user class.
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

  use m_ascii_encoder

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_uuencode
  public uuencode

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! uuencode type
  type, extends(t_ascii_encoder) :: t_uuencode
    private

    contains

!     Block encoding
      procedure, nopass :: encode_block => uuencode_encode_block

!     Block decoding
      procedure, nopass :: decode_block => uuencode_decode_block

!     Length of the trailing part of the data
      procedure, nopass :: trail_len => uuencode_trail_len

  end type t_uuencode

! Decoded block size
  integer, parameter :: uuencode_blk_size = 3

! Encoded block size
  integer, parameter :: uuencode_cblk_size = 4

! Padding constants
  integer(kind=1), parameter :: uuencode_padding = 0_1
  integer(kind=1), parameter :: uuencode_cpadding = ichar('`',1)

! UUEncode algorithm constant
 integer(kind=1), parameter :: uuencode_algo_offset =   32_1 ! z'20'

! Padding map from decoded to encoded
! Number of coded characters as function of characters in decoded block
! A__ --> XX==
! AB_ --> XXX=
! ABC --> XXXX
  integer, dimension(1:3), parameter :: uuencode_encoded_trailing = [ 2, 3, 4 ]

! Padding map from encoded to decoded
! Number of coded characters as function of characters in coded block
! XX== --> A__
! XXX= --> AB_
! XXXX --> ABC
  integer, dimension(1:4), parameter :: uuencode_decoded_trailing = [ 0, 1, 2, 3 ]

! Constructor interface
  interface uuencode
    module procedure uuencode_default
    module procedure uuencode_main
  end interface uuencode

!- End of module variable declarations ----------------------------------------

contains

! Default onstuctor
pure function uuencode_default() result(res)

! The uuencode context
  type(t_uuencode) :: res

! Initialise the processing structures
  res = uuencode( .true. )

end function uuencode_default


! Main constuctor
pure function uuencode_main( padding ) result(res)

! Flag to generate encoding with padding
  logical, intent(in) :: padding

! The uuencode context
  type(t_uuencode) :: res

! Initialise the processing structures
  call res%initialise( uuencode_blk_size, uuencode_cblk_size, &
                       uuencode_padding, uuencode_cpadding, &
                       uuencode_encoded_trailing, uuencode_decoded_trailing, &
                       padding )

end function uuencode_main


! UUencode encode one block
pure subroutine uuencode_encode_block( blk, cblk )

! The block to encode (3 bytes)
  integer(kind=1), dimension(:), intent(in) :: blk

! The encoded block (4 bytes)
  integer(kind=1), dimension(:), intent(out) :: cblk

! Patterns
!        1         2         2         3         3         4  res
!        1         1         2         2         3         3  blk
!        1         2         3         4         5         6  pattern
! 11111100  00000011  11110000  00001111  11000000  00111111
!    z'FC'     z'03'     z'F0'     z'0F'     z'C0'     z'3F'
  integer(kind=1), dimension(6) , parameter :: pattern = [ -4_1, 3_1, -16_1, 15_1, -64_1, 63_1 ]

! Assign the resulting bytes from the 6-bit blocks
  cblk(1) = ishft( iand( blk(1), pattern(1) ), -2 )
  cblk(2) = ishft( iand( blk(1), pattern(2) ), 4 ) + &
            ishft( iand( blk(2), pattern(3) ), -4 )
  cblk(3) = ishft( iand( blk(2), pattern(4) ), 2 ) + &
            ishft( iand( blk(3), pattern(5) ), -6 )
  cblk(4) = iand( blk(3), pattern(6) )

! Encode
  where( cblk /= 0 )
    cblk = cblk + uuencode_algo_offset
  else where
    cblk = uuencode_cpadding
  end where

end subroutine uuencode_encode_block


! UUencode decode one block
pure subroutine uuencode_decode_block( cblk, blk )

! The block to encode (4 bytes)
  integer(kind=1), dimension(:), intent(in) :: cblk

! The decoded block (3 bytes)
  integer(kind=1), dimension(:), intent(out) :: blk

! Patterns
!        1         1         2         2         3         3
!        1         2         2         3         3         4  blk
!        1         2         3         4         5         6  pattern
! 00111111  00110000  00001111  00111100  00000011  00111111
!    z'3F'     z'30'     z'0F'     z'3C'     z'03'     z'3F'
  integer(kind=1), dimension(6) , parameter :: pattern = [ 63_1, 48_1, 15_1, 60_1, 3_1, 63_1 ]
  integer(kind=1), dimension(4) :: i6bit

! Decode the 6-bit representation
  where( cblk /= uuencode_cpadding )
    i6bit = cblk - uuencode_algo_offset
  else where
    i6bit = 0_1
  end where

! Assign the resulting bytes from the 6-bit blocks
  blk(1) = ishft( iand( i6bit(1), pattern(1) ), 2 ) + &
           ishft( iand( i6bit(2), pattern(2) ), -4 )
  blk(2) = ishft( iand( i6bit(2), pattern(3) ), 4 ) + &
           ishft( iand( i6bit(3), pattern(4) ), -2 )
  blk(3) = ishft( iand( i6bit(3), pattern(5) ), 6 ) + &
           iand( i6bit(4), pattern(6) )

end subroutine uuencode_decode_block


! Return length of the trailing part of the data
pure function uuencode_trail_len( text ) result(res)

! The encoded block (4 bytes)
  integer(kind=1), dimension(:), intent(in) :: text

! The encoded data without padding
  integer :: res

! Local variables
  integer(kind=1), dimension(uuencode_cblk_size) :: cblk

! Initialise
  cblk = text(size(text)-uuencode_cblk_size+1:)

! Return the length
  res = max( 2, size( pack( cblk, cblk /= uuencode_cpadding ) ) )

end function uuencode_trail_len

end module m_uuencode
