module m_base64

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Base64 encoding
!
! Algorithm
!  - Divide the input bytes stream into blocks of 3 bytes.
!  - Divide 24 bits of each 3-byte block into 4 groups of 6 bits.
!  - Map each group of 6 bits to 1 printable character, based on the 6-bit
!    value using the Base64 character set map.
!  - If the last 3-byte block has only 1 byte of input data, pad 2 bytes of zero
!    (\x0000). After encoding it as a normal block, override the last 2
!    characters with 2 equal signs (==), so the decoding process knows 2 bytes
!    of zero were padded.
!  - If the last 3-byte block has only 2 bytes of input data, pad 1 byte of zero
!    (\x00). After encoding it as a normal block, override the last 1 character
!    with 1 equal signs (=), so the decoding process knows 1 byte of zero was padded.
!
!    Not yet implemented:
!    Carriage return (\r) and new line (\n) are inserted into the output character
!    stream. They will be ignored by the decoding process.
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
  public t_base64
  public base64

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Base64 encoding type
  type, extends(t_ascii_encoder) :: t_base64
    private

    contains

!     Block encoding
      procedure, nopass :: encode_block => base64_encode_block

!     Block decoding
      procedure, nopass :: decode_block => base64_decode_block

!     Length of the trailing part of the data
      procedure, nopass :: trail_len => base64_trail_len

  end type t_base64

! Decoded block size
  integer, parameter :: base64_blk_size = 3

! Encoded block size
  integer, parameter :: base64_cblk_size = 4

! Encoding lookup table (character representation)
  character, dimension(0:63), parameter :: base64_encode_chars = [ &
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', &
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', &
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', &
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', &
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/' ]

! Encoding lookup table (byte representation)
  integer(kind=1), dimension(0:63), parameter :: base64_encode_dictionary = ichar(base64_encode_chars,1)

! Decoding lookup table (byte representation)
  integer(kind=1), dimension(43:122), parameter :: base64_decode_dictionary = [ &
    62_1,  0_1,  0_1,  0_1, 63_1, 52_1, 53_1, 54_1, 55_1, 56_1, 57_1, 58_1, 59_1, &
    60_1, 61_1,  0_1,  0_1,  0_1,  0_1,  0_1,  0_1,  0_1,  0_1,  1_1,  2_1,  3_1, &
     4_1,  5_1,  6_1,  7_1,  8_1,  9_1, 10_1, 11_1, 12_1, 13_1, 14_1, 15_1, 16_1, &
    17_1, 18_1, 19_1, 20_1, 21_1, 22_1, 23_1, 24_1, 25_1,  0_1,  0_1,  0_1,  0_1, &
     0_1,  0_1, 26_1, 27_1, 28_1, 29_1, 30_1, 31_1, 32_1, 33_1, 34_1, 35_1, 36_1, &
    37_1, 38_1, 39_1, 40_1, 41_1, 42_1, 43_1, 44_1, 45_1, 46_1, 47_1, 48_1, 49_1, &
    50_1, 51_1 ]

! Padding constants
  integer(kind=1), parameter :: base64_padding = 0_1
  integer(kind=1), parameter :: base64_cpadding = ichar('=',1)

! Padding map from decoded to encoded
! Number of coded characters as function of characters in decoded block
! A__ --> XX==
! AB_ --> XXX=
! ABC --> XXXX
  integer, dimension(1:3), parameter :: base64_encoded_trailing = [ 2, 3, 4 ]

! Padding map from encoded to decoded
! Number of decoded characters as function of characters in coded block
! XX== --> A__
! XXX= --> AB_
! XXXX --> ABC
  integer, dimension(1:4), parameter :: base64_decoded_trailing = [ 0, 1, 2, 3 ]

! Constructor interface
  interface base64
    module procedure base64_default
    module procedure base64_main
  end interface base64

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
pure function base64_default() result(res)

! The Base64 context
  type(t_base64) :: res

! Initialise the processing structures
  res = base64( .true. )

end function base64_default


! Main constructor
pure function base64_main( padding ) result(res)

! Flag to generate encoding with padding
  logical, intent(in) :: padding

! The Base64 context
  type(t_base64) :: res

! Initialise the processing structures
  call res%initialise( base64_blk_size, base64_cblk_size, &
                       base64_padding, base64_cpadding, &
                       base64_encoded_trailing, base64_decoded_trailing, &
                       padding )

end function base64_main


! Base64 encode one block
pure subroutine base64_encode_block( blk, cblk )

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
  integer(kind=1) :: i6bits

! Assign the resulting bytes from the 6-bit blocks
  i6bits = ishft( iand( blk(1), pattern(1) ), -2 )
  cblk(1) = base64_encode_dictionary(i6bits)
  i6bits = ishft( iand( blk(1), pattern(2) ), 4 ) + &
           ishft( iand( blk(2), pattern(3) ), -4 )
  cblk(2) = base64_encode_dictionary(i6bits)
  i6bits = ishft( iand( blk(2), pattern(4) ), 2 ) + &
           ishft( iand( blk(3), pattern(5) ), -6 )
  cblk(3) = base64_encode_dictionary(i6bits)
  i6bits = iand( blk(3), pattern(6) )
  cblk(4) = base64_encode_dictionary(i6bits)

end subroutine base64_encode_block


! Base64 decode one block
pure subroutine base64_decode_block( cblk, blk )

! The block to decode (4 bytes)
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
  i6bit = base64_decode_dictionary( cblk )

! Assign the resulting bytes from the 6-bit blocks
  blk(1) = ishft( iand( i6bit(1), pattern(1) ), 2 ) + &
           ishft( iand( i6bit(2), pattern(2) ), -4 )
  blk(2) = ishft( iand( i6bit(2), pattern(3) ), 4 ) + &
           ishft( iand( i6bit(3), pattern(4) ), -2 )
  blk(3) = ishft( iand( i6bit(3), pattern(5) ), 6 ) + &
           iand( i6bit(4), pattern(6) )

end subroutine base64_decode_block


! Return length of the trailing part of the data
pure function base64_trail_len( text ) result(res)

! The encoded block (4 bytes)
  integer(kind=1), dimension(:), intent(in) :: text

! The encoded data without padding
  integer :: res

! Local variables
  integer(kind=1), dimension(base64_cblk_size) :: cblk

! Initialise
  cblk = text(size(text)-base64_cblk_size+1:)

! Return the length
  res = count( cblk /= base64_cpadding )

end function base64_trail_len

end module m_base64
