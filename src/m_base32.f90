module m_base32

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Base32 encoding
!
! Algorithm
!  - Divide the input bytes stream into blocks of 5 bytes.
!  - Divid3 40 bits of each 5-byte block into 8 groups of 5 bits.
!  - Map each group of 5 bits to 1 printable character, based on the 5-bit value
!    using the Base32 character set map.
!  - If the last 5-byte block has only 1 byte of input data, pad 4 bytes of zero
!    (\x0000). After encoding it as a normal block, override the last 6 characters
!    with 6 equal signs (======).
!  - If the last 5-byte block has only 2 bytes of input data, pad 3 bytes of zero
!    (\x0000). After encoding it as a normal block, override the last 4 characters
!    with 4 equal signs (====).
!  - If the last 5-byte block has only 3 bytes of input data, pad 2 bytes of zero
!    (\x0000). After encoding it as a normal block, override the last 3 characters
!    with 3 equal signs (===).
!  - If the last 5-byte block has only 4 bytes of input data, pad 1 byte of zero
!    (\x0000). After encoding it as a normal block, override the last 1 characters
!    with 1 equal sign (=).
!
!    Not yet imlemented:
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
  public t_base32
  public base32

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Base32 encoding type
  type, extends(t_ascii_encoder) :: t_base32
    private

    contains

!     Block encoding
      procedure, nopass :: encode_block => base32_encode_block

!     Block decoding
      procedure, nopass :: decode_block => base32_decode_block

!     Length of the trailing part of the data
      procedure, nopass :: trail_len => base32_trail_len

  end type t_base32

! Decoded block size
  integer, parameter :: base32_blk_size = 5

! Encoded block size
  integer, parameter :: base32_cblk_size = 8

! Encoding lookup table (character representation)
  character, dimension(0:31), parameter :: base32_encode_chars = [ &
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', &
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', &
    '2', '3', '4', '5', '6', '7' ]

! Encoding lookup table (byte representation)
  integer(kind=1), dimension(0:31), parameter :: base32_encode_dictionary = ichar(base32_encode_chars,1)

! Decoding lookup table (byte representation)
  integer(kind=1), dimension(50:90), parameter :: base32_decode_dictionary = [ &
    26_1, 27_1, 28_1, 29_1, 30_1, 31_1,  0_1,  0_1,  0_1,  0_1,  0_1, &
     0_1,  0_1,  0_1,  0_1,  0_1,  1_1,  2_1,  3_1,  4_1,  5_1,  6_1, &
     7_1,  8_1,  9_1, 10_1, 11_1, 12_1, 13_1, 14_1, 15_1, 16_1, 17_1, &
    18_1, 19_1, 20_1, 21_1, 22_1, 23_1, 24_1, 25_1 ]

! Padding constants
  integer(kind=1), parameter :: base32_padding = 0_1
  integer(kind=1), parameter :: base32_cpadding = ichar('=',1)

! Padding map from decoded to encoded
! Number of coded characters as function of characters in decoded block
! A____ --> XX======
! AB___ --> XXXX====
! ABC__ --> XXXXX===
! ABCD_ --> XXXXXXX=
! ABCDE --> XXXXXXXX
  integer, dimension(1:5), parameter :: base32_encoded_trailing = [ 2, 4, 5, 7, 8 ]

! Padding map from encoded to decoded
! Number of decoded characters as function of characters in coded block
! XX====== --> A____
! XXXX==== --> AB___
! XXXXX=== --> ABC__
! XXXXXXX= --> ABCD_
! XXXXXXXX --> ABCDE
  integer, dimension(1:8), parameter :: base32_decoded_trailing = [ 0, 1, 0, 2, 3, 0, 4, 5 ]

! Constructor interface
  interface base32
    module procedure base32_default
    module procedure base32_main
  end interface base32

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
pure function base32_default() result(res)

! The base32 context
  type(t_base32) :: res

! Initialise the processing structures
  res = base32( .true. )

end function base32_default


! Main constructor
pure function base32_main( padding ) result(res)

! Flag to generate encoding with padding
  logical, intent(in) :: padding

! The base32 context
  type(t_base32) :: res

! Initialise the processing structures
  call res%initialise( base32_blk_size, base32_cblk_size, &
                       base32_padding, base32_cpadding, &
                       base32_encoded_trailing, base32_decoded_trailing, &
                       padding )

end function base32_main


! Base32 encode one block
pure subroutine base32_encode_block( blk, cblk )

! The block to encode (5 bytes)
  integer(kind=1), dimension(:), intent(in) :: blk

! The encoded block (8 bytes)
  integer(kind=1), dimension(:), intent(out) :: cblk

! Patterns
!        1         2         2         3         4         4         5         5         6         7         7         8  res
!        1         1         2         2         2         3         3         4         4         4         5         5  blk
!        1         2         3         4         5         6         7         8         9        10        11        12  pattern
! 11111000  00000111  11000000  00111110  00000001  11110000  00001111  10000000  01111100  00000011  11100000  00011111
!    z'F8'     z'07'     z'C0'     z'3E'     z'01'     z'F0'     z'0F'     z'80'     z'7C'     z'03'     z'E0'     z'1F'
! gfortran does not accept -128_1, therefore it is initialise as kind=2 and then converted to kind=1 
  integer(kind=2), dimension(12) , parameter :: pattern2 = [ &
      -8_2, 7_2, -64_2, 62_2, 1_2, -16_2, 15_2, -128_2, 124_2, 3_2, -32_2, 31_2 ]
  integer(kind=1), dimension(12) :: pattern

! Local variables
  integer(kind=1) :: i5bits

! Initialise
  pattern = int( pattern2, 1 )

! Assign the resulting bytes from the 6-bit blocks
  i5bits = ishft( iand( blk(1), pattern(1) ), -3 )
  cblk(1) = base32_encode_dictionary(i5bits)
  i5bits = ishft( iand( blk(1), pattern(2) ), 2 ) + &
           ishft( iand( blk(2), pattern(3) ), -6 )
  cblk(2) = base32_encode_dictionary(i5bits)
  i5bits = ishft( iand( blk(2), pattern(4) ), -1 )
  cblk(3) = base32_encode_dictionary(i5bits)
  i5bits = ishft( iand( blk(2), pattern(5) ), 4 ) + &
           ishft( iand( blk(3), pattern(6) ), -4 )
  cblk(4) = base32_encode_dictionary(i5bits)
  i5bits = ishft( iand( blk(3), pattern(7) ), 1 ) + &
           ishft( iand( blk(4), pattern(8) ), -7 )
  cblk(5) = base32_encode_dictionary(i5bits)
  i5bits = ishft( iand( blk(4), pattern(9) ), -2 )
  cblk(6) = base32_encode_dictionary(i5bits)
  i5bits = ishft( iand( blk(4), pattern(10) ), 3 ) + &
           ishft( iand( blk(5), pattern(11) ), -5 )
  cblk(7) = base32_encode_dictionary(i5bits)
  i5bits = iand( blk(5), pattern(12) )
  cblk(8) = base32_encode_dictionary(i5bits)

end subroutine base32_encode_block


! Base32 encode one block
pure subroutine base32_decode_block( cblk, blk )

! The block to decode (8 bytes)
  integer(kind=1), dimension(:), intent(in) :: cblk

! The decoded block (5 bytes)
  integer(kind=1), dimension(:), intent(out) :: blk

! Patterns
!        1         1         2         2         2         3         3         4         4         4         5         5  res
!        1         2         2         3         4         4         5         5         6         7         7         8  blk
!        1         2         3         4         5         6         7         8         9        10        11        12  pattern
! 00011111  00011100  00000011  00011111  00010000  00001111  00011110  00000001  00011111  00011000  00000111  00011111
!    z'1F'     z'1C'     z'03'     z'1F'     z'10'     z'0F'     z'1E'     z'01'     z'1F'     z'18'     z'07'     z'1F'
  integer(kind=1), dimension(12) , parameter :: pattern = [ &
       31_1, 28_1, 3_1, 31_1, 16_1, 15_1, 30_1, 1_1, 31_1, 24_1, 7_1, 31_1 ]
  integer(kind=1), dimension(8) :: i5bit

! Decode the 5-bit representation
  i5bit = base32_decode_dictionary( cblk )

! Assign the resulting bytes from the 6-bit blocks
  blk(1) = ishft( iand( i5bit(1), pattern(1) ), 3_1 ) + &
           ishft( iand( i5bit(2), pattern(2) ), -2_1 )
  blk(2) = ishft( iand( i5bit(2), pattern(3) ), 6_1 ) + &
           ishft( iand( i5bit(3), pattern(4) ), 1_1 ) + &
           ishft( iand( i5bit(4), pattern(5) ), -4_1 )
  blk(3) = ishft( iand( i5bit(4), pattern(6) ), 4_1 ) + &
           ishft( iand( i5bit(5), pattern(7) ), -1_1 )
  blk(4) = ishft( iand( i5bit(5), pattern(8) ), 7_1 ) + &
           ishft( iand( i5bit(6), pattern(9) ), 2_1 ) + &
           ishft( iand( i5bit(7), pattern(10) ), -3_1 )
  blk(5) = ishft( iand( i5bit(7), pattern(11) ), 5_1 ) + &
           iand( i5bit(8), pattern(12) )

end subroutine base32_decode_block


! Return length of the trailing part of the data
pure function base32_trail_len( text ) result(res)

! The encoded block (4 bytes)
  integer(kind=1), dimension(:), intent(in) :: text

! The encoded data without padding
  integer :: res

! Local variables
  integer(kind=1), dimension(base32_cblk_size) :: cblk

! Initialise
  cblk = text(size(text)-base32_cblk_size+1:)

! Return the length
  res = count( cblk /= base32_cpadding )

end function base32_trail_len

end module m_base32
