module m_ascii_encoder

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Abstract class for acii encoding (uuencode, base64, base32)
!             Provides logic for the processing of the input and relies on
!             derived class to implement the encoding of the blocks according
!             to the specific algotihm (i.e. uuendoce, base32, base64, ...)
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
  public t_ascii_encoder

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! ASCII encoder type
  type, abstract, extends(t_object) :: t_ascii_encoder
    private

!     Decoded block size
      integer :: blk = 0

!     Coded block size
      integer :: cblk = 0

!     Padding character for codding proces
      integer(kind=1) :: padding_byte = 0_1

!     Padding character in the coded array
      integer(kind=1) :: padding_cbyte = 0_1

!     Map between traling bytes in the decoded and coded representations
!     Coded characters as function of the decoded characters
      integer, dimension(:), allocatable :: coded_trailing

!     Map between traling bytes in the coded and decoded representations
!     Decoded characters as function of the coded characters
      integer, dimension(:), allocatable :: decoded_trailing

!     Flag to generate the encoding with padding (for encoding only)
      logical :: padding = .true.

    contains

!     Class initialisation
      procedure :: initialise => ascii_encoder_initialise

!     Encryption interface
      procedure :: encode => ascii_encoder_encode

!     Decryption interface
      procedure :: decode => ascii_encoder_decode

!     Encryption high level logic
      procedure, private :: encode_core => ascii_encoder_encode_core

!     Decryption high level logic
      procedure, private :: decode_core => ascii_encoder_decode_core

!     Block encoding interface
      procedure(i_encoder), deferred, nopass :: encode_block

!     Block decoding interface
      procedure(i_decoder), deferred, nopass :: decode_block

!     Length of the trailing part of the data interface
      procedure(i_trailer), deferred, nopass :: trail_len

  end type t_ascii_encoder

! Encoder interfaces
  interface
    pure subroutine i_encoder( blk, cblk )
      integer(kind=1), dimension(:), intent(in) :: blk
      integer(kind=1), dimension(:), intent(out) :: cblk
    end subroutine i_encoder
  end interface

! Decoder interfaces
  interface
    pure subroutine i_decoder( cblk, blk )
      integer(kind=1), dimension(:), intent(in) :: cblk
      integer(kind=1), dimension(:), intent(out) :: blk
    end subroutine i_decoder
  end interface

! Stripper interface
  interface
    pure function i_trailer( text ) result(res)
      integer(kind=1), dimension(:), intent(in) :: text
      integer :: res
    end function i_trailer
  end interface

!- End of module variable declarations ----------------------------------------

contains

! Main constructor
pure subroutine ascii_encoder_initialise( this, &
                                          block_size, cblock_size, &
                                          padding_byte, padding_cbyte, &
                                          coded_trailing, decoded_trailing, &
                                          padding )

! The encoder context
  class(t_ascii_encoder), intent(inout) :: this

! Decoded block size
  integer, intent(in) :: block_size

! Coded block size
  integer, intent(in) :: cblock_size

! Padding character for codding proces
  integer(kind=1), intent(in) :: padding_byte

! Padding character in the coded array
  integer(kind=1), intent(in) :: padding_cbyte

! Map between trailing bytes in the decoded and coded representations
  integer, dimension(:), intent(in) :: coded_trailing

! Map between trailing bytes in the coded and decoded representations
  integer, dimension(:), intent(in):: decoded_trailing

! Flag to generate encoding with padding
  logical, intent(in) :: padding

! Set block sizes
  this%blk = block_size
  this%cblk = cblock_size

! Set padding bytes
  this%padding_byte = padding_byte
  this%padding_cbyte = padding_cbyte

! Set the trailing maps
  allocate( this%coded_trailing, source=coded_trailing )
  allocate( this%decoded_trailing, source=decoded_trailing )

! Set padding flag
  this%padding = padding

end subroutine ascii_encoder_initialise


! Encoding from byte buffer
pure subroutine ascii_encoder_encode( this, text, ctext )

! The encoder context
  class(t_ascii_encoder), intent(in) :: this

! The plain text to encode
  integer(kind=1), dimension(:), intent(in) :: text

! The encoded text
  integer(kind=1), dimension(:), allocatable, intent(out) :: ctext

! Local variables
  integer :: n, ipad

! Calculate the base size of the output buffer
  n = max( this%cblk, this%cblk * ( ( size(text) - 1 ) / this%blk + 1 ) )

! Consider padding
  if( .not. this%padding ) then

!   Compute the number of characters in the last block
    ipad = mod( size(text), this%blk )
    if( ipad /= 0 ) then

!     There are characters in the last block; remove the padding trailing characters
      n = n - ( this%cblk - this%coded_trailing(ipad) )

    end if

  end if

! Allocate the output buffer
  allocate( ctext(n) )

! Call the encoding subroutine
  call this%encode_core( text, ctext )

end subroutine ascii_encoder_encode


! Encoding from byte buffer (high level logic)
pure subroutine ascii_encoder_encode_core( this, text, ctext )

! The encoder context
  class(t_ascii_encoder), intent(in) :: this

! The plain text to encode
  integer(kind=1), dimension(:), intent(in) :: text

! The encoded text
  integer(kind=1), dimension(:), intent(out) :: ctext

! Local variables
  logical :: last
  integer :: llast, nlast
  integer :: iblk, nblk
  integer :: offset, coffset
  integer(kind=1), dimension(this%blk) :: buffer
  integer(kind=1), dimension(this%cblk) :: cbuffer

! Initialise
  offset = 0
  coffset = 0

! Compute the number of blocks
  nblk = size(text) / this%blk

! Check if there one more block that is not complete (padding)
  last = ( mod( size(text), this%blk ) /= 0 )

! Loop on the input blocks
  do iblk = 1, nblk

!   Compute offsets
    offset = this%blk * ( iblk - 1 ) + 1
    coffset = this%cblk * ( iblk - 1 ) + 1

!   Process this block
    buffer = text(offset:offset+this%blk-1)
    call this%encode_block( buffer, cbuffer )
    ctext(coffset:coffset+this%cblk-1) = cbuffer

  end do

! Process the last incomplete block if present
  if( last ) then

!   Compute offsets for the last block
    offset = this%blk * nblk + 1
    coffset = this%cblk * nblk + 1

!   Compute the length of the last block
    llast = size(text(offset:))

!   Pad buffer with zeros before encoding
    buffer(1:llast) = text(offset:)
    buffer(llast+1:this%blk) = this%padding_byte

!   Encode the last block
    call this%encode_block( buffer, cbuffer )

!   Get the number of encoded characters in the last block
    nlast = this%coded_trailing( llast )

!   Check if padding is to be added
    if( this%padding ) then

!     Pad coded buffer
      cbuffer(nlast+1:) = this%padding_cbyte

!     Add encoded buffer with padding
      ctext(coffset:) = cbuffer

    else

!     Add encoded buffer without padding
      ctext(coffset:) = cbuffer(:nlast)

    end if

  end if

end subroutine ascii_encoder_encode_core


! Decoding from byte buffer
pure subroutine ascii_encoder_decode( this, ctext, text )

! The encoder context
  class(t_ascii_encoder), intent(inout) :: this

! The encoded text
  integer(kind=1), dimension(:), intent(in) :: ctext

! The plain decoded text
  integer(kind=1), dimension(:), allocatable, intent(out) :: text

! Local variables
  integer :: length
  integer :: n, ipad

! Initialise
  length = size(ctext)

! Calculate the reference length of the output buffer assuming padding to block-size-byte
  n = size(ctext) / this%cblk * this%blk

! Check if padding is present, i.e. no alignment to block-size-byte means no padding
  ipad = mod( length, this%cblk )
  if( ipad /= 0 ) then

!   Then increase the output buffer length by the extra bytes minus one
    n = n + this%decoded_trailing( ipad )

  else

!   Remove the corresponding size from the output
    ipad = this%trail_len(ctext)
    n = n - ( this%blk - this%decoded_trailing( ipad ) )

  end if

! Allocate the output buffer
  allocate( text(n) )

! Call the recursive subroutine
  call this%decode_core( ctext, text )

end subroutine ascii_encoder_decode


! Decoding from byte buffer (high level logic)
pure subroutine ascii_encoder_decode_core( this, ctext, text )

! The encoder context
  class(t_ascii_encoder), intent(in) :: this

! The encoded text
  integer(kind=1), dimension(:), intent(in) :: ctext

! The plain decoded text
  integer(kind=1), dimension(:), intent(out) :: text

! Local variables
  logical :: last
  integer :: llast, nlast
  integer :: iblk, nblk
  integer :: offset, coffset
  integer(kind=1), dimension(this%blk) :: buffer
  integer(kind=1), dimension(this%cblk) :: cbuffer

! Compute the number of input blocks
  nblk = size(ctext) / this%cblk

! Check if there one more block that is not complete (no padding)
  last = ( mod( size(ctext), this%cblk ) /= 0 )
  if( last ) nblk = nblk + 1

! Loop on the blocks (but last one)
  do iblk = 1, nblk - 1

!   Compute offsets
    offset = this%blk * ( iblk - 1 ) + 1
    coffset = this%cblk * ( iblk - 1 ) + 1

!   Process this block
    cbuffer = ctext(coffset:coffset+this%cblk-1)
    call this%decode_block( cbuffer, buffer )
    text(offset:offset+this%blk-1) = buffer

  end do

! Compute offsets for the last block
  offset = this%blk * ( nblk - 1 ) + 1
  coffset = this%cblk * ( nblk - 1 ) + 1

! Process the last incomplete block if present
  if( last ) then

!   Compute the length of the last block
    llast = size(ctext(coffset:))

!   Pad the last block
    cbuffer(:llast) = ctext(coffset:coffset+llast-1)
    cbuffer(llast+1:) = this%padding_cbyte

  else

!   Compute the length of the last block
    llast = this%trail_len(ctext(coffset:))

!   The last block is already padded
    cbuffer = ctext(coffset:)

  end if

! Decode the last block
  call this%decode_block( cbuffer, buffer )
  nlast = this%decoded_trailing( llast )
  text(offset:) = buffer(:nlast)

end subroutine ascii_encoder_decode_core

end module m_ascii_encoder