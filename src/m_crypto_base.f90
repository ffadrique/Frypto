module m_crypto_base

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Encryption/decryption base implementation (for buffer)
!             Implements the encryption modes ECB/CBC/PCBC/CFB/OFB
!             http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation
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

  use m_crypto_padding

  use m_block_cipher
  use m_aes
  use m_triple_des
  use m_blowfish

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_crypto_base
  public crypto_base

  public crypto_base_aes256
  public crypto_base_triple_des
  public crypto_base_blowfish
  
  public crypto_base_mode_ecb
  public crypto_base_mode_cbc
  public crypto_base_mode_pcbc
  public crypto_base_mode_cfb
  public crypto_base_mode_ofb

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Encryption algorithm enumeration
  integer, parameter :: crypto_base_aes256     = 1
  integer, parameter :: crypto_base_triple_des = 2
  integer, parameter :: crypto_base_blowfish   = 3

! Encryption mode eunmeration
  integer, parameter :: crypto_base_mode_ecb  = 0
  integer, parameter :: crypto_base_mode_cbc  = 1
  integer, parameter :: crypto_base_mode_pcbc = 2
  integer, parameter :: crypto_base_mode_cfb  = 3
  integer, parameter :: crypto_base_mode_ofb  = 4

! Number of block to read into buffer at once
  integer, parameter :: crypto_base_blocks_from_buffer = 10

! The encryption type
  type, extends(t_object) :: t_crypto_base
    private

!     The cipher algorithm
      class(t_block_cipher), allocatable :: cipher
    
!     Selected algorithm
      integer :: algorithm = crypto_base_aes256

!     Encryption mode
      integer :: mode = crypto_base_mode_ecb

!     Encryption padding scheme (from enumeration)
      integer :: padding = crypto_padding_pkcs7

!     Encryption initial vector (for chained encryption modes)
      integer(kind=1), dimension(:), allocatable :: inivec

!     Current value of the initial vector (for chained encryption modes)
      integer(kind=1), dimension(:), allocatable :: xblock

    contains

!     Cipher block size
      procedure :: block_size => crypto_base_block_size
      
!     Encryption interface
      procedure :: encrypt => crypto_base_encrypt

!     Decryption interface
      procedure :: decrypt => crypto_base_decrypt

!     Reset the initial vector
      procedure :: reset => crypto_reset
      
!     Assignment
      generic :: assignment(=) => crypto_base_assign
      procedure, private :: crypto_base_assign

  end type t_crypto_base

!- End of module variable declarations ----------------------------------------

contains

! Constuctor from key
pure function crypto_base( algorithm, key, mode, padding, inivec ) result(res)

! The algorithm
  integer, intent(in) :: algorithm

! The initialisation key
  integer(kind=1), dimension(:), intent(in) :: key

! The encryption mode
  integer, optional, intent(in) :: mode

! The encryption padding scheme
  integer, optional, intent(in) :: padding

! The initial vector (when applicable for mode)
  integer(kind=1), optional, dimension(:), intent(in) :: inivec

! The crypto context
  type(t_crypto_base) :: res

! Local variables
  integer :: block_size
  integer :: idx

! Initialise algorithm context
  res%algorithm = algorithm
  select case( algorithm )

!   Advanced Encryption Standard (AES)
    case( crypto_base_aes256 )
      allocate( res%cipher, source=aes( key ) )

!   Triple Data Encryption Standard (3-DES)
    case( crypto_base_triple_des )
      allocate( res%cipher, source=triple_des( key ) )

!   Blowfish
    case( crypto_base_blowfish )
      allocate( res%cipher, source=blowfish( key ) )

!   Default is Advanced Encryption Standard (AES)
    case default
      allocate( res%cipher, source=aes( key ) )

  end select

! Initialise encryption mode
  if( present(mode) ) then
    res%mode = mode
  else
    res%mode = crypto_base_mode_ecb
  end if

! Initialise padding
  if( present(padding) ) then
    res%padding = padding
  else
    res%padding = crypto_padding_pkcs7
  end if

! Initialise the initial vector
  block_size = res%cipher%block_size()
  allocate( res%inivec(block_size), res%xblock(block_size) )
  if( present(inivec) ) then
    idx = min( block_size, size(inivec) )
    res%inivec(:idx) = inivec(:idx)
    res%xblock(:idx) = inivec(:idx)
  else
    res%inivec = 0_1
  end if

end function crypto_base


! Cipher block size
pure function crypto_base_block_size( this ) result(res)

! The crypto context
  class(t_crypto_base), intent(in) :: this

! Cipher block size
  integer :: res
   
! Return the cipher block size
  res = this%cipher%block_size()
   
end function crypto_base_block_size


! Encryption
pure subroutine crypto_base_encrypt( this, text, cipher )

! The crypto context
  class(t_crypto_base), intent(inout) :: this

! The plain text to encrypt
  integer(kind=1), dimension(:), intent(in) :: text

! The encrypted text
  integer(kind=1), dimension(:), allocatable, intent(out) :: cipher

! The block from the plain text to be encrypted
  integer(kind=1), dimension(:), allocatable :: block

! The encrypted block at the end of each step of the mode
  integer(kind=1), dimension(:), allocatable :: cblock

! Local variables
  integer(kind=1), dimension(:), allocatable :: pad
  integer :: bs
  integer :: iblock, nblocks, textlen, cipherlen
  integer :: ib0, ib1
  type(t_crypto_padding) :: padding
  
! Blocking scheme
  bs = this%cipher%block_size()
  allocate( block(bs), cblock(bs) )
  textlen = size(text)
  nblocks = textlen / bs

! Padding scheme
  if( nblocks * bs < textlen ) then
    padding = crypto_padding()
    nblocks = nblocks + 1
    call padding%text( text, this%padding, bs, pad )
  end if

! Allocate encrypted buffer
  cipherlen = nblocks * bs
  allocate( cipher(cipherlen) )

! Loop on all blocks
  do iblock = 1, nblocks

!   Compute block start index
    ib0 = ( iblock - 1 ) * bs + 1

!   Detect last block
    if( iblock /= nblocks ) then

!     Extract block from input buffer
      ib1 = ib0 + bs - 1
      block = text(ib0:ib1)

    else

!     Check if padding required
      if( allocated(pad) ) then

!       Last block with padding
        ib1 = textlen
        block(1:ib1-ib0+1) = text(ib0:ib1)
        block(ib1-ib0+2:) = pad

      else

!       No padding in last block
        ib1 = ib0 + bs - 1
        block = text(ib0:ib1)

      end if

    end if

!   Select the mode
    select case( this%mode )

      case( crypto_base_mode_ecb )
        call this%cipher%encrypt( block, cblock )

      case( crypto_base_mode_cbc )
        this%xblock = ieor( this%xblock, block )
        call this%cipher%encrypt( this%xblock, cblock )
        this%xblock = cblock

      case( crypto_base_mode_pcbc )
        this%xblock = ieor( this%xblock, block )
        call this%cipher%encrypt( this%xblock, cblock )
        this%xblock = ieor( cblock, block )

      case( crypto_base_mode_cfb )
        call this%cipher%encrypt( this%xblock, cblock )
        cblock = ieor( cblock, block )
        this%xblock = cblock

      case( crypto_base_mode_ofb )
        call this%cipher%encrypt( this%xblock, cblock )
        this%xblock = cblock
        cblock = ieor( cblock, block )

    end select

!   Store in encrypted buffer
    cipher(ib0:ib0+bs-1) = cblock

  end do

end subroutine crypto_base_encrypt


! Decryption
pure subroutine crypto_base_decrypt( this, cipher, text, no_padding )

! The crypto context
  class(t_crypto_base), intent(inout) :: this

! The encrypted text
  integer(kind=1), dimension(:), intent(in) :: cipher

! The plain decrypted text
  integer(kind=1), dimension(:), allocatable, intent(out) :: text

! Do not consider padding in the last block
  logical, optional, intent(in) :: no_padding
  
! The block from the plain text to be decrypted
  integer(kind=1), dimension(:), allocatable :: cblock

! The decrypted block at the end of each step of the mode
  integer(kind=1), dimension(:), allocatable :: block

! Local variables
  integer :: bs
  integer :: iblock, nblocks, textlen, cipherlen
  integer :: ib0, ib1
  integer(kind=1), allocatable, dimension(:) :: local, trail
  integer :: ntrail
  logical :: remove_padding
  type(t_crypto_padding) :: padding
  
! Blocking scheme
  bs = this%cipher%block_size()
  allocate( block(bs), cblock(bs) )
  cipherlen = size(cipher)
  nblocks = cipherlen / bs

! Allocate decrypted buffer
  textlen = nblocks * bs
  allocate( local(textlen) )

! Loop on all blocks
  do iblock = 1, nblocks

!   Extract block from input buffer
    ib0 = ( iblock - 1 ) * bs + 1
    ib1 = ib0 + bs - 1
    cblock = cipher(ib0:ib1)

!   Select the mode
    select case( this%mode )

      case( crypto_base_mode_ecb )
        call this%cipher%decrypt( cblock, block )

      case( crypto_base_mode_cbc )
        call this%cipher%decrypt( cblock, block )
        block = ieor( this%xblock, block )
        this%xblock = cblock

      case( crypto_base_mode_pcbc )
        call this%cipher%decrypt( cblock, block )
        block = ieor( this%xblock, block )
        this%xblock = ieor( cblock, block )

      case( crypto_base_mode_cfb )
        call this%cipher%encrypt( this%xblock, block )
        block = ieor( cblock, block )
        this%xblock = cblock

      case( crypto_base_mode_ofb )
        call this%cipher%encrypt( this%xblock, block )
        this%xblock = block
        block = ieor( cblock, block )

    end select

!   Store in decrypted buffer
    local(ib0:ib0+bs-1) = block

  end do

! Process trailing padding
  if( present(no_padding) ) then
    remove_padding = .not. no_padding
  else
    remove_padding = .true.
  end if
  
! Check last block for padding
  if( remove_padding ) then
    padding = crypto_padding()
    call padding%remove( block, trail )
    ntrail = size(trail)
  else
    ntrail = 0
  end if
  
! Process trailing padding
  if( ntrail > 0 .and. ntrail < bs ) then

!   Padding
    textlen = bs * ( nblocks - 1 ) + ntrail
    ib1 = bs * ( nblocks - 1 )
    allocate( text(textlen), source=[ local(:ib1), trail ] )

  else
  
!   No padding
    allocate( text(textlen), source=local )
    
  end if
  
end subroutine crypto_base_decrypt


! Reset the initial vector
elemental subroutine crypto_reset( this )

! The crypto context
  class(t_crypto_base), intent(inout) :: this

! Reset initial vector
  this%xblock = this%inivec
  
end subroutine crypto_reset


! Assignment
elemental subroutine crypto_base_assign( this, other )

! The crypto context
  class(t_crypto_base), intent(inout) :: this

! The other context
  class(t_crypto_base), intent(in) :: other

! Assign elements
  if( allocated(this%cipher) ) deallocate(this%cipher)
  if( allocated(other%cipher) ) then
    allocate( this%cipher, source=other%cipher )
  end if
  this%algorithm = other%algorithm
  this%mode = other%mode
  this%padding = other%padding

  if( allocated(this%inivec) ) deallocate(this%inivec)
  if( allocated(other%inivec) ) then
    allocate( this%inivec, source=other%inivec )
  end if

  if( allocated(this%xblock) ) deallocate(this%xblock)
  if( allocated(other%xblock) ) then
    allocate( this%xblock, source=other%xblock )
  end if

end subroutine crypto_base_assign

end module m_crypto_base
