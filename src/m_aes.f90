module m_aes

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Advanced Encryption Standard (AES) implementation
! Reference : National Insitute of Standards and Technology (NIST)
!             Federal Information Processing Standards Publication (FIPS) 197
!             https://csrc.nist.gov/publications/detail/fips/197/final
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

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_aes
  public aes

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Numeric constants
  integer(kind=1), parameter :: z80 = int(z'80',1)
  integer(kind=1), parameter :: z1b = int(z'1b',1)
  integer(kind=1), parameter :: z8d = int(z'8d',1)
  integer(kind=1), parameter :: z00 = int(z'00',1)
  integer(kind=1), parameter :: z01 = int(z'01',1)
  integer(kind=1), parameter :: zff = int(z'ff',1)
  
! AES type
  type, extends(t_block_cipher) :: t_aes
    private

!     Encryption key
      integer(kind=1), dimension(32) :: enckey = z00

!     Decryption key
      integer(kind=1), dimension(32) :: deckey = z00

    contains

!     Block size
      procedure, nopass :: block_size => aes_block_size
      
!     Key size
      procedure, nopass :: key_size => aes_key_size
      
!     Encryption interface
      procedure :: encrypt => aes_encrypt

!     Decryption interface
      procedure :: decrypt => aes_decrypt

  end type t_aes

! The AES block size in bytes
  integer, parameter :: aes_block_size_in_bytes = 16

! The AES key size in bytes
  integer, parameter :: aes_key_size_in_bytes = 32

! The Rijndael look up tables
  integer(kind=1), dimension(-128:127), parameter :: rj_sbox = &
   [ int(z'CD',1), int(z'0C',1), int(z'13',1), int(z'EC',1), int(z'5F',1), int(z'97',1), int(z'44',1), int(z'17',1), &
     int(z'C4',1), int(z'A7',1), int(z'7E',1), int(z'3D',1), int(z'64',1), int(z'5D',1), int(z'19',1), int(z'73',1), &
     int(z'60',1), int(z'81',1), int(z'4F',1), int(z'DC',1), int(z'22',1), int(z'2A',1), int(z'90',1), int(z'88',1), &
     int(z'46',1), int(z'EE',1), int(z'B8',1), int(z'14',1), int(z'DE',1), int(z'5E',1), int(z'0B',1), int(z'DB',1), &
     int(z'E0',1), int(z'32',1), int(z'3A',1), int(z'0A',1), int(z'49',1), int(z'06',1), int(z'24',1), int(z'5C',1), &
     int(z'C2',1), int(z'D3',1), int(z'AC',1), int(z'62',1), int(z'91',1), int(z'95',1), int(z'E4',1), int(z'79',1), &
     int(z'E7',1), int(z'C8',1), int(z'37',1), int(z'6D',1), int(z'8D',1), int(z'D5',1), int(z'4E',1), int(z'A9',1), &
     int(z'6C',1), int(z'56',1), int(z'F4',1), int(z'EA',1), int(z'65',1), int(z'7A',1), int(z'AE',1), int(z'08',1), &
     int(z'BA',1), int(z'78',1), int(z'25',1), int(z'2E',1), int(z'1C',1), int(z'A6',1), int(z'B4',1), int(z'C6',1), &
     int(z'E8',1), int(z'DD',1), int(z'74',1), int(z'1F',1), int(z'4B',1), int(z'BD',1), int(z'8B',1), int(z'8A',1), &
     int(z'70',1), int(z'3E',1), int(z'B5',1), int(z'66',1), int(z'48',1), int(z'03',1), int(z'F6',1), int(z'0E',1), &
     int(z'61',1), int(z'35',1), int(z'57',1), int(z'B9',1), int(z'86',1), int(z'C1',1), int(z'1D',1), int(z'9E',1), &
     int(z'E1',1), int(z'F8',1), int(z'98',1), int(z'11',1), int(z'69',1), int(z'D9',1), int(z'8E',1), int(z'94',1), &
     int(z'9B',1), int(z'1E',1), int(z'87',1), int(z'E9',1), int(z'CE',1), int(z'55',1), int(z'28',1), int(z'DF',1), &
     int(z'8C',1), int(z'A1',1), int(z'89',1), int(z'0D',1), int(z'BF',1), int(z'E6',1), int(z'42',1), int(z'68',1), &
     int(z'41',1), int(z'99',1), int(z'2D',1), int(z'0F',1), int(z'B0',1), int(z'54',1), int(z'BB',1), int(z'16',1), &
     int(z'63',1), int(z'7C',1), int(z'77',1), int(z'7B',1), int(z'F2',1), int(z'6B',1), int(z'6F',1), int(z'C5',1), &
     int(z'30',1), int(z'01',1), int(z'67',1), int(z'2B',1), int(z'FE',1), int(z'D7',1), int(z'AB',1), int(z'76',1), &
     int(z'CA',1), int(z'82',1), int(z'C9',1), int(z'7D',1), int(z'FA',1), int(z'59',1), int(z'47',1), int(z'F0',1), &
     int(z'AD',1), int(z'D4',1), int(z'A2',1), int(z'AF',1), int(z'9C',1), int(z'A4',1), int(z'72',1), int(z'C0',1), &
     int(z'B7',1), int(z'FD',1), int(z'93',1), int(z'26',1), int(z'36',1), int(z'3F',1), int(z'F7',1), int(z'CC',1), &
     int(z'34',1), int(z'A5',1), int(z'E5',1), int(z'F1',1), int(z'71',1), int(z'D8',1), int(z'31',1), int(z'15',1), &
     int(z'04',1), int(z'C7',1), int(z'23',1), int(z'C3',1), int(z'18',1), int(z'96',1), int(z'05',1), int(z'9A',1), &
     int(z'07',1), int(z'12',1), int(z'80',1), int(z'E2',1), int(z'EB',1), int(z'27',1), int(z'B2',1), int(z'75',1), &
     int(z'09',1), int(z'83',1), int(z'2C',1), int(z'1A',1), int(z'1B',1), int(z'6E',1), int(z'5A',1), int(z'A0',1), &
     int(z'52',1), int(z'3B',1), int(z'D6',1), int(z'B3',1), int(z'29',1), int(z'E3',1), int(z'2F',1), int(z'84',1), &
     int(z'53',1), int(z'D1',1), int(z'00',1), int(z'ED',1), int(z'20',1), int(z'FC',1), int(z'B1',1), int(z'5B',1), &
     int(z'6A',1), int(z'CB',1), int(z'BE',1), int(z'39',1), int(z'4A',1), int(z'4C',1), int(z'58',1), int(z'CF',1), &
     int(z'D0',1), int(z'EF',1), int(z'AA',1), int(z'FB',1), int(z'43',1), int(z'4D',1), int(z'33',1), int(z'85',1), &
     int(z'45',1), int(z'F9',1), int(z'02',1), int(z'7F',1), int(z'50',1), int(z'3C',1), int(z'9F',1), int(z'A8',1), &
     int(z'51',1), int(z'A3',1), int(z'40',1), int(z'8F',1), int(z'92',1), int(z'9D',1), int(z'38',1), int(z'F5',1), &
     int(z'BC',1), int(z'B6',1), int(z'DA',1), int(z'21',1), int(z'10',1), int(z'FF',1), int(z'F3',1), int(z'D2',1) ]

  integer(kind=1), dimension(-128:127), parameter :: rj_sbox_inv = &
   [ int(z'3A',1), int(z'91',1), int(z'11',1), int(z'41',1), int(z'4F',1), int(z'67',1), int(z'DC',1), int(z'EA',1), &
     int(z'97',1), int(z'F2',1), int(z'CF',1), int(z'CE',1), int(z'F0',1), int(z'B4',1), int(z'E6',1), int(z'73',1), &
     int(z'96',1), int(z'AC',1), int(z'74',1), int(z'22',1), int(z'E7',1), int(z'AD',1), int(z'35',1), int(z'85',1), &
     int(z'E2',1), int(z'F9',1), int(z'37',1), int(z'E8',1), int(z'1C',1), int(z'75',1), int(z'DF',1), int(z'6E',1), &
     int(z'47',1), int(z'F1',1), int(z'1A',1), int(z'71',1), int(z'1D',1), int(z'29',1), int(z'C5',1), int(z'89',1), &
     int(z'6F',1), int(z'B7',1), int(z'62',1), int(z'0E',1), int(z'AA',1), int(z'18',1), int(z'BE',1), int(z'1B',1), &
     int(z'FC',1), int(z'56',1), int(z'3E',1), int(z'4B',1), int(z'C6',1), int(z'D2',1), int(z'79',1), int(z'20',1), &
     int(z'9A',1), int(z'DB',1), int(z'C0',1), int(z'FE',1), int(z'78',1), int(z'CD',1), int(z'5A',1), int(z'F4',1), &
     int(z'1F',1), int(z'DD',1), int(z'A8',1), int(z'33',1), int(z'88',1), int(z'07',1), int(z'C7',1), int(z'31',1), &
     int(z'B1',1), int(z'12',1), int(z'10',1), int(z'59',1), int(z'27',1), int(z'80',1), int(z'EC',1), int(z'5F',1), &
     int(z'60',1), int(z'51',1), int(z'7F',1), int(z'A9',1), int(z'19',1), int(z'B5',1), int(z'4A',1), int(z'0D',1), &
     int(z'2D',1), int(z'E5',1), int(z'7A',1), int(z'9F',1), int(z'93',1), int(z'C9',1), int(z'9C',1), int(z'EF',1), &
     int(z'A0',1), int(z'E0',1), int(z'3B',1), int(z'4D',1), int(z'AE',1), int(z'2A',1), int(z'F5',1), int(z'B0',1), &
     int(z'C8',1), int(z'EB',1), int(z'BB',1), int(z'3C',1), int(z'83',1), int(z'53',1), int(z'99',1), int(z'61',1), &
     int(z'17',1), int(z'2B',1), int(z'04',1), int(z'7E',1), int(z'BA',1), int(z'77',1), int(z'D6',1), int(z'26',1), &
     int(z'E1',1), int(z'69',1), int(z'14',1), int(z'63',1), int(z'55',1), int(z'21',1), int(z'0C',1), int(z'7D',1), &
     int(z'52',1), int(z'09',1), int(z'6A',1), int(z'D5',1), int(z'30',1), int(z'36',1), int(z'A5',1), int(z'38',1), &
     int(z'BF',1), int(z'40',1), int(z'A3',1), int(z'9E',1), int(z'81',1), int(z'F3',1), int(z'D7',1), int(z'FB',1), &
     int(z'7C',1), int(z'E3',1), int(z'39',1), int(z'82',1), int(z'9B',1), int(z'2F',1), int(z'FF',1), int(z'87',1), &
     int(z'34',1), int(z'8E',1), int(z'43',1), int(z'44',1), int(z'C4',1), int(z'DE',1), int(z'E9',1), int(z'CB',1), &
     int(z'54',1), int(z'7B',1), int(z'94',1), int(z'32',1), int(z'A6',1), int(z'C2',1), int(z'23',1), int(z'3D',1), &
     int(z'EE',1), int(z'4C',1), int(z'95',1), int(z'0B',1), int(z'42',1), int(z'FA',1), int(z'C3',1), int(z'4E',1), &
     int(z'08',1), int(z'2E',1), int(z'A1',1), int(z'66',1), int(z'28',1), int(z'D9',1), int(z'24',1), int(z'B2',1), &
     int(z'76',1), int(z'5B',1), int(z'A2',1), int(z'49',1), int(z'6D',1), int(z'8B',1), int(z'D1',1), int(z'25',1), &
     int(z'72',1), int(z'F8',1), int(z'F6',1), int(z'64',1), int(z'86',1), int(z'68',1), int(z'98',1), int(z'16',1), &
     int(z'D4',1), int(z'A4',1), int(z'5C',1), int(z'CC',1), int(z'5D',1), int(z'65',1), int(z'B6',1), int(z'92',1), &
     int(z'6C',1), int(z'70',1), int(z'48',1), int(z'50',1), int(z'FD',1), int(z'ED',1), int(z'B9',1), int(z'DA',1), &
     int(z'5E',1), int(z'15',1), int(z'46',1), int(z'57',1), int(z'A7',1), int(z'8D',1), int(z'9D',1), int(z'84',1), &
     int(z'90',1), int(z'D8',1), int(z'AB',1), int(z'00',1), int(z'8C',1), int(z'BC',1), int(z'D3',1), int(z'0A',1), &
     int(z'F7',1), int(z'E4',1), int(z'58',1), int(z'05',1), int(z'B8',1), int(z'B3',1), int(z'45',1), int(z'06',1), &
     int(z'D0',1), int(z'2C',1), int(z'1E',1), int(z'8F',1), int(z'CA',1), int(z'3F',1), int(z'0F',1), int(z'02',1), &
     int(z'C1',1), int(z'AF',1), int(z'BD',1), int(z'03',1), int(z'01',1), int(z'13',1), int(z'8A',1), int(z'6B',1) ]
      
! Unit test interface for privte parts
  interface 

    module pure subroutine unit_test_private_aes_sub_bytes( buffer )
      integer(kind=1), dimension(:), intent(inout) :: buffer
    end subroutine unit_test_private_aes_sub_bytes
  
    module pure subroutine unit_test_private_aes_sub_bytes_inv( buffer )
      integer(kind=1), dimension(:), intent(inout) :: buffer
    end subroutine unit_test_private_aes_sub_bytes_inv

    module pure subroutine unit_test_private_aes_add_round_key( buffer, key )
      integer(kind=1), dimension(:), intent(inout) :: buffer
      integer(kind=1), dimension(:), intent(in) :: key
    end subroutine unit_test_private_aes_add_round_key
      
    module pure subroutine unit_test_private_aes_add_round_key_cpy( buffer, key, cpk )
      integer(kind=1), dimension(:), intent(inout) :: buffer
      integer(kind=1), dimension(:), intent(in) :: key
      integer(kind=1), dimension(:), intent(inout) :: cpk
    end subroutine unit_test_private_aes_add_round_key_cpy
      
    module pure subroutine unit_test_private_aes_shift_rows(buffer )
      integer(kind=1), dimension(:), intent(inout) :: buffer
    end subroutine unit_test_private_aes_shift_rows
  
    module pure subroutine unit_test_private_aes_shift_rows_inv(buffer )
      integer(kind=1), dimension(:), intent(inout) :: buffer
    end subroutine unit_test_private_aes_shift_rows_inv

    module pure subroutine unit_test_private_aes_mix_columns( buffer )
      integer(kind=1), dimension(:), intent(inout) :: buffer
    end subroutine unit_test_private_aes_mix_columns
  
    module pure subroutine unit_test_private_aes_mix_columns_inv( buffer )
      integer(kind=1), dimension(:), intent(inout) :: buffer
    end subroutine unit_test_private_aes_mix_columns_inv

    module pure subroutine unit_test_private_aes_expand_encryption_key( key, rc )
      integer(kind=1), dimension(:), intent(inout) :: key
      integer(kind=1), intent(inout) :: rc
    end subroutine unit_test_private_aes_expand_encryption_key
  
    module pure subroutine unit_test_private_aes_expand_decryption_key( key, rc )
      integer(kind=1), dimension(:), intent(inout) :: key
      integer(kind=1), intent(inout) :: rc
    end subroutine unit_test_private_aes_expand_decryption_key
  
  end interface

  public unit_test_private_aes_sub_bytes, unit_test_private_aes_sub_bytes_inv
  public unit_test_private_aes_add_round_key, unit_test_private_aes_add_round_key_cpy
  public unit_test_private_aes_shift_rows, unit_test_private_aes_shift_rows_inv
  public unit_test_private_aes_mix_columns, unit_test_private_aes_mix_columns_inv
  public unit_test_private_aes_expand_encryption_key, unit_test_private_aes_expand_decryption_key
    
!- End of module variable declarations ----------------------------------------

contains

! Constructor from key
pure function aes( key ) result(res)

! The initialisation key
  integer(kind=1), dimension(1:32), intent(in) :: key

! The AES context
  type(t_aes) :: res

! Local variables
  integer :: i
  integer(kind=1) :: rcon

! Initialise
  res%enckey = key
  res%deckey = key
  rcon = z01
  do i = 1, 7
    call aes_expand_encryption_key( res%deckey, rcon )
  end do

end function aes


! AES block size
pure function aes_block_size() result(res)

! AES block size
  integer :: res
   
! Return the AES block size
  res = aes_block_size_in_bytes
   
end function aes_block_size


! AES key size
pure function aes_key_size() result(res)

! AES key size
  integer :: res
   
! Return the AES key size
  res = aes_key_size_in_bytes
   
end function aes_key_size


! AES encryption (from 16 bytes buffer)
pure subroutine aes_encrypt( this, text, cipher )

! The AES context
  class(t_aes), intent(in) :: this

! The plain text to encrypt
  integer(kind=1), dimension(:), intent(in) :: text

! The encrypted text
  integer(kind=1), dimension(size(text)), intent(out) :: cipher

! Local variables
  integer :: i
  integer(kind=1) :: rcon
  integer(kind=1), dimension(16) :: buffer
  integer(kind=1), dimension(32) :: cpkey

! Initialise local buffers
  buffer = text
  cpkey = z00

! Initialise encryption
  call aes_add_round_key_cpy( buffer, this%enckey, cpkey )

! Make the rounds
  rcon = z01
  do i = 1, 13
    call aes_sub_bytes( buffer )
    call aes_shift_rows( buffer )
    call aes_mix_columns( buffer )
    if( iand( int(i,1), z01 ) /= 0 ) then
      call aes_add_round_key( buffer, cpkey(17:) )
    else
      call aes_expand_encryption_key( cpkey, rcon )
      call aes_add_round_key( buffer, cpkey )
    end if
  end do

! Final round (no MixColumns)
  call aes_sub_bytes( buffer )
  call aes_shift_rows( buffer )
  call aes_expand_encryption_key( cpkey, rcon )
  call aes_add_round_key( buffer, cpkey )

! Return the encrypted buffer
  cipher = buffer

end subroutine aes_encrypt


! AES decryption (from 16 bytes buffer)
pure subroutine aes_decrypt( this, cipher, text )

! The AES context
  class(t_aes), intent(in) :: this

! The encrypted text
  integer(kind=1), dimension(:), intent(in) :: cipher

! The plain decrypted text
  integer(kind=1), dimension(size(cipher)), intent(out) :: text

! Local variables
  integer :: i
  integer(kind=1) :: rcon
  integer(kind=1), dimension(16) :: buffer
  integer(kind=1), dimension(32) :: cpkey

! Initialise local buffers
  buffer = cipher
  cpkey = z00

! Initial round
  call aes_add_round_key_cpy( buffer, this%deckey, cpkey )
  call aes_shift_rows_inv( buffer )
  call aes_sub_bytes_inv( buffer )

! Make the rounds
  rcon = z80
  do i = 1, 13
    if( iand( int(i,1), z01 ) /= 0 ) then
      call aes_expand_decryption_key( cpkey, rcon )
      call aes_add_round_key( buffer, cpkey(17:) )
    else
      call aes_add_round_key( buffer, cpkey )
    end if
    call aes_mix_columns_inv( buffer )
    call aes_shift_rows_inv( buffer )
    call aes_sub_bytes_inv( buffer)
  end do

! Finalise decryption
  call aes_add_round_key( buffer, cpkey )

! Return the decryprted buffer
  text = buffer

end subroutine aes_decrypt


! AES SubBytes step
pure subroutine aes_sub_bytes( buffer )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! Processing
  buffer = rj_sbox( buffer )

end subroutine aes_sub_bytes


! AES Subbytes inverse step
pure subroutine aes_sub_bytes_inv( buffer )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! Processing
  buffer = rj_sbox_inv( buffer )

end subroutine aes_sub_bytes_inv


! AES AddRoundKey step
pure subroutine aes_add_round_key( buffer, key )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! The encryption/decryption key
  integer(kind=1), dimension(:), intent(in) :: key

! Processing
  buffer = ieor( buffer, key(:size(buffer)) )

end subroutine aes_add_round_key


! AES AddRoundKey step with key copy
pure subroutine aes_add_round_key_cpy( buffer, key, cpk )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! The encryption/decryption key
  integer(kind=1), dimension(:), intent(in) :: key

! The input key copy
  integer(kind=1), dimension(:), intent(inout) :: cpk

! Processing
  cpk = key
  buffer = ieor( buffer, key(:size(buffer)) )

end subroutine aes_add_round_key_cpy


! AES ShiftRows step
pure subroutine aes_shift_rows( buffer )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! Processing
  buffer( [ 2, 6, 10, 14 ] ) = buffer( [ 6, 10, 14, 2 ] )
  buffer( [ 11, 3 ] ) = buffer( [ 3, 11 ] )
  buffer( [ 4, 16, 12, 8 ] ) = buffer( [ 16, 12, 8, 4 ] )
  buffer( [ 15, 7 ] ) = buffer( [ 7, 15 ] )

end subroutine aes_shift_rows


! AES ShiftRows inverse step
pure subroutine aes_shift_rows_inv( buffer )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! Processing
  buffer( [ 2, 14, 10, 6 ] ) = buffer( [ 14, 10, 6, 2 ] )
  buffer( [ 3, 11 ] ) = buffer( [ 11, 3 ] )
  buffer( [ 4, 8, 12, 16 ] ) = buffer( [ 8, 12, 16, 4 ] )
  buffer( [ 7, 15 ] ) = buffer( [ 15, 7 ] )

end subroutine aes_shift_rows_inv


! AES MixColumns step
pure subroutine aes_mix_columns( buffer )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! Local variables
  integer :: i
  integer(kind=1) :: a, b, c, d, e;


! Processing

  do i = 1, 15, 4

    a = buffer(i)
    b = buffer(i + 1)
    c = buffer(i + 2)
    d = buffer(i + 3)

    e = ieor( c, d )
    e = ieor( b, e )
    e = ieor( a, e )

    buffer(i)   = ieor( buffer(i),   ieor( e, rj_xtime( ieor( a, b ) ) ) )
    buffer(i+1) = ieor( buffer(i+1), ieor( e, rj_xtime( ieor( b, c ) ) ) )
    buffer(i+2) = ieor( buffer(i+2), ieor( e, rj_xtime( ieor( c, d ) ) ) )
    buffer(i+3) = ieor( buffer(i+3), ieor( e, rj_xtime( ieor( d, a ) ) ) )

  end do

end subroutine aes_mix_columns


! AES MixColumns inverse step
pure subroutine aes_mix_columns_inv( buffer )

! Processing buffer
  integer(kind=1), dimension(:), intent(inout) :: buffer

! Local variables
  integer :: i
  integer(kind=1) :: a, b, c, d, e, x, y, z


! Processing

  do i = 1, 15, 4

    a = buffer(i)
    b = buffer(i + 1)
    c = buffer(i + 2)
    d = buffer(i + 3)

    e = ieor( c, d )
    e = ieor( b, e )
    e = ieor( a, e )

    z = rj_xtime(e)
    x = ieor( e, rj_xtime( rj_xtime( ieor( z, ieor( a, c ) ) ) ) )
    y = ieor( e, rj_xtime( rj_xtime( ieor( z, ieor( b, d ) ) ) ) )

    buffer(i)   = ieor( buffer(i),   ieor( x, rj_xtime( ieor( a, b ) ) ) )
    buffer(i+1) = ieor( buffer(i+1), ieor( y, rj_xtime( ieor( b, c ) ) ) )
    buffer(i+2) = ieor( buffer(i+2), ieor( x, rj_xtime( ieor( c, d ) ) ) )
    buffer(i+3) = ieor( buffer(i+3), ieor( y, rj_xtime( ieor( d, a ) ) ) )

  end do

end subroutine aes_mix_columns_inv


! AES expand encryption key
pure subroutine aes_expand_encryption_key( key, rc )

! The decryption key
  integer(kind=1), dimension(:), intent(inout) :: key

! ?????????????
  integer(kind=1), intent(inout) :: rc

! Local variables
  integer :: i


! Processing

  key(1) = ieor( key(1), ieor( rj_sbox(key(30)), rc ) )
  key(2:4) = ieor( key(2:4) , rj_sbox( key( [ 31, 32, 29 ] ) ) )

  rc = F(rc)

  do i = 5, 14, 4
   key(i)   = ieor( key(i),   key(i-4) )
   key(i+1) = ieor( key(i+1), key(i-3) )
   key(i+2) = ieor( key(i+2), key(i-2) )
   key(i+3) = ieor( key(i+3), key(i-1) )
  end do

  key(17:20) = ieor( key(17:20), rj_sbox( key(13:16) ) )

  do i = 21, 32, 4
   key(i)   = ieor( key(i),   key(i-4) )
   key(i+1) = ieor( key(i+1), key(i-3) )
   key(i+2) = ieor( key(i+2), key(i-2) )
   key(i+3) = ieor( key(i+3), key(i-1) )
  end do

end subroutine aes_expand_encryption_key


! AES expand decryption key
pure subroutine aes_expand_decryption_key( key, rc )

! The decryption key
  integer(kind=1), dimension(:), intent(inout) :: key

! ?????????????
  integer(kind=1), intent(inout) :: rc

! Local variables
  integer :: i

! Processing

  do i = 29, 18, -4
   key(i)   = ieor( key(i),   key(i-4) )
   key(i+1) = ieor( key(i+1), key(i-3) )
   key(i+2) = ieor( key(i+2), key(i-2) )
   key(i+3) = ieor( key(i+3), key(i-1) )
  end do

  key(17:20) = ieor( key(17:20), rj_sbox( key(13:16) ) )

  do i = 13, 2, -4
   key(i)   = ieor( key(i),   key(i-4) )
   key(i+1) = ieor( key(i+1), key(i-3) )
   key(i+2) = ieor( key(i+2), key(i-2) )
   key(i+3) = ieor( key(i+3), key(i-1) )
  end do

  rc = FD(rc)

  key(1) = ieor( key(1), ieor( rj_sbox( key(30) ), rc ) )
  key(2:4) = ieor( key(2:4) , rj_sbox( key( [ 31, 32, 29 ] ) ) )

end subroutine aes_expand_decryption_key


! Encryption key expansion support function
elemental function F( x ) result(res)

! The byte to process
  integer(kind=1), intent(in) :: x

! The processed byte
  integer(kind=1) :: res

! Processing
  res = ieor( ishft(x,1), iand( ishft(x,-7), z01 ) * z1b )

end function F


! Decryption key expansion support function
elemental function FD( x ) result(res)

! The byte to process
  integer(kind=1), intent(in) :: x

! The processed byte
  integer(kind=1) :: res

! Processing
  if( iand(x,z01) /= 0 ) then
    res = ieor( ishft(x,-1), z8d )
  else
    res = ieor( ishft(x,-1), z00 )
  end if

end function FD


!  AES MixColumns step support function
elemental function rj_xtime( x ) result(res)

! The byte to process
  integer(kind=1), intent(in) :: x

! The processed byte
  integer(kind=1) :: res

! Processing
  res = ishft( x, 1 )
!  if( iand(x,z80) /= 0 ) then
  if( btest(x,7) ) then
    res = ieor( res, z1b )
  end if

end function rj_xtime

end module m_aes


! Unit test of private methods
submodule (m_aes) unit_test_private_aes

contains

 ! AES SubBytes step
module procedure unit_test_private_aes_sub_bytes
  call aes_sub_bytes( buffer )
end procedure unit_test_private_aes_sub_bytes

! AES Subbytes inverse step
module procedure unit_test_private_aes_sub_bytes_inv
  call aes_sub_bytes_inv( buffer )
end procedure unit_test_private_aes_sub_bytes_inv

! AES AddRoundKey step
module procedure unit_test_private_aes_add_round_key
  call aes_add_round_key( buffer, key )
end procedure unit_test_private_aes_add_round_key

! AES AddRoundKey step with key copy
module procedure unit_test_private_aes_add_round_key_cpy
  call aes_add_round_key_cpy( buffer, key, cpk )
end procedure unit_test_private_aes_add_round_key_cpy

 ! AES ShiftRows step
module procedure unit_test_private_aes_shift_rows
  call aes_shift_rows( buffer )
end procedure unit_test_private_aes_shift_rows

! AES ShiftRows inverse step
module procedure unit_test_private_aes_shift_rows_inv
  call aes_shift_rows_inv( buffer )
end procedure unit_test_private_aes_shift_rows_inv

 ! AES MixColumns step
module procedure unit_test_private_aes_mix_columns
  call aes_mix_columns( buffer )
end procedure unit_test_private_aes_mix_columns

! AES MixColumns inverse step
module procedure unit_test_private_aes_mix_columns_inv
  call aes_mix_columns_inv( buffer )
end procedure unit_test_private_aes_mix_columns_inv

! AES expand encryption key
module procedure unit_test_private_aes_expand_encryption_key
  call aes_expand_encryption_key( key, rc )
end procedure unit_test_private_aes_expand_encryption_key
  
! AES expand decryption key
module procedure unit_test_private_aes_expand_decryption_key
  call aes_expand_decryption_key( key, rc )
end procedure unit_test_private_aes_expand_decryption_key

end submodule unit_test_private_aes
