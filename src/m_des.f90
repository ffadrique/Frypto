module m_des

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Data Encryption Standard (DES) implementation
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

  use m_util_convert
  use m_block_cipher

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_des
  public des

  public des_block_size
  public des_key_Size

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Numeric constants
  integer(kind=1), parameter :: z00 = int(z'00',1)
  integer(kind=1), parameter :: z01 = int(z'01',1)

! Encryption/decryption
  integer(kind=1), parameter :: des_do_encrypt = z00
  integer(kind=1), parameter :: des_do_decrypt = z01

! DES type
  type, extends(t_block_cipher) :: t_des
    private

!     Encryption key
      integer(kind=1), dimension(8) :: key = z00

!     Working area
      integer(kind=1), dimension(48,16) :: Kn = z00

    contains

!     Block size
      procedure, nopass :: block_size => des_block_size
      
!     Key size
      procedure, nopass :: key_size => des_key_size

!     Encryption interface
      procedure :: encrypt => des_encrypt

!     Decryption interface
      procedure :: decrypt => des_decrypt

!     DES encryption (from 64 bits buffer)
      procedure, private :: des_crypt

!     Transform the secret key, so that it is ready for data processing
!     Create the 16 subkeys, K[1] - K[16]
      procedure, private :: des_create_sub_keys

  end type t_des

! The DES block size in bytes
  integer, parameter :: des_block_size_in_bytes = 8

! The DES key size in bytes
  integer, parameter :: des_key_size_in_bytes = 8

! Permutation and translation tables for DES
  integer(kind=1), dimension(56), parameter :: des_pc1 = [ &
       56_1, 48_1, 40_1, 32_1, 24_1, 16_1,  8_1,  0_1, 57_1, 49_1, 41_1, 33_1, 25_1, 17_1, &
        9_1,  1_1, 58_1, 50_1, 42_1, 34_1, 26_1, 18_1, 10_1,  2_1, 59_1, 51_1, 43_1, 35_1, &
       62_1, 54_1, 46_1, 38_1, 30_1, 22_1, 14_1,  6_1, 61_1, 53_1, 45_1, 37_1, 29_1, 21_1, &
       13_1,  5_1, 60_1, 52_1, 44_1, 36_1, 28_1, 20_1, 12_1,  4_1, 27_1, 19_1, 11_1,  3_1 ] + 1_1

! Number left rotations of pc1
  integer(kind=1), dimension(16), parameter :: des_left_rotations = [ &
        1_1, 1_1, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1, 1_1, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1, 1_1 ]

! Permuted choice key (table 2)
  integer(kind=1), dimension(48), parameter :: des_pc2 = [ &
       13_1, 16_1, 10_1, 23_1,  0_1,  4_1,  2_1, 27_1, 14_1,  5_1, 20_1,  9_1, &
       22_1, 18_1, 11_1,  3_1, 25_1,  7_1, 15_1,  6_1, 26_1, 19_1, 12_1,  1_1, &
       40_1, 51_1, 30_1, 36_1, 46_1, 54_1, 29_1, 39_1, 50_1, 44_1, 32_1, 47_1, &
       43_1, 48_1, 38_1, 55_1, 33_1, 52_1, 45_1, 41_1, 49_1, 35_1, 28_1, 31_1 ] + 1_1

! Initial permutation IP
  integer(kind=1), dimension(64), parameter :: des_ip = [ &
       57_1, 49_1, 41_1, 33_1, 25_1, 17_1, 9_1,  1_1, 59_1, 51_1, 43_1, 35_1, 27_1, 19_1, 11_1, 3_1, &
       61_1, 53_1, 45_1, 37_1, 29_1, 21_1, 13_1, 5_1, 63_1, 55_1, 47_1, 39_1, 31_1, 23_1, 15_1, 7_1, &
       56_1, 48_1, 40_1, 32_1, 24_1, 16_1, 8_1,  0_1, 58_1, 50_1, 42_1, 34_1, 26_1, 18_1, 10_1, 2_1, &
       60_1, 52_1, 44_1, 36_1, 28_1, 20_1, 12_1, 4_1, 62_1, 54_1, 46_1, 38_1, 30_1, 22_1, 14_1, 6_1 ] + 1_1

! Expansion table for turning 32 bit blocks into 48 bits
  integer(kind=1), dimension(48), parameter :: des_expansion_table = [ &
       31_1,  0_1,  1_1,  2_1,  3_1,  4_1,  3_1,  4_1,  5_1,  6_1,  7_1,  8_1, &
        7_1,  8_1,  9_1, 10_1, 11_1, 12_1, 11_1, 12_1, 13_1, 14_1, 15_1, 16_1, &
       15_1, 16_1, 17_1, 18_1, 19_1, 20_1, 19_1, 20_1, 21_1, 22_1, 23_1, 24_1, &
       23_1, 24_1, 25_1, 26_1, 27_1, 28_1, 27_1, 28_1, 29_1, 30_1, 31_1,  0_1 ] + 1_1

! The (in)famous S-boxes
  integer(kind=1), dimension(64,8), parameter :: des_sbox = reshape( &
! S1
    [ 14_1,  4_1, 13_1,  1_1,  2_1, 15_1, 11_1,  8_1,  3_1, 10_1,  6_1, 12_1,  5_1,  9_1,  0_1,  7_1, &
       0_1, 15_1,  7_1,  4_1, 14_1,  2_1, 13_1,  1_1, 10_1,  6_1, 12_1, 11_1,  9_1,  5_1,  3_1,  8_1, &
       4_1,  1_1, 14_1,  8_1, 13_1,  6_1,  2_1, 11_1, 15_1, 12_1,  9_1,  7_1,  3_1, 10_1,  5_1,  0_1, &
      15_1, 12_1,  8_1,  2_1,  4_1,  9_1,  1_1,  7_1,  5_1, 11_1,  3_1, 14_1, 10_1,  0_1,  6_1, 13_1, &
! S2
       15_1,  1_1,  8_1, 14_1,  6_1, 11_1,  3_1,  4_1,  9_1,  7_1,  2_1, 13_1, 12_1,  0_1,  5_1, 10_1, &
        3_1, 13_1,  4_1,  7_1, 15_1,  2_1,  8_1, 14_1, 12_1,  0_1,  1_1, 10_1,  6_1,  9_1, 11_1,  5_1, &
        0_1, 14_1,  7_1, 11_1, 10_1,  4_1, 13_1,  1_1,  5_1,  8_1, 12_1,  6_1,  9_1,  3_1,  2_1, 15_1, &
       13_1,  8_1, 10_1,  1_1,  3_1, 15_1,  4_1,  2_1, 11_1,  6_1,  7_1, 12_1,  0_1,  5_1, 14_1,  9_1, &
! S3
       10_1,  0_1,  9_1, 14_1,  6_1,  3_1, 15_1,  5_1,  1_1, 13_1, 12_1,  7_1, 11_1,  4_1,  2_1,  8_1, &
       13_1,  7_1,  0_1,  9_1,  3_1,  4_1,  6_1, 10_1,  2_1,  8_1,  5_1, 14_1, 12_1, 11_1, 15_1,  1_1, &
       13_1,  6_1,  4_1,  9_1,  8_1, 15_1,  3_1,  0_1, 11_1,  1_1,  2_1, 12_1,  5_1, 10_1, 14_1,  7_1, &
        1_1, 10_1, 13_1,  0_1,  6_1,  9_1,  8_1,  7_1,  4_1, 15_1, 14_1,  3_1, 11_1,  5_1,  2_1, 12_1, &
! S4
        7_1, 13_1, 14_1,  3_1,  0_1,  6_1,  9_1, 10_1,  1_1,  2_1,  8_1,  5_1, 11_1, 12_1,  4_1, 15_1, &
       13_1,  8_1, 11_1,  5_1,  6_1, 15_1,  0_1,  3_1,  4_1,  7_1,  2_1, 12_1,  1_1, 10_1, 14_1,  9_1, &
       10_1,  6_1,  9_1,  0_1, 12_1, 11_1,  7_1, 13_1, 15_1,  1_1,  3_1, 14_1,  5_1,  2_1,  8_1,  4_1, &
        3_1, 15_1,  0_1,  6_1, 10_1,  1_1, 13_1,  8_1,  9_1,  4_1,  5_1, 11_1, 12_1,  7_1,  2_1, 14_1, &
! S5
        2_1, 12_1,  4_1,  1_1,  7_1, 10_1, 11_1,  6_1,  8_1,  5_1,  3_1, 15_1, 13_1,  0_1, 14_1,  9_1, &
       14_1, 11_1,  2_1, 12_1,  4_1,  7_1, 13_1,  1_1,  5_1,  0_1, 15_1, 10_1,  3_1,  9_1,  8_1,  6_1, &
        4_1,  2_1,  1_1, 11_1, 10_1, 13_1,  7_1,  8_1, 15_1,  9_1, 12_1,  5_1,  6_1,  3_1,  0_1, 14_1, &
       11_1,  8_1, 12_1,  7_1,  1_1, 14_1,  2_1, 13_1,  6_1, 15_1,  0_1,  9_1, 10_1,  4_1,  5_1,  3_1, &
! S6
       12_1,  1_1, 10_1, 15_1,  9_1,  2_1,  6_1,  8_1,  0_1, 13_1,  3_1,  4_1, 14_1,  7_1,  5_1, 11_1, &
       10_1, 15_1,  4_1,  2_1,  7_1, 12_1,  9_1,  5_1,  6_1,  1_1, 13_1, 14_1,  0_1, 11_1,  3_1,  8_1, &
        9_1, 14_1, 15_1,  5_1,  2_1,  8_1, 12_1,  3_1,  7_1,  0_1,  4_1, 10_1,  1_1, 13_1, 11_1,  6_1, &
        4_1,  3_1,  2_1, 12_1,  9_1,  5_1, 15_1, 10_1, 11_1, 14_1,  1_1,  7_1,  6_1,  0_1,  8_1, 13_1, &
! S7
        4_1, 11_1,  2_1, 14_1, 15_1,  0_1,  8_1, 13_1,  3_1, 12_1,  9_1,  7_1,  5_1, 10_1,  6_1,  1_1, &
       13_1,  0_1, 11_1,  7_1,  4_1,  9_1,  1_1, 10_1, 14_1,  3_1,  5_1, 12_1,  2_1, 15_1,  8_1,  6_1, &
        1_1,  4_1, 11_1, 13_1, 12_1,  3_1,  7_1, 14_1, 10_1, 15_1,  6_1,  8_1,  0_1,  5_1,  9_1,  2_1, &
        6_1, 11_1, 13_1,  8_1,  1_1,  4_1, 10_1,  7_1,  9_1,  5_1,  0_1, 15_1, 14_1,  2_1,  3_1, 12_1, &
! S8
       13_1,  2_1,  8_1,  4_1,  6_1, 15_1, 11_1,  1_1, 10_1,  9_1,  3_1, 14_1,  5_1,  0_1, 12_1,  7_1, &
        1_1, 15_1, 13_1,  8_1, 10_1,  3_1,  7_1,  4_1, 12_1,  5_1,  6_1, 11_1,  0_1, 14_1,  9_1,  2_1, &
        7_1, 11_1,  4_1,  1_1,  9_1, 12_1, 14_1,  2_1,  0_1,  6_1, 10_1, 13_1, 15_1,  3_1,  5_1,  8_1, &
        2_1,  1_1, 14_1,  7_1,  4_1, 10_1,  8_1, 13_1, 15_1, 12_1,  9_1,  0_1,  3_1,  5_1,  6_1, 11_1 ], &

    [ 64, 8 ] )

! 32-bit permutation function P used on the output of the S-boxes
  integer(kind=1), dimension(32), parameter :: des_p = [ &
       15_1,  6_1, 19_1, 20_1, 28_1, 11_1, 27_1, 16_1,  0_1, 14_1, 22_1, 25_1, &
        4_1, 17_1, 30_1,  9_1,  1_1,  7_1, 23_1, 13_1, 31_1, 26_1,  2_1,  8_1, &
       18_1, 12_1, 29_1,  5_1, 21_1, 10_1,  3_1, 24_1 ] + 1_1

! Final permutation IP^-1
  integer(kind=1), dimension(64), parameter :: des_fp = [ &
       39_1,  7_1, 47_1, 15_1, 55_1, 23_1, 63_1, 31_1, 38_1,  6_1, 46_1, 14_1, 54_1, 22_1, 62_1, 30_1, &
       37_1,  5_1, 45_1, 13_1, 53_1, 21_1, 61_1, 29_1, 36_1,  4_1, 44_1, 12_1, 52_1, 20_1, 60_1, 28_1, &
       35_1,  3_1, 43_1, 11_1, 51_1, 19_1, 59_1, 27_1, 34_1,  2_1, 42_1, 10_1, 50_1, 18_1, 58_1, 26_1, &
       33_1,  1_1, 41_1,  9_1, 49_1, 17_1, 57_1, 25_1, 32_1,  0_1, 40_1,  8_1, 48_1, 16_1, 56_1, 24_1 ] + 1_1

       
! Unit test interface for privte parts
  interface 

    module pure subroutine unit_test_private_des_crypt( this, blist, ctype, cipher )
      class(t_des), intent(inout) :: this
      integer(kind=1), dimension(:), intent(in) :: blist
      integer(kind=1), intent(in) :: ctype
      integer(kind=1), dimension(size(blist)), intent(out) :: cipher
    end subroutine unit_test_private_des_crypt
  
    module pure function unit_test_private_des_permutate( table, block ) result(res)
      integer(kind=1), dimension(:), intent(in) :: table
      integer(kind=1), dimension(:), intent(in) :: block
      integer(kind=1), dimension(size(table)) :: res
    end function unit_test_private_des_permutate

    module pure subroutine unit_test_private_des_create_sub_keys( this )
      class(t_des), intent(inout) :: this
    end subroutine unit_test_private_des_create_sub_keys

  end interface
    
  public unit_test_private_des_crypt, unit_test_private_des_permutate, unit_test_private_des_create_sub_keys
  
!- End of module variable declarations ----------------------------------------

contains

! Constuctor from key
pure function des( key ) result(res)

! The initialisation key
  integer(kind=1), dimension(:), intent(in) :: key

! The DES context
  type(t_des) :: res

! Initialise
  res%key = key

! Initialise sub-keys
  call res%des_create_sub_keys()

end function des


! DES block size
pure function des_block_size() result(res)

! DES block size
  integer :: res
   
! Return the DES block size
  res = des_block_size_in_bytes
   
end function des_block_size


! DES key size
pure function des_key_size() result(res)

! DES key size
  integer :: res
   
! Return the DES key size
  res = des_key_size_in_bytes
   
end function des_key_size


! DES encryption (from 8 bytes buffer)
pure subroutine des_encrypt( this, text, cipher )

! The DES context
  class(t_des), intent(in) :: this

! The plain text to encrypt
  integer(kind=1), dimension(:), intent(in) :: text

! The encrypted text
  integer(kind=1), dimension(size(text)), intent(out) :: cipher

! Local variables
  integer(kind=1), dimension(size(text)*8) :: cblist, blist


! Generate the list of bits from the input buffer
  blist = bytes_to_bits_list( text )

! Encrypt
  call this%des_crypt( blist, des_do_encrypt, cblist )

! Convert the list of bits to the output buffer
  cipher = bits_list_to_bytes( cblist )

end subroutine des_encrypt


! DES decryption (from 8 bytes buffer)
pure subroutine des_decrypt( this, cipher, text )

! The DES context
  class(t_des), intent(in) :: this

! The encrypted text
  integer(kind=1), dimension(:), intent(in) :: cipher

! The plain decrypted text
  integer(kind=1), dimension(size(cipher)), intent(out) :: text

! Local variables
  integer(kind=1), dimension(size(cipher)*8) :: cblist, blist

! Generate the list of bits from the input buffer
  cblist = bytes_to_bits_list( cipher )

! Decrypt
  call this%des_crypt( cblist, des_do_decrypt, blist )

! Convert the list of bits to the output buffer
  text = bits_list_to_bytes( blist )

end subroutine des_decrypt


! DES encryption (from 64 bits buffer)
pure subroutine des_crypt( this, blist, ctype, cipher )

! The DES context
  class(t_des), intent(in) :: this

! The plain text to encrypt
  integer(kind=1), dimension(:), intent(in) :: blist

! The encryption/decryption flag
  integer(kind=1), intent(in) :: ctype

! The encrypted text
  integer(kind=1), dimension(size(blist)), intent(out) :: cipher

! Local variables
  integer(kind=1), dimension(size(des_ip)) :: block
  integer(kind=1), dimension(32) :: L, R
  integer(kind=1), dimension(48) :: RR
  integer :: i, j
  integer :: it, dit, pos
  integer(kind=1), dimension(32) :: tmpR
  integer(kind=1), dimension(6,8) :: B
  integer(kind=1), dimension(32) :: Bn
  integer(kind=1) :: m, n, v

! Generate permutation
  block = des_permutate( des_ip, blist )

! Split into left and right sections
  L = block(:32)
  R = block(33:)

! Encryption starts from Kn[1] through to Kn[16]
  if( ctype == des_do_encrypt ) then
    it = 1
    dit = 1
  else
    it = 16
    dit = -1
  end if

! Main loop
  do i = 1, 16

!   Make a copy of R[i-1], this will later become L[i]
    tmpR = R

!   Permutate R[i-1] to start creating R[i]
    RR(:32) = R
    RR(33:) = z00
    RR = des_permutate( des_expansion_table, RR )

!   Exclusive or R[i-1] with K[i], create B[1] to B[8] whilst here
    where( RR /= this%Kn(:,it) )
      RR = z01
    elsewhere
      RR = z00
    end where
    B = reshape( RR, [ 6, 8 ] )

!   Permutate B[1] to B[8] using the S-Boxes
    pos = 1
    do j = 1, 8

!     Work out the offsets
      m = ishft( B(1,j), 1 ) + B(6,j)
      n = ishft( B(2,j), 3 ) + ishft( B(3,j), 2 ) + ishft( B(4,j), 1 ) + B(5,j)

!     Find the permutation value
      v = des_sbox( ishft(m,4) + n + 1, j )

!     Turn value into bits, add it to result: Bn
      Bn(pos:pos+3) = ishft( iand( v, [ 8_1, 4_1, 2_1, 1_1 ] ), [ -3, -2, -1, 0 ] )

!     Iterate position in Bn
      pos = pos + 4

    end do

!   Permutate the concatination of B[1] to B[8] (Bn)
    R = des_permutate( des_p, Bn )

!   Exclusive or with L[i-1]
    where( R /= L )
      R = z01
    elsewhere
      R = z00
    end where

!   L[i] becomes R[i-1]
    L = tmpR

!   Iterate
    it = it + dit

  end do

! Final permutation of R[16]L[16]
  cipher = des_permutate( des_fp, [ R, L ] )

end subroutine des_crypt


! Permutate this block with the specified table
pure function des_permutate( table, block ) result(res)

! The table to generate the permutation
  integer(kind=1), dimension(:), intent(in) :: table

! The block to permutate
  integer(kind=1), dimension(:), intent(in) :: block

! The permutated block
  integer(kind=1), dimension(size(table)) :: res

! Generate the result
  res = block( [ table ] )

end function des_permutate


! Transform the secret key, so that it is ready for data processing
! Create the 16 subkeys, K[1] - K[16]
pure subroutine des_create_sub_keys( this )

! The DES context
  class(t_des), intent(inout) :: this

! Local variables
  integer(kind=1), dimension(size(des_pc1)) :: key
  integer(kind=1), dimension(size(des_pc1)/2) :: L, R
  integer :: i

! Generate the key permutation
  key = des_permutate( des_pc1, bytes_to_bits_list( this%key ) )

! Split into left and right sections
  L = key(:28)
  R = key(29:)

! Create the 16 subkeys K[1] to K[16] from the given key
  do i = 1, 16

!   Perform circular left shifts
    L = cshift( L, des_left_rotations(i) )
    R = cshift( R, des_left_rotations(i) )

!   Create one of the 16 subkeys through pc2 permutation
    this%Kn(:,i) = des_permutate( des_pc2, [ L, R ] )

  end do

end subroutine des_create_sub_keys

end module m_des


! Unit test of private methods
submodule (m_des) unit_test_private_des

contains

! Transform the secret key, so that it is ready for data processing
! Create the 16 subkeys, K[1] - K[16]
module procedure unit_test_private_des_create_sub_keys
  call this%des_create_sub_keys()
end procedure unit_test_private_des_create_sub_keys

! Permutate this block with the specified table
module procedure unit_test_private_des_permutate
  res = des_permutate( table, block )
end procedure unit_test_private_des_permutate

! DES encryption (from 64 bits buffer)
module procedure unit_test_private_des_crypt
  call this%des_crypt( blist, ctype, cipher )
end procedure unit_test_private_des_crypt

end submodule unit_test_private_des
