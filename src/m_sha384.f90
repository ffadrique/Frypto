module m_sha384

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : FIPS 180-2, Secure Hash Standard
!             http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
! Synopsis  : Cryptographic Secure Hash Algorithm (SHA-512)
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

  use m_hash64

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_sha384
  public sha384

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Initial hash value
  integer(kind=8), dimension(8), parameter :: H0 = &
   [ int(z'CBBB9D5DC1059ED8',8), int(z'629A292A367CD507',8), int(z'9159015A3070DD17',8), int(z'152FECD8F70E5939',8), &
     int(z'67332667FFC00B31',8), int(z'8EB44A8768581511',8), int(z'DB0C2E0D64F98FA7',8), int(z'47B5481DBEFA4FA4',8) ]

! SHA-384 processing constants
  integer(kind=8), parameter, dimension(80) :: K = &
   [ int(z'428A2F98D728AE22',8), int(z'7137449123EF65CD',8), int(z'B5C0FBCFEC4D3B2F',8), int(z'E9B5DBA58189DBBC',8), &
     int(z'3956C25BF348B538',8), int(z'59F111F1B605D019',8), int(z'923F82A4AF194F9B',8), int(z'AB1C5ED5DA6D8118',8), &
     int(z'D807AA98A3030242',8), int(z'12835B0145706FBE',8), int(z'243185BE4EE4B28C',8), int(z'550C7DC3D5FFB4E2',8), &
     int(z'72BE5D74F27B896F',8), int(z'80DEB1FE3B1696B1',8), int(z'9BDC06A725C71235',8), int(z'C19BF174CF692694',8), &
     int(z'E49B69C19EF14AD2',8), int(z'EFBE4786384F25E3',8), int(z'0FC19DC68B8CD5B5',8), int(z'240CA1CC77AC9C65',8), &
     int(z'2DE92C6F592B0275',8), int(z'4A7484AA6EA6E483',8), int(z'5CB0A9DCBD41FBD4',8), int(z'76F988DA831153B5',8), &
     int(z'983E5152EE66DFAB',8), int(z'A831C66D2DB43210',8), int(z'B00327C898FB213F',8), int(z'BF597FC7BEEF0EE4',8), &
     int(z'C6E00BF33DA88FC2',8), int(z'D5A79147930AA725',8), int(z'06CA6351E003826F',8), int(z'142929670A0E6E70',8), &
     int(z'27B70A8546D22FFC',8), int(z'2E1B21385C26C926',8), int(z'4D2C6DFC5AC42AED',8), int(z'53380D139D95B3DF',8), &
     int(z'650A73548BAF63DE',8), int(z'766A0ABB3C77B2A8',8), int(z'81C2C92E47EDAEE6',8), int(z'92722C851482353B',8), &
     int(z'A2BFE8A14CF10364',8), int(z'A81A664BBC423001',8), int(z'C24B8B70D0F89791',8), int(z'C76C51A30654BE30',8), &
     int(z'D192E819D6EF5218',8), int(z'D69906245565A910',8), int(z'F40E35855771202A',8), int(z'106AA07032BBD1B8',8), &
     int(z'19A4C116B8D2D0C8',8), int(z'1E376C085141AB53',8), int(z'2748774CDF8EEB99',8), int(z'34B0BCB5E19B48A8',8), &
     int(z'391C0CB3C5C95A63',8), int(z'4ED8AA4AE3418ACB',8), int(z'5B9CCA4F7763E373',8), int(z'682E6FF3D6B2B8A3',8), &
     int(z'748F82EE5DEFB2FC',8), int(z'78A5636F43172F60',8), int(z'84C87814A1F0AB72',8), int(z'8CC702081A6439EC',8), &
     int(z'90BEFFFA23631E28',8), int(z'A4506CEBDE82BDE9',8), int(z'BEF9A3F7B2C67915',8), int(z'C67178F2E372532B',8), &
     int(z'CA273ECEEA26619C',8), int(z'D186B8C721C0C207',8), int(z'EADA7DD6CDE0EB1E',8), int(z'F57D4F7FEE6ED178',8), &
     int(z'06F067AA72176FBA',8), int(z'0A637DC5A2C898A6',8), int(z'113F9804BEF90DAE',8), int(z'1B710B35131C471B',8), &
     int(z'28DB77F523047D84',8), int(z'32CAAB7B40C72493',8), int(z'3C9EBE0A15C9BEBC',8), int(z'431D67C49C100D4C',8), &
     int(z'4CC5D4BECB3E42B6',8), int(z'597F299CFC657E2A',8), int(z'5FCB6FAB3AD6FAEC',8), int(z'6C44198C4A475817',8) ]

! Size of the message block (1024 bits = 128 bytes)
  integer, parameter :: size_of_M = 128

! Size of the message schedule
  integer, parameter :: size_of_W = 80

! Size of the hash (in 64-bit words)
  integer, parameter :: size_of_H = 6

! Hashing algorithm for SHA-348
  type, extends(t_hash64) :: t_sha384
    private

    contains

!     Hashing initialisation
!     Initialises the hashing base message-digest operation
      procedure, nopass :: initialise => sha384_initialise

!     Hashing update operation
!     Continues the message-digest operation, processing
!     another message block, and updating the context
      procedure :: update => sha384_update

!     Hashing finalisation
!     Finalises the hashing process and returns the resulting hash
!     Overrides the generic finalisation in t_hash64 to truncate the digest
      procedure :: finalise => sha384_finalise

!     Get hash size
      procedure, nopass :: get_hash_size => sha384_get_hash_size

  end type t_sha384

! Constructor interface
  interface sha384
    module procedure sha384_default
  end interface sha384

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
pure function sha384_default() result(res)

! Constructed structure
  type(t_sha384) :: res

! Initialise
  res = t_sha384()

end function sha384_default


! Initialisation
pure subroutine sha384_initialise( H )

! Initial value for hash buffer
  class(*), allocatable, dimension(:), intent(inout) :: H

! Return initial hash
  allocate( H, source=H0 )

end subroutine sha384_initialise


! Message disgst update
pure subroutine sha384_update( this, M, H )

! Calling object
  class(t_sha384), intent(in) :: this

! The message schedule
  integer(kind=1), dimension(:), intent(in) :: M

! Hash buffer
  class(*), dimension(:), intent(inout) :: H

! Local variables
  integer :: i, j
  integer(kind=8), dimension(size_of_W) :: w
  integer(kind=8) :: t1, t2, s0, s1
  integer(kind=8) :: a, b, c, d, e, f, g, n

! Initalise local variables
  w = 0_8

! Prepare the message schedule (1-16)
! The transfer operations set for every double-word W four bytes from the message block
! The message block is assigned left to right whereas W is assined right to left
  do i = 1, 16
    j = 8 * ( i - 1 ) + 1
    w(i) = transfer( M(j+7:j:-1), w(i) )
  end do

! Prepare the message schedule (17-64)
  do i = 17, size(w)
    w(i) = this%low_s1(w(i-2)) + w(i-7) + this%low_s0(w(i-15)) + w(i-16)
  end do

! Initialise the intermediate variables
  select type( H )
    type is(integer(kind=8))
      a = H(1)
      b = H(2)
      c = H(3)
      d = H(4)
      e = H(5)
      f = H(6)
      g = H(7)
      n = H(8)
  end select

! Cycle on the intermediate variables
  do i = 1, size(w)
    s0 = this%high_s0(a)
    s1 = this%high_s1(e)
    t1 = n + s1 + this%choose(e,f,g) + K(i) + w(i)
    t2 = s0 + this%majority(a,b,c)
    n = g
    g = f
    f = e
    e = d + t1
    d = c
    c = b
    b = a
    a = t1 + t2
  end do

! Update the intermediate hash
  select type( H )
    type is(integer(kind=8))
      H(1) = a + H(1)
      H(2) = b + H(2)
      H(3) = c + H(3)
      H(4) = d + H(4)
      H(5) = e + H(5)
      H(6) = f + H(6)
      H(7) = g + H(7)
      H(8) = n + H(8)
  end select

end subroutine sha384_update


! Finalise the message disgest
! Overrides the generic finalisation in t_hash64 to truncate the digest
pure function sha384_finalise( this, H ) result(res)

! The hash structure
  class(t_sha384), intent(in) :: this

! Hash buffer
  class(*), dimension(:), intent(in) :: H

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Local varaibles
  integer(kind=8), dimension(size(H)) :: local8

! Allocate resulting hash (initial allocation)
  allocate( res(this%get_hash_size()) )

! Reoroder the bytes
! The solution is in 8 64-bit integers left to write with
! their bytes order right to left.
! First step: arrange al qwords right to left by qword
! Second step: revert all bytes left to right
! Third step: truncate to the expected size of the hash
  select type( H )
    type is(integer(kind=8))
    local8 = H(size(H):1:-1)
  end select
  res = transfer( local8, res )
  res = res(size(res):1:-1)
  res = res(:this%get_hash_size())

end function sha384_finalise


! Get the hash size (bytes) for this algorithm
pure function sha384_get_hash_size() result(res)

! Hash size
  integer :: res

! Return the size of the hash (in bytes)
  res = 8 * size_of_H

end function sha384_get_hash_size

end module m_sha384
