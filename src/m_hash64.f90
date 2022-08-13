module m_hash64

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : FIPS 180-2, Secure Hash Standard
!             http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
! Synopsis  : Cryptographic hashing base abstract class for 32-bit algorithms
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
  use m_hash

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_hash64

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Size of the hash trailing (in bytes)
  integer, parameter :: size_of_H_trailing = 16

! Size of the message block (1024 bits = 128 bytes)
  integer, parameter :: size_of_M = 128

! Hashing base type for 32 bit algorithms
  type, abstract, extends(t_hash) :: t_hash64
    private

    contains

!     Hashing finalisation
!     Finalises the hashing process and stores the resulting hash
      procedure :: finalise => hash64_finalise

!     Basic algorithm for crypto (Choose)
      procedure, nopass :: choose => hash64_choose

!     Basic algorithm for crypto (Majority)
      procedure, nopass :: majority => hash64_majority

!     Basic algorithm for crypto (low_s0)
      procedure, nopass :: low_s0 => hash64_low_s0

!     Basic algorithm for crypto (low_s1)
      procedure, nopass :: low_s1 => hash64_low_s1

!     Basic algorithm for crypto (high_s0)
      procedure, nopass :: high_s0 => hash64_high_s0

!     Basic algorithm for crypto (high_s1)
      procedure, nopass :: high_s1 => hash64_high_s1

!     Get hash trailing size
      procedure, nopass :: get_hash_trailing_size => hash64_get_hash_trailing_size

!     Get size of the message schedule
      procedure, nopass :: get_size_of_M => hash64_get_size_of_M

  end type t_hash64

!- End of module variable declarations ----------------------------------------

contains

! Finalise the message disgest
pure function hash64_finalise( this, H ) result(res)

! The hash structure
  class(t_hash64), intent(in) :: this

! Hash buffer
  class(*), dimension(:), intent(in) :: H

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Local varaibles
  integer(kind=8), dimension(size(H)) :: local8

! Allocate resulting hash
  allocate( res(this%get_hash_size()) )

! Reoroder the bytes
! The solution is in 8 64-bit integers left to write with
! their bytes order right to left.
! First step: arrange al qwords right to left by qword
! Second step: revert all bytes left to right
  select type( H )
    type is(integer(kind=8))
    local8 = H(size(H):1:-1)
  end select
  res = transfer( local8, res )
  res = res(size(res):1:-1)

end function hash64_finalise


! Basic function for crypto (Choose)
pure function hash64_choose( x, y, z ) result(res)

! First word
  integer(kind=8), intent(in) :: x

! Second word
  integer(kind=8), intent(in) :: y

! Third word
  integer(kind=8), intent(in) :: z

! Return word
  integer(kind=8) :: res

! Compute the result
  res = ieor( iand( x, y ), iand( not(x), z ) )

end function hash64_choose


! Basic function for crypto (Majority)
pure function hash64_majority( x, y, z ) result(res)

! First word
  integer(kind=8), intent(in) :: x

! Second word
  integer(kind=8), intent(in) :: y

! Third word
  integer(kind=8), intent(in) :: z

! Return word
  integer(kind=8) :: res

! Compute the result
  res = ieor( ieor( iand( x, y ), iand( x, z ) ), iand( y, z ) )

end function hash64_majority


! Basic function for SHA-384/512 (high_s0)
pure function hash64_high_s0( x ) result(res)

! Input word
  integer(kind=8), intent(in) :: x

! Return word
  integer(kind=8) :: res

! Compute the result
  integer(kind=8) :: a, b, c, y
  a = ishftc(x,-28)
  b = ishftc(x,-34)
  c = ishftc(x,-39)
  y = ishftc(x,-1)
  res = ieor( ishftc(x,-28), ieor( ishftc(x,-34), ishftc(x,-39) ) )

end function hash64_high_s0


! Basic function for SHA-384/512 (high_s1)
pure function hash64_high_s1( x ) result(res)

! Input word
  integer(kind=8), intent(in) :: x

! Return word
  integer(kind=8) :: res

! Compute the result
  res = ieor( ishftc(x,-14), ieor( ishftc(x,-18), ishftc(x,-41) ) )

end function hash64_high_s1


! Basic function for SHA-384/512 (low_s0)
pure function hash64_low_s0( x ) result(res)

! Input word
  integer(kind=8), intent(in) :: x

! Return word
  integer(kind=8) :: res

! Compute the result
  res = ieor( ishftc(x,-1), ieor( ishftc(x,-8), ishft(x,-7) ) )

end function hash64_low_s0


! Basic function for SHA-384/512 (low_s1)
pure function hash64_low_s1( x ) result(res)

! Input word
  integer(kind=8), intent(in) :: x

! Return word
  integer(kind=8) :: res

! Compute the result
  res = ieor( ishftc(x,-19), ieor( ishftc(x,-61), ishft(x,-6) ) )

end function hash64_low_s1


! Get the hash traling size (bytes) for this hash
pure function hash64_get_hash_trailing_size() result(res)

! Hash size
  integer :: res

! Return the size of the hash trailing (in bytes)
  res = size_of_H_trailing

end function hash64_get_hash_trailing_size


! Get the size of the message schedule
pure function hash64_get_size_of_M() result(res)

! Hash size
  integer :: res

! Return the size of the message schedule (in bytes)
  res = size_of_M

end function hash64_get_size_of_M

end module m_hash64
