module m_hash32

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
  public t_hash32

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Size of the hash trailing (in bytes)
  integer, parameter :: size_of_H_trailing = 8

! Size of the message block (512 bits = 64 bytes)
  integer, parameter :: size_of_M = 64

! Hashing base type for 32 bit algorithms
  type, abstract, extends(t_hash) :: t_hash32
    private

    contains

!     Hashing finalisation
!     Finalises the hashing process and stores the resulting hash
      procedure :: finalise => hash32_finalise

!     Basic algorithm for crypto (Choose)
      procedure, nopass :: choose => hash32_choose
      procedure, nopass :: choose2 => hash32_choose2

!     Basic algorithm for crypto (Majority)
      procedure, nopass :: majority => hash32_majority

!     Basic algorithm for crypto (parity)
      procedure, nopass :: parity => hash32_parity

!     Basic algorithm for crypto (selectivity; name invented by Fran)
      procedure, nopass :: selectivity => hash32_selectivity

!     Basic algorithm for crypto (low_s0)
      procedure, nopass :: low_s0 => hash32_low_s0

!     Basic algorithm for crypto (low_s1)
      procedure, nopass :: low_s1 => hash32_low_s1

!     Basic algorithm for crypto (high_s0)
      procedure, nopass :: high_s0 => hash32_high_s0

!     Basic algorithm for crypto (high_s1)
      procedure, nopass :: high_s1 => hash32_high_s1

!     Get hash trailing size
      procedure, nopass :: get_hash_trailing_size => hash32_get_hash_trailing_size

!     Get size of the message schedule
      procedure, nopass :: get_size_of_M => hash32_get_size_of_M

  end type t_hash32

!- End of module variable declarations ----------------------------------------

  contains

! Finalise the message disgest
pure function hash32_finalise( this, H ) result(res)

! The hash structure
  class(t_hash32), intent(in) :: this

! Hash buffer
  class(*), dimension(:), intent(in) :: H

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Local varaibles
  integer, dimension(size(H)) :: local

! Allocate resulting hash
  allocate( res(this%get_hash_size()) )

! Reoroder the bytes
! The solution is in 4 32-bit integers left to write with
! their bytes order right to left.
! First step: arrange al dwords right to left by dword
! Second step: revert all bytes left to right
  select type( H )
    type is(integer)
    local = H(size(H):1:-1)
  end select
  res = transfer( local, res )
  res = res(size(res):1:-1)

end function hash32_finalise


! Basic function for crypto (Choose)
pure function hash32_choose( x, y, z ) result(res)

! First word
  integer(kind=4), intent(in) :: x

! Second word
  integer(kind=4), intent(in) :: y

! Third word
  integer(kind=4), intent(in) :: z

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( iand( x, y ), iand( not(x), z ) )

end function hash32_choose


! Basic function for crypto (Choose second function)
pure function hash32_choose2( x, y, z ) result(res)

! First word
  integer(kind=4), intent(in) :: x

! Second word
  integer(kind=4), intent(in) :: y

! Third word
  integer(kind=4), intent(in) :: z

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( iand( x, z ), iand( y, not(z) ) )

end function hash32_choose2


! Basic function for crypto (Majority)
pure function hash32_majority( x, y, z ) result(res)

! First word
  integer(kind=4), intent(in) :: x

! Second word
  integer(kind=4), intent(in) :: y

! Third word
  integer(kind=4), intent(in) :: z

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( ieor( iand( x, y ), iand( x, z ) ), iand( y, z ) )

end function hash32_majority


! Basic function for crypto (Parity)
pure function hash32_parity( x, y, z ) result(res)

! First word
  integer(kind=4), intent(in) :: x

! Second word
  integer(kind=4), intent(in) :: y

! Third word
  integer(kind=4), intent(in) :: z

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( ieor( x, y ), z )

end function hash32_parity


! Basic function for crypto (Selectivity; invented name by Fran)
pure function hash32_selectivity( x, y, z ) result(res)

! First word
  integer(kind=4), intent(in) :: x

! Second word
  integer(kind=4), intent(in) :: y

! Third word
  integer(kind=4), intent(in) :: z

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( y, ior( x, not(z) ) )

end function hash32_selectivity


! Basic function for SHA-256 (high_s0)
pure function hash32_high_s0( x ) result(res)

! Input word
  integer(kind=4), intent(in) :: x

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( ishftc(x,-2), ieor( ishftc(x,-13), ishftc(x,-22) ) )

end function hash32_high_s0


! Basic function for SHA-256 (high_s1)
pure function hash32_high_s1( x ) result(res)

! Input word
  integer(kind=4), intent(in) :: x

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( ishftc(x,-6), ieor( ishftc(x,-11), ishftc(x,-25) ) )

end function hash32_high_s1


! Basic function for SHA-256 (low_s0)
pure function hash32_low_s0( x ) result(res)

! Input word
  integer(kind=4), intent(in) :: x

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( ishftc(x,-7), ieor( ishftc(x,-18), ishft(x,-3) ) )

end function hash32_low_s0


! Basic function for SHA-256 (low_s1)
pure function hash32_low_s1( x ) result(res)

! Input word
  integer(kind=4), intent(in) :: x

! Return word
  integer(kind=4) :: res

! Compute the result
  res = ieor( ishftc(x,-17), ieor( ishftc(x,-19), ishft(x,-10) ) )

end function hash32_low_s1


! Get the hash traling size (bytes) for this hash
pure function hash32_get_hash_trailing_size() result(res)

! Hash size
  integer :: res

! Return the size of the hash trailing (in bytes)
  res = size_of_H_trailing

end function hash32_get_hash_trailing_size


! Get the size of the message schedule
pure function hash32_get_size_of_M() result(res)

! Hash size
  integer :: res

! Return the size of the message schedule (in bytes)
  res = size_of_M

end function hash32_get_size_of_M

end module m_hash32
