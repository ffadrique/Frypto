module m_sha1

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : FIPS 180-2, Secure Hash Standard
!             http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
! Synopsis  : Cryptographic Secure Hash Algorithm (SHA-1)
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

  use m_hash32

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_sha1
  public sha1

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Initial hash value
  integer(kind=4), dimension(5), parameter :: H0 = &
         [ int(z'67452301'), int(z'efcdab89'), int(z'98badcfe'), int(z'10325476'), int(z'c3d2e1f0') ]

! SHA-1 processing constants
  integer(kind=4), dimension(4), parameter :: K = &
         [ int(z'5a827999'), int(z'6ed9eba1'), int(z'8f1bbcdc'), int(z'ca62c1d6') ]

! Size of the message schedule
  integer, parameter :: size_of_W = 80

! Size of the hash (in 32-bit words)
  integer, parameter :: size_of_H = 5

! Hashing algorithm for SHA-1
  type, extends(t_hash32) :: t_sha1
    private

    contains

!     Hashing initialisation
!     Initialises the hashing base message-digest operation
      procedure, nopass :: initialise => sha1_initialise

!     Hashing update operation
!     Continues the message-digest operation, processing
!     another message block, and updating the context
      procedure :: update => sha1_update

!     Get hash size
      procedure, nopass :: get_hash_size => sha1_get_hash_size

  end type t_sha1

! Constructor interface
  interface sha1
    module procedure sha1_default
  end interface sha1

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
pure function sha1_default() result(res)

! Constructed structure
  type(t_sha1) :: res

! Initialise
  res = t_sha1()

end function sha1_default


! Initialisation
pure subroutine sha1_initialise( H )

! Initial value for hash buffer
  class(*), allocatable, dimension(:), intent(inout) :: H

! Return initial hash
  allocate( H, source=H0 )

end subroutine sha1_initialise


! Message disgst update
pure subroutine sha1_update( this, M, H )

! Calling object
  class(t_sha1), intent(in) :: this

! The message schedule
  integer(kind=1), dimension(:), intent(in) :: M

! Hash buffer
  class(*), dimension(:), intent(inout) :: H

! Local variables
  integer :: i, j
  integer(kind=4), dimension(size_of_W) :: w
  integer(kind=4) :: t
  integer(kind=4) :: a, b, c, d, e

! Initalise local variables
  w = 0

! Prepare the message schedule (1-16)
! The transfer operations set for every double-word W four bytes from the message block
! The message block is assigned left to right whereas W is assined right to left
  do i = 1, 16
    j = 4 * ( i - 1 ) + 1
    w(i) = transfer( M(j+3:j:-1), w(i) )
  end do

! Prepare the message schedule (17-64)
  do i = 17, size(w)
    t = ieor( ieor( ieor( w(i-3), w(i-8) ), w(i-14) ), w(i-16) )
    w(i) = ishftc( t, 1 )
  end do

! Initialise the intermediate variables
  select type( H )
    type is(integer)
      a = H(1)
      b = H(2)
      c = H(3)
      d = H(4)
      e = H(5)
  end select

! Cycle on the intermediate variables
  do i = 1, 20
    t = ishftc(a,5) + this%choose( b, c, d ) + e + K(1) + W(i)
    e = d
    d = c
    c = ishftc(b,30)
    b = a
    a = t
  end do

  do i = 21, 40
    t = ishftc(a,5) + this%parity( b, c, d ) + e + K(2) + W(i)
    e = d
    d = c
    c = ishftc(b,30)
    b = a
    a = t
  end do

  do i = 41, 60
    t = ishftc(a,5) + this%majority( b, c, d ) + e + K(3) + W(i)
    e = d
    d = c
    c = ishftc(b,30)
    b = a
    a = t
  end do

  do i = 61, 80
    t = ishftc(a,5) + this%parity( b, c, d ) + e + K(4) + W(i)
    e = d
    d = c
    c = ishftc(b,30)
    b = a
    a = t
  end do

! Update the intermediate hash
  select type( H )
    type is(integer)
      H(1) = a + H(1)
      H(2) = b + H(2)
      H(3) = c + H(3)
      H(4) = d + H(4)
      H(5) = e + H(5)
  end select

end subroutine sha1_update


! Get the hash size (bytes) for this algorithm
pure function sha1_get_hash_size() result(res)

! Hash size
  integer :: res

! Return the size of the hash (in bytes)
  res = 4 * size_of_H

end function sha1_get_hash_size

end module m_sha1
