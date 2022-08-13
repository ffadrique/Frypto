module m_sha256

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : FIPS 180-2, Secure Hash Standard
!             http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
! Synopsis  : Cryptographic Secure Hash Algorithm (SHA-256)
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
  public t_sha256
  public sha256

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Initial hash value
  integer(kind=4), dimension(8), parameter :: H0 = &
    [ int(z'6a09e667'), int(z'bb67ae85'), int(z'3c6ef372'), int(z'a54ff53a'), &
      int(z'510e527f'), int(z'9b05688c'), int(z'1f83d9ab'), int(z'5be0cd19') ]

! SHA-256 processing constants
  integer(kind=4), dimension(64), parameter :: K = &
     [ int(z'428a2f98'), int(z'71374491'), int(z'b5c0fbcf'), int(z'e9b5dba5'), &
       int(z'3956c25b'), int(z'59f111f1'), int(z'923f82a4'), int(z'ab1c5ed5'), &
       int(z'd807aa98'), int(z'12835b01'), int(z'243185be'), int(z'550c7dc3'), &
       int(z'72be5d74'), int(z'80deb1fe'), int(z'9bdc06a7'), int(z'c19bf174'), &
       int(z'e49b69c1'), int(z'efbe4786'), int(z'0fc19dc6'), int(z'240ca1cc'), &
       int(z'2de92c6f'), int(z'4a7484aa'), int(z'5cb0a9dc'), int(z'76f988da'), &
       int(z'983e5152'), int(z'a831c66d'), int(z'b00327c8'), int(z'bf597fc7'), &
       int(z'c6e00bf3'), int(z'd5a79147'), int(z'06ca6351'), int(z'14292967'), &
       int(z'27b70a85'), int(z'2e1b2138'), int(z'4d2c6dfc'), int(z'53380d13'), &
       int(z'650a7354'), int(z'766a0abb'), int(z'81c2c92e'), int(z'92722c85'), &
       int(z'a2bfe8a1'), int(z'a81a664b'), int(z'c24b8b70'), int(z'c76c51a3'), &
       int(z'd192e819'), int(z'd6990624'), int(z'f40e3585'), int(z'106aa070'), &
       int(z'19a4c116'), int(z'1e376c08'), int(z'2748774c'), int(z'34b0bcb5'), &
       int(z'391c0cb3'), int(z'4ed8aa4a'), int(z'5b9cca4f'), int(z'682e6ff3'), &
       int(z'748f82ee'), int(z'78a5636f'), int(z'84c87814'), int(z'8cc70208'), &
       int(z'90befffa'), int(z'a4506ceb'), int(z'bef9a3f7'), int(z'c67178f2') ]

! Size of the message block (512 bits = 64 bytes)
  integer, parameter :: size_of_M = 64

! Size of the message schedule
  integer, parameter :: size_of_W = 64

! Size of the hash (in 32-bit words)
  integer, parameter :: size_of_H = 8

! Hashing algorithm for SHA-256
  type, extends(t_hash32) :: t_sha256
    private

    contains

!     Hashing initialisation
!     Initialises the hashing base message-digest operation
      procedure, nopass :: initialise => sha256_initialise

!     Hashing update operation
!     Continues the message-digest operation, processing
!     another message block, and updating the context
      procedure :: update => sha256_update

!     Get hash size
      procedure, nopass :: get_hash_size => sha256_get_hash_size

  end type t_sha256

! Constructor interface
  interface sha256
    module procedure sha256_default
  end interface sha256

!- End of module variable declarations ----------------------------------------

  contains

! Default constructor
pure function sha256_default() result(res)

! Constructed structure
  type(t_sha256) :: res

! Initialise
  res = t_sha256()

end function sha256_default


! Initialisation
pure subroutine sha256_initialise( H )

! Initial value for hash buffer
  class(*), allocatable, dimension(:), intent(inout) :: H

! Return initial hash
  allocate( H, source=H0 )

end subroutine sha256_initialise


! Message disgst update
pure subroutine sha256_update( this, M, H )

! Calling object
  class(t_sha256), intent(in) :: this

! The message schedule
  integer(kind=1), dimension(:), intent(in) :: M

! Hash buffer
  class(*), dimension(:), intent(inout) :: H

! Local variables
  integer :: i, j
  integer(kind=4), dimension(size_of_W) :: W
  integer(kind=4), dimension(size_of_H) :: HH
  integer(kind=4) :: t1, t2
  integer(kind=4) :: a, b, c, d, e, f, g, n

! Initalise local variables
  W = 0
  HH = 0

! Prepare the message schedule (1-16)
! The transfer operations set for every double-word W four bytes from the message block
! The message block is assigned left to right whereas W is assined right to left
  do i = 1, 16
    j = 4 * ( i - 1 ) + 1
    w(i) = transfer( M(j+3:j:-1), w(i) )
  end do

! Prepare the message schedule (17-64)
  do i = 17, size(w)
    w(i) = this%low_s1(w(i-2)) + w(i-7) + this%low_s0(w(i-15)) + w(i-16)
  end do

! Initialise the intermediate variables
  select type( H )
    type is(integer)
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
    t1 = n + this%high_s1(e) + this%choose(e,f,g) + K(i) + w(i)
    t2 = this%high_s0(a) + this%majority(a,b,c)
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
    type is(integer)
      H(1) = a + H(1)
      H(2) = b + H(2)
      H(3) = c + H(3)
      H(4) = d + H(4)
      H(5) = e + H(5)
      H(6) = f + H(6)
      H(7) = g + H(7)
      H(8) = n + H(8)
  end select

end subroutine sha256_update


! Get the hash size (bytes) for this algorithm
pure function sha256_get_hash_size() result(res)

! Hash size
  integer :: res

! Return the size of the hash (in bytes)
  res = 4 * size_of_H

end function sha256_get_hash_size

end module m_sha256
