module m_md5

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : https://www.ietf.org/rfc/rfc1321.txt
!             http://tools.ietf.org/html/rfc1321
! Synopsis  : Cryptographic Message Digest Algorithm 5 (MD5)
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

  use, intrinsic :: iso_fortran_env

  use m_util_convert
  use m_object

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_md5
  public md5

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Model constants
  integer(kind=4), parameter :: S11 = 7
  integer(kind=4), parameter :: S12 = 12
  integer(kind=4), parameter :: S13 = 17
  integer(kind=4), parameter :: S14 = 22
  integer(kind=4), parameter :: S21 = 5
  integer(kind=4), parameter :: S22 = 9
  integer(kind=4), parameter :: S23 = 14
  integer(kind=4), parameter :: S24 = 20
  integer(kind=4), parameter :: S31 = 4
  integer(kind=4), parameter :: S32 = 11
  integer(kind=4), parameter :: S33 = 16
  integer(kind=4), parameter :: S34 = 23
  integer(kind=4), parameter :: S41 = 6
  integer(kind=4), parameter :: S42 = 10
  integer(kind=4), parameter :: S43 = 15
  integer(kind=4), parameter :: S44 = 21

! Round 1 constants
  integer(kind=4), parameter :: z101 = int(z'd76aa478')
  integer(kind=4), parameter :: z102 = int(z'e8c7b756')
  integer(kind=4), parameter :: z103 = int(z'242070db')
  integer(kind=4), parameter :: z104 = int(z'c1bdceee')
  integer(kind=4), parameter :: z105 = int(z'f57c0faf')
  integer(kind=4), parameter :: z106 = int(z'4787c62a')
  integer(kind=4), parameter :: z107 = int(z'a8304613')
  integer(kind=4), parameter :: z108 = int(z'fd469501')
  integer(kind=4), parameter :: z109 = int(z'698098d8')
  integer(kind=4), parameter :: z110 = int(z'8b44f7af')
  integer(kind=4), parameter :: z111 = int(z'ffff5bb1')
  integer(kind=4), parameter :: z112 = int(z'895cd7be')
  integer(kind=4), parameter :: z113 = int(z'6b901122')
  integer(kind=4), parameter :: z114 = int(z'fd987193')
  integer(kind=4), parameter :: z115 = int(z'a679438e')
  integer(kind=4), parameter :: z116 = int(z'49b40821')

! Round 2 constants
  integer(kind=4), parameter :: z201 = int(z'f61e2562')
  integer(kind=4), parameter :: z202 = int(z'c040b340')
  integer(kind=4), parameter :: z203 = int(z'265e5a51')
  integer(kind=4), parameter :: z204 = int(z'e9b6c7aa')
  integer(kind=4), parameter :: z205 = int(z'd62f105d')
  integer(kind=4), parameter :: z206 = int(z'02441453')
  integer(kind=4), parameter :: z207 = int(z'd8a1e681')
  integer(kind=4), parameter :: z208 = int(z'e7d3fbc8')
  integer(kind=4), parameter :: z209 = int(z'21e1cde6')
  integer(kind=4), parameter :: z210 = int(z'c33707d6')
  integer(kind=4), parameter :: z211 = int(z'f4d50d87')
  integer(kind=4), parameter :: z212 = int(z'455a14ed')
  integer(kind=4), parameter :: z213 = int(z'a9e3e905')
  integer(kind=4), parameter :: z214 = int(z'fcefa3f8')
  integer(kind=4), parameter :: z215 = int(z'676f02d9')
  integer(kind=4), parameter :: z216 = int(z'8d2a4c8a')

! Round 3 constants
  integer(kind=4), parameter :: z301 = int(z'fffa3942')
  integer(kind=4), parameter :: z302 = int(z'8771f681')
  integer(kind=4), parameter :: z303 = int(z'6d9d6122')
  integer(kind=4), parameter :: z304 = int(z'fde5380c')
  integer(kind=4), parameter :: z305 = int(z'a4beea44')
  integer(kind=4), parameter :: z306 = int(z'4bdecfa9')
  integer(kind=4), parameter :: z307 = int(z'f6bb4b60')
  integer(kind=4), parameter :: z308 = int(z'bebfbc70')
  integer(kind=4), parameter :: z309 = int(z'289b7ec6')
  integer(kind=4), parameter :: z310 = int(z'eaa127fa')
  integer(kind=4), parameter :: z311 = int(z'd4ef3085')
  integer(kind=4), parameter :: z312 = int(z'04881d05')
  integer(kind=4), parameter :: z313 = int(z'd9d4d039')
  integer(kind=4), parameter :: z314 = int(z'e6db99e5')
  integer(kind=4), parameter :: z315 = int(z'1fa27cf8')
  integer(kind=4), parameter :: z316 = int(z'c4ac5665')

! Round 4 constants
  integer(kind=4), parameter :: z401 = int(z'f4292244')
  integer(kind=4), parameter :: z402 = int(z'432aff97')
  integer(kind=4), parameter :: z403 = int(z'ab9423a7')
  integer(kind=4), parameter :: z404 = int(z'fc93a039')
  integer(kind=4), parameter :: z405 = int(z'655b59c3')
  integer(kind=4), parameter :: z406 = int(z'8f0ccc92')
  integer(kind=4), parameter :: z407 = int(z'ffeff47d')
  integer(kind=4), parameter :: z408 = int(z'85845dd1')
  integer(kind=4), parameter :: z409 = int(z'6fa87e4f')
  integer(kind=4), parameter :: z410 = int(z'fe2ce6e0')
  integer(kind=4), parameter :: z411 = int(z'a3014314')
  integer(kind=4), parameter :: z412 = int(z'4e0811a1')
  integer(kind=4), parameter :: z413 = int(z'f7537e82')
  integer(kind=4), parameter :: z414 = int(z'bd3af235')
  integer(kind=4), parameter :: z415 = int(z'2ad7d2bb')
  integer(kind=4), parameter :: z416 = int(z'eb86d391')

! Initial state
  integer(kind=4), dimension(4), parameter :: state0 = &
        [ int( z'67452301' ), int (z'efcdab89' ), int( z'98badcfe' ), int( z'10325476' ) ]
  
! Bit sequence padding
! gfortran does not accept -128_1, therefore it is initialise as kind=2 and then converted to kind=1 
  integer(kind=2), parameter, dimension(64) :: padding2 = [ &
    -128_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, &
       0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, &
       0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, &
       0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2  ]
  integer(kind=1), dimension(64) :: padding = int( padding2, 1 )

! Reference buffer size constant
 integer(kind=4), parameter :: z3f = int(z'3f')

! MD5 type
  type, extends(t_object) :: t_md5
    private

      integer(kind=4), dimension(4) :: state = state0
      integer(kind=4), dimension(2) :: count = 0
      integer(kind=1), dimension(64) :: buffer = 0_1
      integer(kind=1), dimension(16) :: digest = 0_1
      logical :: finalised = .false.

    contains

!     Encode a character string or from open file
      generic :: encode => md5_encode_string, md5_encode_buffer, md5_encode_unit
      procedure, private :: md5_encode_string
      procedure, private :: md5_encode_buffer
      procedure, private :: md5_encode_unit

!     Get interfaces
      procedure :: get_hash => md5_get_hash

!     MD5 hashing initialisation
      procedure, private :: md5_initialise

!     MD5 block update operation.
!     Continues an MD5 message-digest operation, processing
!     another message block, and updating the context.
      procedure, private :: md5_update

!     MD5 basic transformation. Transforms state based on block.
      procedure, private :: md5_transform

!     MD5 finalization. Ends an MD5 message-digest operation,
!     generating the message digest and zeroizing the context.
      procedure, private :: md5_finalise

  end type t_md5

! Constructor interface
  interface md5
    module procedure md5_default
  end interface md5

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
elemental function md5_default() result(res)

! The MD5 structure
  type(t_md5) :: res

! Initialise
  res = t_md5()

end function md5_default


! Encode from character string
elemental subroutine md5_encode_string( this, string )

! The MD5 structure
  class(t_md5), intent(inout) :: this

! The string to digest
  character(len=*), intent(in) :: string

! Initialise
  call this%md5_initialise()

  ! Process the input string
  call this%md5_update( character_to_bytes(string) )

! Finalise the digest
  call this%md5_finalise()

end subroutine md5_encode_string


! Encode from byte buffer
pure subroutine md5_encode_buffer( this, buffer )

! The MD5 structure
  class(t_md5), intent(inout) :: this

! The buffer to digest
  integer(kind=1), dimension(:), intent(in) :: buffer

! Initialise
  call this%md5_initialise()

! Process the input string
  call this%md5_update( buffer )

! Finalise the digest
  call this%md5_finalise()

end subroutine md5_encode_buffer


! Encode file contents
subroutine md5_encode_unit( this, unit, iostat )

! The MD5 structure
  class(t_md5), intent(inout) :: this

! The Fortran unit for the file to digest
  integer, intent(in) :: unit

! The read status
  integer, intent(out) :: iostat

! File read control
  integer :: nbytes
  integer :: nreads

! Local variables
  integer(kind=1), dimension(1024) :: buffer
  integer :: isize

! Initialise
  call this%md5_initialise()
  iostat = 0
  nreads = 0

! Get the file size
  inquire( unit=unit, size=nbytes )

! Loop reading into the buffer
  do

!   Reset buffer
    buffer = 0_1

!   Read into buffer
    read( unit, iostat=iostat ) buffer
    if( iostat > 0 ) then

!     This is an error; return
      exit

    else

!     Check if a full buffer has been read
      if( iostat == 0 ) then

!       A full buffer has been read
        nreads = nreads + 1

!       Process the buffer
        call this%md5_update( buffer )

      else if( iostat == iostat_end ) then

!       End of the file; check the number of read elements in last call
        isize = nbytes - nreads * size(buffer)

!       Process the buffer
        call this%md5_update( buffer(:isize) )

!       Finalise the read loop
        iostat = 0
        exit

      end if

    end if

  end do

! Finalise the digest
  call this%md5_finalise()

end subroutine md5_encode_unit


! Initialise MD5 hashing
pure subroutine md5_initialise( this )

! The MD5 structure
  class(t_md5), intent(inout) :: this

  this%state = state0
  this%count = 0
  this%buffer = 0_1
  this%digest = 0_1
  this%finalised = .false.

end subroutine md5_initialise


! MD5 block update operation.
! Continues an MD5 message-digest operation, processing
! another message block, and updating the context.
pure subroutine md5_update( this, input )

! The MD5 structure
  class(t_md5), intent(inout) :: this

! The input block
  integer(kind=1), dimension(:), intent(in) :: input

! Local variables
  integer :: input_index, buffer_index
  integer :: input_length
  integer :: buffer_space
  integer :: iaux

! Check if finalised
  if( .not. this%finalised ) then

!   Compute the input length
    input_length = size(input)

!   Compute number of bytes mod 64
    buffer_index = iand( ishft( this%count(1), -3), z3f ) + 1

!   Update number of bits
    iaux = ishft( input_length, 3 )
    this%count(1) = this%count(1) + iaux
    if( this%count(1) < iaux ) then
      this%count(2) = this%count(2) + 1
    end if
    this%count(2) = ishft( input_length, -29 )

!   How much space is left in buffer
    buffer_space = 64 - buffer_index + 1

!   Transform as many times as possible.
   if( input_length >= buffer_space ) then

!     We have enough to fill the buffer
!     Fill the rest of the buffer and transform
      this%buffer(buffer_index:buffer_index+buffer_space-1) = input(:buffer_space)
      call this%md5_transform( this%buffer )

!     Transform each 64-byte piece of the input, bypassing the buffer
      do input_index = buffer_space + 1, input_length - 63, 64
        call this%md5_transform( input(input_index:) )
      end do

!     Reset the buffer index so we can buffer remaining
      buffer_index = 1

    else

!     Reset the buffer index so we can buffer the whole input
      input_index = 1

    end if

!   Do the buffering
    this%buffer(buffer_index:buffer_index+input_length-input_index) = &
                     input(input_index:input_index+input_length-input_index)

  end if

end subroutine md5_update


! MD5 basic transformation. Transforms state based on block.
pure subroutine md5_transform( this, block )

! The MD5 structure
  class(t_md5), intent(inout) :: this

! The block to process
  integer(kind=1), dimension(1:64), intent(in) :: block

! Local variables
  integer(kind=4) :: a, b, c, d
  integer(kind=4), dimension(16) :: x

! Initialise
  a = this%state(1)
  b = this%state(2)
  c = this%state(3)
  d = this%state(4)

! Decode the block
  call md5_decode( block, x )

! Round 1
  call FF( a, b, c, d, x( 1), S11, z101 )
  call FF( d, a, b, c, x( 2), S12, z102 )
  call FF( c, d, a, b, x( 3), S13, z103 )
  call FF( b, c, d, a, x( 4), S14, z104 )
  call FF( a, b, c, d, x( 5), S11, z105 )
  call FF( d, a, b, c, x( 6), S12, z106 )
  call FF( c, d, a, b, x( 7), S13, z107 )
  call FF( b, c, d, a, x( 8), S14, z108 )
  call FF( a, b, c, d, x( 9), S11, z109 )
  call FF( d, a, b, c, x(10), S12, z110 )
  call FF( c, d, a, b, x(11), S13, z111 )
  call FF( b, c, d, a, x(12), S14, z112 )
  call FF( a, b, c, d, x(13), S11, z113 )
  call FF( d, a, b, c, x(14), S12, z114 )
  call FF( c, d, a, b, x(15), S13, z115 )
  call FF( b, c, d, a, x(16), S14, z116 )

! Round 2
  call GG( a, b, c, d, x( 2), S21, z201 )
  call GG( d, a, b, c, x( 7), S22, z202 )
  call GG( c, d, a, b, x(12), S23, z203 )
  call GG( b, c, d, a, x( 1), S24, z204 )
  call GG( a, b, c, d, x( 6), S21, z205 )
  call GG( d, a, b, c, x(11), S22, z206 )
  call GG( c, d, a, b, x(16), S23, z207 )
  call GG( b, c, d, a, x( 5), S24, z208 )
  call GG( a, b, c, d, x(10), S21, z209 )
  call GG( d, a, b, c, x(15), S22, z210 )
  call GG( c, d, a, b, x( 4), S23, z211 )
  call GG( b, c, d, a, x( 9), S24, z212 )
  call GG( a, b, c, d, x(14), S21, z213 )
  call GG( d, a, b, c, x( 3), S22, z214 )
  call GG( c, d, a, b, x( 8), S23, z215 )
  call GG( b, c, d, a, x(13), S24, z216 )

! Round 3
  call HH( a, b, c, d, x( 6), S31, z301 )
  call HH( d, a, b, c, x( 9), S32, z302 )
  call HH( c, d, a, b, x(12), S33, z303 )
  call HH( b, c, d, a, x(15), S34, z304 )
  call HH( a, b, c, d, x( 2), S31, z305 )
  call HH( d, a, b, c, x( 5), S32, z306 )
  call HH( c, d, a, b, x( 8), S33, z307 )
  call HH( b, c, d, a, x(11), S34, z308 )
  call HH( a, b, c, d, x(14), S31, z309 )
  call HH( d, a, b, c, x( 1), S32, z310 )
  call HH( c, d, a, b, x( 4), S33, z311 )
  call HH( b, c, d, a, x( 7), S34, z312 )
  call HH( a, b, c, d, x(10), S31, z313 )
  call HH( d, a, b, c, x(13), S32, z314 )
  call HH( c, d, a, b, x(16), S33, z315 )
  call HH( b, c, d, a, x( 3), S34, z316 )

! Round 4
  call II( a, b, c, d, x( 1), S41, z401 )
  call II( d, a, b, c, x( 8), S42, z402 )
  call II( c, d, a, b, x(15), S43, z403 )
  call II( b, c, d, a, x( 6), S44, z404 )
  call II( a, b, c, d, x(13), S41, z405 )
  call II( d, a, b, c, x( 4), S42, z406 )
  call II( c, d, a, b, x(11), S43, z407 )
  call II( b, c, d, a, x( 2), S44, z408 )
  call II( a, b, c, d, x( 9), S41, z409 )
  call II( d, a, b, c, x(16), S42, z410 )
  call II( c, d, a, b, x( 7), S43, z411 )
  call II( b, c, d, a, x(14), S44, z412 )
  call II( a, b, c, d, x( 5), S41, z413 )
  call II( d, a, b, c, x(12), S42, z414 )
  call II( c, d, a, b, x( 3), S43, z415 )
  call II( b, c, d, a, x(10), S44, z416 )

! Set state
  this%state = this%state + [ a, b, c, d ]

! Zeroize sensitive information
  x = 0

end subroutine md5_transform


! MD5 finalization. Ends an MD5 message-digest operation,
! generating the message digest and zeroizing the context.
elemental subroutine md5_finalise( this )

! The MD5 structure
  class(t_md5), intent(inout) :: this

! Local variables
  integer(kind=1), dimension(8) :: bits
  integer(kind=4) :: idx, pad_len

! Save number of bits
  call md5_encode( this%count, bits );

! Pad out to 56 mod 64
  idx = iand( ishft( this%count(1), -3 ), z3f )
  if( idx < 56 ) then
    pad_len = 56 - idx
  else
    pad_len = 120 - idx
  end if
  call md5_update( this, padding(:pad_len) )

! Append length (before padding)
  call md5_update( this, bits )

! Store state in digest
  call md5_encode( this%state, this%digest )

! Zeroize sensitive information
  this%buffer = 0_1

! Set finalisation flag
  this%finalised = .true.

end subroutine md5_finalise


! Encode input (integer(kind=4)) into output (integer(kind=1)).
! Assumes len is a multiple of 4.
pure subroutine md5_encode( input, output )

! The input array
  integer(kind=4), dimension(:), intent(in) :: input

! The output array
  integer(kind=1), dimension(:), intent(out) :: output

! Local variables
  integer :: i, j

! Set the result
  j = 1
  do i = 1, size(input)
    output(j:j+3) = transfer( input(i), output(j:j+3) )
    j = j + 4
  end do

end subroutine md5_encode


! Decode input (character) into output (integer(kind=4)).
! Assumes len is a multiple of 4.
pure subroutine md5_decode( input, output )

! The input array
  integer(kind=1), dimension(:), intent(in) :: input

! The output array
  integer(kind=4), dimension(:), intent(out) :: output

! Local variables
  integer :: i, j

! Set the result
  j = 1
  do i = 1, size(output)
    output(i) = transfer( input(j:j+3), output(i) )
    j = j + 4
  end do

end subroutine md5_decode


! The F auxiliary MD5 function
elemental function F( x, y, z ) result(res)

! First integer
  integer(kind=4), intent(in) :: x

! Second integer
  integer(kind=4), intent(in) :: y

! Third integer
  integer(kind=4), intent(in) :: z

! The resulting integer
  integer(kind=4) :: res

! Compute the result
  res = ior( iand( x, y ),  iand( not(x), z ) )

end function F


! The G auxiliary MD5 function
elemental function G( x, y, z ) result(res)

! First integer
  integer(kind=4), intent(in) :: x

! Second integer
  integer(kind=4), intent(in) :: y

! Third integer
  integer(kind=4), intent(in) :: z

! The resulting integer
  integer(kind=4) :: res

! Compute the result
  res = ior( iand( x, z ), iand( y, not(z) ) )

end function G


! The H auxiliary MD5 function
elemental function H( x, y, z ) result(res)

! First integer
  integer(kind=4), intent(in) :: x

! Second integer
  integer(kind=4), intent(in) :: y

! Third integer
  integer(kind=4), intent(in) :: z

! The resulting integer
  integer(kind=4) :: res

! Compute the result
  res = ieor( ieor( x, y ), z )

end function H


! The I auxiliary MD5 function
elemental function I( x, y, z ) result(res)

! First integer
  integer(kind=4), intent(in) :: x

! Second integer
  integer(kind=4), intent(in) :: y

! Third integer
  integer(kind=4), intent(in) :: z

! The resulting integer
  integer(kind=4) :: res

! Compute the result
  res = ieor( y, ior( x, not(z) ) )

end function I


! The FF auxiliary MD5 function for transformation 1, 2, 3, 4
elemental subroutine FF( a, b, c, d, x, s, ac )

! First integer
  integer(kind=4), intent(inout) :: a

! Second integer
  integer(kind=4), intent(in) :: b

! Third integer
  integer(kind=4), intent(in) :: c

! Fourth integer
  integer(kind=4), intent(in) :: d

! Fifth integer
  integer(kind=4), intent(in) :: x

! Sixth integer
  integer(kind=4), intent(in) :: s

! Seventh integer
  integer(kind=4), intent(in) :: ac

! Compute the result
 a = a + F( b, c, d ) + x + ac
 a = ishftc( a, s ) + b

end subroutine FF


! The GG auxiliary MD5 function for transformation 1, 2, 3, 4
elemental subroutine GG( a, b, c, d, x, s, ac )

! First integer
  integer(kind=4), intent(inout) :: a

! Second integer
  integer(kind=4), intent(in) :: b

! Third integer
  integer(kind=4), intent(in) :: c

! Fourth integer
  integer(kind=4), intent(in) :: d

! Fifth integer
  integer(kind=4), intent(in) :: x

! Sixth integer
  integer(kind=4), intent(in) :: s

! Seventh integer
  integer(kind=4), intent(in) :: ac

! Compute the result
 a = a + G( b, c, d ) + x + ac;
 a = ishftc( a, s ) + b

end subroutine GG


! The HH auxiliary MD5 function for transformation 1, 2, 3, 4
elemental subroutine HH( a, b, c, d, x, s, ac )

! First integer
  integer(kind=4), intent(inout) :: a

! Second integer
  integer(kind=4), intent(in) :: b

! Third integer
  integer(kind=4), intent(in) :: c

! Fourth integer
  integer(kind=4), intent(in) :: d

! Fifth integer
  integer(kind=4), intent(in) :: x

! Sixth integer
  integer(kind=4), intent(in) :: s

! Seventh integer
  integer(kind=4), intent(in) :: ac

! Compute the result
 a = a + H( b, c, d ) + x + ac;
 a = ishftc( a, s ) + b

end subroutine HH


! II auxiliary MD5 function for transformation 1, 2, 3, 4
elemental subroutine II( a, b, c, d, x, s, ac )

! First integer
  integer(kind=4), intent(inout) :: a

! Second integer
  integer(kind=4), intent(in) :: b

! Third integer
  integer(kind=4), intent(in) :: c

! Fourth integer
  integer(kind=4), intent(in) :: d

! Fifth integer
  integer(kind=4), intent(in) :: x

! Sixth integer
  integer(kind=4), intent(in) :: s

! Seventh integer
  integer(kind=4), intent(in) :: ac

! Compute the result
 a = a + I( b, c, d ) + x + ac;
 a = ishftc( a, s ) + b

end subroutine II


! Get attribute digest
pure function md5_get_hash( this ) result(res)

! The data structure
  class(t_md5), intent(in) :: this

! The parameter value to be returned
  integer(kind=1), dimension(16) :: res

! Return the value
  res = this%digest

end function md5_get_hash


end module m_md5
