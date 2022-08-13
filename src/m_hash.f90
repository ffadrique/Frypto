module m_hash

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : FIPS 180-2, Secure Hash Standard
!             http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
! Synopsis  : Cryptographic hashing base abstract class
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

  use m_object
  use m_util_convert

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_hash

  public hash_z00, hash_z80, hash_zff

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Used constants
 integer(kind=1), parameter :: hash_z00 = int(z'00',1)
 integer(kind=1), parameter :: hash_z80 = int(z'80',1)
 integer(kind=1), parameter :: hash_zff = int(z'ff',1)

! Hashing base type
  type, abstract, extends(t_object) :: t_hash
    private

    contains

!     Encode a character string or from open file
      generic :: encode => hash_encode_string, hash_encode_buffer, hash_encode_unit
      procedure, private :: hash_encode_string
      procedure, private :: hash_encode_buffer
      procedure, private :: hash_encode_unit

!     Hashing initialisation
!     Initialises the hashing base message-digest operation
      procedure(hash_initialise), nopass, deferred :: initialise

!     Hashing update operation
!     Continues the message-digest operation, processing
!     another message block, and updating the context
      procedure(hash_update), deferred :: update

!     Hashing padding
!     Pads the message block to the size of the implemented algorithm
      procedure, nopass :: padding => hash_padding

!     Hashing base trailing
!     Add the message length to the end of the last processed block
      procedure, nopass :: trailing => hash_trailing

!     Hashing finalisation
!     Finalises the hashing base message-digest operation
      procedure(hash_finalise), deferred :: finalise

!     Get the size (bytes) of the hash
      procedure(hash_get_property_integer), nopass, deferred :: get_hash_size

!     Get the size (bytes) of the hash trailing bytes
      procedure(hash_get_property_integer), nopass, deferred :: get_hash_trailing_size

!     Get the size (bytes) of M
      procedure(hash_get_property_integer), nopass, deferred :: get_size_of_M

!     Hashing block processing
!     Processes a full-bit-sized message block
      procedure :: process_block => hash_process_block

!     Hashing last block processing
!     Processes the last (non-full-bit-sized) input block
      procedure :: process_last_block => hash_process_last_block

  end type t_hash


! Abstractct procedure interfaces
  abstract interface

!   Initialisation
    pure subroutine hash_initialise( H )
      class(*), allocatable, dimension(:), intent(inout) :: H
    end subroutine hash_initialise

!   Update
    pure subroutine hash_update( this, M, H )
      import t_hash
      class(t_hash), intent(in) :: this
      integer(kind=1), dimension(:), intent(in) :: M
      class(*), dimension(:), intent(inout) :: H
    end subroutine hash_update

!   Finalisation
    pure function hash_finalise( this, H ) result(res)
      import t_hash
      class(t_hash), intent(in) :: this
      class(*), dimension(:), intent(in) :: H
      integer(kind=1), dimension(:), allocatable :: res
    end function hash_finalise

!   Access to integer property
    pure function hash_get_property_integer() result(res)
      integer :: res
    end function hash_get_property_integer

  end interface

!- End of module variable declarations ----------------------------------------

contains

! Encode from input byte buffer
pure function hash_encode_buffer( this, buffer ) result(res)

! The hash structure
  class(t_hash), intent(in) :: this

! The string to digest
  integer(kind=1), dimension(:), intent(in) :: buffer

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Array for the message hash buffer
  class(*), allocatable, dimension(:) :: H

! Local variables
  integer :: iblock
  integer :: nblocks
  integer :: ipos
  integer :: mlength
  integer :: input_length
  integer :: hashsize

! Initialise
  mlength = this%get_size_of_M()

! Store the message length
  input_length = size(buffer)

! Compute the number of blocks to process
! This is the actual number of full 512-bit blocks
  nblocks = input_length / mlength

! Initialise the input string position
  ipos = 1

! Initialise hash
  call this%initialise( H )

! Loop on the expected blocks
  do iblock = 1, nblocks

!   Process the block (in local copy)
    call this%process_block( buffer(ipos:ipos+mlength-1), H )

!   Update the position in input string
    ipos = ipos + mlength

  end do

! Process the last block
! if len(6) = n * 64 exactly this next block is empty but still needs
! to be processed as the last block with the trailing count of bytes
  call this%process_last_block( buffer(ipos:), H, input_length )

! Finalise
  hashsize = this%get_hash_size()
  allocate( res(hashsize), source=0_1 )
  res = this%finalise( H )

end function hash_encode_buffer


! Encode from input character string
pure function hash_encode_string( this, s ) result(res)

! The hash structure
  class(t_hash), intent(in) :: this

! The string to digest
  character(len=*), intent(in) :: s

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Convert to buffer and process
  res = this%encode( character_to_bytes(s) )

end function hash_encode_string


! Encode from file contents
function hash_encode_unit( this, unit, iostat ) result(res)

! The hash structure
  class(t_hash), intent(in) :: this

! The Fortran unit for the file to digest
! File must have been opened with access=stream
  integer, intent(in) :: unit

! The read status
  integer, intent(out) :: iostat

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Array for the message hash buffer
  class(*), allocatable, dimension(:) :: H

! File read control
  integer :: nbytes
  integer :: nreads
  integer :: isize
  integer :: input_length
  integer :: hashsize

! Read buffer of the same size as the algorithm message block
  integer(kind=1), dimension(:), allocatable :: M

! Intialise
  iostat = 0
  nreads = 0
  allocate( M(this%get_size_of_M()), source=0_1 )

! Get the file size
  inquire( unit=unit, size=nbytes )

! Set the file size
  input_length = nbytes

! Initialise hash
  call this%initialise( H )

! Loop reading into the message block
  do

!   Reset block
    M = 0_1

!   Read into block
    read( unit, iostat=iostat ) M
    if( iostat > 0 ) then

!     This is an error; return
      exit

    else

!     Check if a full buffer has been read
      if( iostat == 0 ) then

!       A full buffer has been read
        nreads = nreads + 1

!       Process the buffer
        call this%process_block( M, H )

      else if( iostat == iostat_end ) then

!       End of the file
!       Compute and set the total file size
        isize = nbytes - nreads * size(M)

!       Process the buffer
        call this%process_last_block( M(:isize), H, input_length )

!       Finalise the read loop
        iostat = 0
        exit

      end if

    end if

  end do

! Finalise
  hashsize = this%get_hash_size()
  allocate( res(hashsize), source=0_1 )
  res = this%finalise( H )

end function hash_encode_unit


! Process a message block
pure subroutine hash_process_block( this, M, H )

! The hash structure
  class(t_hash), intent(in) :: this

! The input block
  integer(kind=1), dimension(:), intent(in) :: M

! Hash buffer
  class(*), dimension(:), intent(inout) :: H

! Update
  call this%update( M, H )

end subroutine hash_process_block


! Process an incomplete (trailing) message block
pure recursive subroutine hash_process_last_block( this, S, H, input_length )

! The hash structure
  class(t_hash), intent(in) :: this

! The input block
  integer(kind=1), dimension(:), intent(in) :: S

! Hash buffer
  class(*), dimension(:), intent(inout) :: H

! The meassage total input length (required for trailing)
  integer, intent(in) :: input_length

! Local variables
  integer :: slen
  integer :: mlength
  integer(kind=1), dimension(:), allocatable :: M

! Initialise local variables
  mlength = this%get_size_of_M()
  allocate( M(mlength), source=0_1 )
  slen = size(s)

! Insert the block into the processing message block
  M(1:slen) = s
  M(slen+1:) = hash_z00

! Check if the trailing fits in the the reaminder of the block
! At least 9 bytes are required at the end of the string
  if( slen > mlength - this%get_hash_trailing_size() - 1 ) then

!   Pad the message block
    call this%padding( M , slen+1 )

!   Update
    call this%update( M, H )

!   Next block is zeroed except for the trailing
    M = 0_1

  else

!   Pad the message block
    call this%padding( M, slen+1 )

  end if

! Add the length trailer
  call this%trailing( M, input_length, this%get_hash_trailing_size() )

! Update
  call this%update( M, H )


end subroutine hash_process_last_block


! Hashing base padding
pure subroutine hash_padding( M, position )

! Message block
  integer(kind=1), dimension(:), intent(inout) :: M

! The postion to start padding
  integer, intent(in) :: position

! Add the first bit
  M(position) = hash_z80

! Padd with zeroes to the end of the buffer
  M(position+1:) = hash_z00

end subroutine hash_padding


! Hashing base trailing
pure subroutine hash_trailing( M, input_length, trailing_size )

! Message block
  integer(kind=1), dimension(:), target, intent(inout) :: M

! The meassage total input length
  integer, intent(in) :: input_length

! Trailing size
  integer, intent(in) :: trailing_size

! Local variables
  integer(kind=8), dimension(2) :: input_bit_length
  integer :: mlength
  integer(kind=1), dimension(:), allocatable :: Mtrail
  integer :: n

! Initialise
  input_bit_length = [ 8_8 * input_length, 0_8 ]
  mlength = size(M)

! Assign the length
  n = trailing_size - 1

! Generate the trailing bytes
  Mtrail = transfer( input_bit_length, Mtrail )

! Set the trailing from the trailing bytes in reverse order estarting from the end
  M(mlength-n+1:) = Mtrail(n:1:-1)

end subroutine hash_trailing

end module m_hash
