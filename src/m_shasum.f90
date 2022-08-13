module m_shasum

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Interface for file hashing and hash checking
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
  use m_file_handler
  use m_messages
  
  use m_hash
  use m_sha1
  use m_sha256
  use m_sha384
  use m_sha512

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_shasum, shasum
  public shasum_1, shasum_256, shasum_384, shasum_512

!- End of Public/Private declarations -----------------------------------------

  !- Start of module variable declarations --------------------------------------

! Eumeration for algorithm selection
  integer, parameter :: shasum_1   = 1
  integer, parameter :: shasum_256 = 256
  integer, parameter :: shasum_384 = 384
  integer, parameter :: shasum_512 = 512

! Hashing algorithm frontend
  type, extends(t_object) :: t_shasum
    private

!     SHA algorithm version
      integer :: version = shasum_1

!     Hashing object
      class(t_hash), allocatable :: ohash

    contains

!     Encode a character string or from open file
      generic :: encode => shasum_encode_string, shasum_encode_buffer, shasum_encode_unit
      procedure, private :: shasum_encode_string
      procedure, private :: shasum_encode_buffer
      procedure, private :: shasum_encode_unit

!     Encode files
      generic :: encode_file => shasum_encode_file, shasum_encode_files
      procedure, private :: shasum_encode_file
      procedure, private :: shasum_encode_files

!     Get the size (bytes) of the hash
      procedure :: get_hash_size => shasum_get_hash_size

  end type t_shasum

! Constructor interface
  interface shasum
    module procedure shasum_default
    module procedure shasum_general
  end interface shasum

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
function shasum_default() result(res)

! Constructed object
  type(t_shasum) :: res

! Default to SHA-1
  res%version = shasum_1
  allocate( res%ohash, source=sha1() )

end function shasum_default


! Default constructor
function shasum_general( version ) result(res)

! The SHA algorithm version (from enumeration)
  integer, intent(in) :: version

! Constructed object
  type(t_shasum) :: res

! Store the sha version
  res%version = shasum_1

! Select the algorithm
  select case( version )

!   SHA-1
    case( shasum_1 )
      allocate( res%ohash, source=sha1() )

!   SHA-256
    case( shasum_256 )
      allocate( res%ohash, source=sha256() )

!   SHA-384
    case( shasum_384 )
      allocate( res%ohash, source=sha384() )

!   SHA-512
    case( shasum_512 )
      allocate( res%ohash, source=sha512() )

!   Default to SHA-1
    case default
      res%version = shasum_1
      allocate( res%ohash, source=sha1() )

  end select

end function shasum_general


! Encode from input byte buffer
pure function shasum_encode_buffer( this, buffer ) result(res)

! The hash structure
  class(t_shasum), intent(in) :: this

! The string to digest
  integer(kind=1), dimension(:), intent(in) :: buffer

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Encode the buffer with the selected algorithm
  res = this%ohash%encode( buffer )

end function shasum_encode_buffer


! Encode from input character string
pure function shasum_encode_string( this, s ) result(res)

! The hash structure
  class(t_shasum), intent(in) :: this

! The string to digest
  character(len=*), intent(in) :: s

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Convert to buffer and process
  res = this%encode( character_to_bytes(s) )

end function shasum_encode_string


! Encode from file contents
function shasum_encode_unit( this, unit, iostat ) result(res)

! The hash structure
  class(t_shasum), intent(in) :: this

! The Fortran unit for the file to digest
! File must have been opened with access=stream
  integer, intent(in) :: unit

! The read status
  integer, intent(out) :: iostat

! Resulting hash
  integer(kind=1), dimension(:), allocatable :: res

! Encode the buffer with the selected algorithm
  res = this%ohash%encode( unit, iostat )

end function shasum_encode_unit


! Encode files
subroutine shasum_encode_files( this, paths, hashes, msg )

! Calling object
  class(t_shasum), intent(in) :: this

! File full path
  character(len=*), dimension(:), intent(in) :: paths

! The resulting hash (in hex)
  character(len=:), dimension(:), allocatable, intent(out) :: hashes

! Processing error hndling
  type(t_messages), intent(inout) :: msg

! Local variables
  integer :: ipath
  type(t_messages) :: lmsg
  character(len=:), allocatable :: hash
  integer :: length

! Do a first initialisation to get the length of the resulting hash strings
  length = this%ohash%get_hash_size()

! Initialise hashes array (in hex it is twice as long as the hash size)
  allocate( character(len=2*length) :: hashes(size(paths)) )

! Loop on the files
  do ipath = 1, size(paths)

!   Encode this file
    call this%encode_file( paths(ipath), hash, lmsg )

!   Check error in this file
    if( .not. lmsg%on_error() ) then

!     Assign hash
      hashes(ipath) = hash

    else

!     Invalidate hash and report error
      hashes(ipath) = repeat("*", 2 * this%ohash%get_hash_size() )
      call msg%error( 'm_shassum', 'shasum_encode_files', 1, 'Failed on file "' // trim(paths(ipath)) // '"' )
      call lmsg%reset_error()

    end if

  end do

end subroutine shasum_encode_files


! Encode file
subroutine shasum_encode_file( this, path, hash, msg )

! Calling object
  class(t_shasum), intent(in) :: this

! File full path
  character(len=*), intent(in) :: path

! The resulting hash (in hex)
  character(len=:), allocatable, intent(out) :: hash

! Processing error hndling
  type(t_messages), intent(inout) :: msg

! Local variables
  type(t_file_handler) :: fhandler
  integer :: iostat
  integer(kind=1), allocatable, dimension(:) :: bhash

! Open the input file
  fhandler = file_handler( path )
  call fhandler%open( stream=.true. )
  if( fhandler%is_open() ) then

!   Encode the file
    allocate( bhash(this%ohash%get_hash_size()), source=0_1 )
    bhash = this%encode( fhandler%get_unit(), iostat )
    if( iostat == 0 ) then

!     Get the hash
      hash = bytes_to_hex( bhash, .true. )

    else

!     Report error
      call msg%error( 'm_shasum', 'shasum_encode_file', 2, &
                      'Hash algorithm failed on "' // trim(path) // '" with iostat=' // character(iostat) )

    end if

!   Close the input file
    call fhandler%close()

  else

!   Report error
    call msg%error( 'm_shasum', 'shasum_encode_file', 1, 'Cannot open "' // trim(path) // '"' )

  end if

end subroutine shasum_encode_file


! Get the hash size (bytes) for this algorithm
pure function shasum_get_hash_size( this ) result(res)

! Calling object
  class(t_shasum), intent(in) :: this

! Hash size
  integer :: res

! Return the size of the hash (in bytes)
  res = this%ohash%get_hash_size()

end function shasum_get_hash_size


end module m_shasum
