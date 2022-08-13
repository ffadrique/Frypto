module m_uuid

! -----------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : UUID Universally Unique Identifier (128-bit)
!             Ref: https://en.wikipedia.org/wiki/Universally_unique_identifier
!                  https://tools.ietf.org/html/rfc4122
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
! -----------------------------------------------------------------------------

! Dependencies ----------------------------------------------------------------

  use m_util_convert
  use m_object

  use m_iso8601_date_time
  use m_sha1
  use m_md5

  implicit none

! Public/Private declarations -------------------------------------------------

  private
  public t_uuid, uuid

  public uuid_namespace_dns, &
         uuid_namespace_url, &
         uuid_namespace_iso_oid, &
         uuid_namespace_x500

  public uuid_nil

! Module declarations ---------------------------------------------------------

! Variant values
 integer(kind=1), parameter :: uuid_variant_rfc4122 = int(z'08',1)

! Variant bit masks
 integer(kind=2), parameter :: uuid_variant_mask_get = int(z'8000',2)
 integer(kind=2), parameter :: uuid_variant_mask_set = int(z'3fff',2)

! Version bit masks
 integer(kind=2), parameter :: uuid_version_mask_get = int(z'f000',2)
 integer(kind=2), parameter :: uuid_version_mask_set = int(z'0fff',2)

! Default UUID separator
  character, parameter :: uuid_default_separator = '-'

! Curly brackets
  character, parameter :: uuid_lbracket = '{'
  character, parameter :: uuid_rbracket = '}'

! UUID Universally Unique Identifier (128-bit)
  type, extends(t_object) :: t_uuid
    private

!     Integer giving the low 32 bits of the time
      integer(kind=4) :: time_low = 0_4

!     Integer giving the middle 16 bits of the time
      integer(kind=2) :: time_mid = 0_2

!     4-bit "version" in the most significant bits, followed by the high 12 bits of the time
      integer(kind=2) :: time_hi_and_version = 0_2

!     1-3 bit "variant" in the most significant bits, followed by the 13-15 bit clock sequence
      integer(kind=2) :: clock_seq_hi_low = 0_2

!     The 48-bit node id
      integer(kind=1), dimension(6) :: node = 0_1

    contains

!     Getters
      procedure :: get_time_low => uuid_get_time_low
      procedure :: get_time_mid => uuid_get_time_mid
      procedure :: get_time_hi_and_version => uuid_get_time_hi_and_version
      procedure :: get_clock_seq_hi_low => uuid_get_clock_seq_hi_low
      procedure :: get_node => uuid_get_node
      procedure :: get_version => uuid_get_version
      procedure :: get_variant => uuid_get_variant

!     Comparison operators
      generic :: operator(==) => uuid_equal_to
      procedure, private :: uuid_equal_to
      generic :: operator(/=) => uuid_not_equal_to
      procedure, private :: uuid_not_equal_to

!     Set the node from the MAC in the UUID
      procedure, private :: set_node_from_mac => uuid_set_node_from_mac

!     Set the time stamp in the UUID
      procedure, private :: set_timestamp => uuid_set_timestamp

!     Set a random clock in the UUID
      procedure, private :: set_random_clock => uuid_set_random_clock

!     Set the version in the UUID
      procedure, private :: set_version => uuid_set_version

!     Set the variant in the UUID
      procedure, private :: set_variant => uuid_set_variant

!     Assign the UUID fields from an array of bytes
      procedure, private :: from_bytes => uuid_from_bytes

!     Generate the UUID as a character string
      procedure :: to_string => uuid_to_string

!     Generate the UID as a stream of bytes
      procedure :: to_bytes => uuid_to_bytes

  end type t_uuid

! Constructor interface
  interface uuid
    module procedure uuid_default
    module procedure uuid_time_and_mac
    module procedure uuid_namespace_and_name
    module procedure uuid_name
  end interface uuid

! Nil UUID
! RFC 4122: 4.1.7.  Nil UUID
! t_uuid( z'00000000', z'0000', z'0000', z'0000', [ z'00', z'00', z'00', z'00', z'00', z'00' ] )
  type(t_uuid), parameter :: uuid_nil = &  ! 00000000-0000-0000-0000-000000000000
    t_uuid( 0, 0_2, 0_2, 0_2, [ 0_1, 0_1, 0_1, 0_1, 0_1, 0_1 ] )

! Predefined UUID for name generation
! RFC 4122: Appendix C - Some Name Space IDs

! Name string is a fully-qualified domain name
! t_uuid( z'6ba7b810', z'9dad', z'11d1', z'80b4', [ z'00', z'c0', z'4f', z'd4', z'30', z'c8' ] )
  type(t_uuid), parameter :: uuid_namespace_dns = &  ! 6ba7b810-9dad-11d1-80b4-00c04fd430c8
    t_uuid( 1806153744, -25171_2, 4561_2, -32588_2, [ 0_1, -64_1, 79_1, -44_1, 48_1, -56_1 ] )

! Name string is a URL
! t_uuid( z'6ba7b811', z'9dad', z'11d1', z'80b4', [ z'00', z'c0', z'4f', z'd4', z'30', z'c8' ] )
  type(t_uuid), parameter :: uuid_namespace_url = &  ! 6ba7b811-9dad-11d1-80b4-00c04fd430c8
    t_uuid( 1806153745, -25171_2, 4561_2, -32588_2, [ 0_1, -64_1, 79_1, -44_1, 48_1, -56_1 ] )

! Name string is an ISO OID
! t_uuid( z'6ba7b812', z'9dad', z'11d1', z'80b4', [ z'00', z'c0', z'4f', z'd4', z'30', z'c8' ] )
  type(t_uuid), parameter :: uuid_namespace_iso_oid = & ! 6ba7b812-9dad-11d1-80b4-00c04fd430c8
    t_uuid( 1806153746, -25171_2, 4561_2, -32588_2, [ 0_1, -64_1, 79_1, -44_1, 48_1, -56_1 ] )

! Name string is an X.500 DN (in DER or a text output format)
! t_uuid( z'6ba7b814', z'9dad', z'11d1', z'80b4', [ z'00', z'c0', z'4f', z'd4', z'30', z'c8' ] )
  type(t_uuid), parameter :: uuid_namespace_x500 = &  ! 6ba7b814-9dad-11d1-80b4-00c04fd430c8
    t_uuid( 1806153748, -25171_2, 4561_2, -32588_2, [ 0_1, -64_1, 79_1, -44_1, 48_1, -56_1 ] )

! Implementation --------------------------------------------------------------

contains

! Default constructor
! RFC 4122: 4.4 Algorithms for Creating a UUID from Truly Random or Pseudo-Random Numbers
! Random number generation must be controlled from the calling function
elemental impure function uuid_default(  ) result(res)

! Returned object
  type(t_uuid) :: res

! Local variables
  real, dimension(6) :: rand

! Generate a random node
! RFC 4122: 4.1.6 Node
  call random_number( rand )
  res%node = int( 256 * rand, kind=1 )

! Generate the time stamp from the current date-time
! RFC 4122: 4.1.4 Timestamp
  call res%set_timestamp( iso8601_date_time_now() )

! Generate the node from the mac
! RFC 4122: 4.1.5 Clock Sequence
  call res%set_random_clock()

! Set the variant
! RFC 4122: 4.1.1 Variant
  call res%set_variant( uuid_variant_rfc4122 )

! This is a version 4 UUID
! RFC 4122: 4.1.3 Version
  call res%set_version(4_1)

end function uuid_default


! Constructor from time and MAC
! RFC 4122: 4.2 Algorithms for Creating a Time-Based UUID
! Random number generation must be controlled from the calling function
elemental impure function uuid_time_and_mac( time, mac ) result(res)

! Time in ISO8601 structure
  type(t_iso8601_date_time), intent(in) :: time

! MAC address
  character(len=*), intent(in) :: mac

! Returned object
  type(t_uuid) :: res

! Generate the node
! RFC 4122: 4.1.6 Node
! RFC 4122: 4.2.2 Generation details
  call res%set_node_from_mac( mac )

! Generate the time stamp
! RFC 4122: 4.1.4 Timestamp
! RFC 4122: 4.2.2 Generation details
  call res%set_timestamp( time )

! Generate the node from the mac
! RFC 4122: 4.1.5 Clock Sequence
! RFC 4122: 4.2.2 Generation details
  call res%set_random_clock()

! Set the variant
! RFC 4122: 4.1.1 Variant
! RFC 4122: 4.2.2 Generation details
  call res%set_variant( uuid_variant_rfc4122 )

! This is a version 1 UUID
! RFC 4122: 4.1.3 Version
! RFC 4122: 4.2.2 Generation details
  call res%set_version(1_1)

end function uuid_time_and_mac


! Constructor from name and namespace
! RFC 4122: 4.3 Algorithm for Creating a Name-Based UUID
elemental impure function uuid_namespace_and_name( namespace, name, version ) result(res)

! Namespace
  type(t_uuid), intent(in) :: namespace

! Name
  character(len=*), intent(in) :: name

! UUID version (only 3 and 5 for this constructor)
  integer, optional, intent(in) :: version

! Returned object
  type(t_uuid) :: res

! Local variables
  integer(kind=1), dimension(16) :: code
  integer(kind=1), dimension(:), allocatable :: nsbytes, nbytes, bytes

! Get the namesapce UUID
  allocate( nsbytes(16), source=namespace%to_bytes() )

! Generate the byte sequence for the name
  allocate( nbytes(len(name)), source=character_to_bytes(name) )

! Concatenate name and namespace
  allocate( bytes(size(nsbytes)+size(nbytes)) )
  bytes(:size(nsbytes)) = nsbytes
  bytes(size(nsbytes)+1:) = nbytes

! Encode the bytes
  code = uuid_encode_bytes( bytes, version )

! Assign the coded full name
  call res%from_bytes( code )

! Check if the UUID is nil
  if( res /= uuid_nil ) then

!   Set the variant
!   RFC 4122: 4.1.1 Variant
    call res%set_variant( uuid_variant_rfc4122 )

!   This is a version 1 UUID
!   RFC 4122: 4.1.3 Version
    if( present(version) ) then
      call res%set_version(int(version,1))
    else
      call res%set_version(5_1)
    end if

  end if

end function uuid_namespace_and_name


! Constructor for name
! RFC 4122: 4.3 Algorithm for Creating a Name-Based UUID
elemental impure function uuid_name( name, version ) result(res)

! Namespace
  character(len=*), intent(in) :: name

! UUID version (only 3 and 5 for this constructor)
  integer, optional, intent(in) :: version

! Returned object
  type(t_uuid) :: res

! Local variables
  integer(kind=1), dimension(16) :: code

! Code the input namespace
  code = uuid_encode_bytes( character_to_bytes(name), version )

! Assign the coded full name
  call res%from_bytes( code )

! Check if UUID is nil
  if( res /= uuid_nil ) then

!   Set the variant
!   RFC 4122: 4.1.1 Variant
    call res%set_variant( uuid_variant_rfc4122 )

!   This is a version 1 UUID
!   RFC 4122: 4.1.3 Version
    if( present(version) ) then
      call res%set_version(int(version,1))
    else
      call res%set_version(5_1)
    end if

  end if

end function uuid_name


! Encode a byte array
pure function uuid_encode_bytes( bytes, version ) result(res)

! Array of bytes
  integer(kind=1), dimension(:), intent(in) :: bytes

! Version
  integer, optional, intent(in) :: version

! Encoded byte array
  integer(kind=1), dimension(16) :: res

! Local variables
  type(t_sha1) :: esha1
  type(t_md5) :: emd5
  integer(kind=1) :: ver

! Determine the version
! By default, prefer SHA-1 using version 5
  if( present(version) ) then
    ver = int( version, kind=1 )
  else
    ver = 5_1
  end if

! Select the version
  select case(ver)

!   Encode with MD5 (version 3)
    case(3)
      emd5 = md5()
      call emd5%encode(bytes)
      res = emd5%get_hash()

!   Encode with SHA-1 (version 5 or default)
    case(5)
      esha1 = sha1()
      res = esha1%encode(bytes)

!   Invalid version
    case default
      res = 0_1

  end select

end function uuid_encode_bytes


! Comparison operator (==)
elemental function uuid_equal_to( this, right ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Right operand
  type(t_uuid), intent(in) :: right

! Comparison result
  logical :: res

! Compare UUIDs
  res = .not. ( this /= right )

end function uuid_equal_to


! Comparison operator [=)
elemental function uuid_not_equal_to( this, right ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Right operand
  type(t_uuid), intent(in) :: right

! Comparison result
  logical :: res

! Compare UUIDs
  res = ( this%time_low /= right%time_low ) .or. &
        ( this%time_mid /= right%time_mid ) .or. &
        ( this%time_hi_and_version /= right%time_hi_and_version ) .or. &
        ( this%clock_seq_hi_low /= right%clock_seq_hi_low ) .or. &
        any( this%node /= right%node )

end function uuid_not_equal_to


! Generate the node from the mac
! RFC 4122: 4.1.6 Node
! RFC 4122: 4.2.2 Generation details
pure subroutine uuid_set_node_from_mac( this, mac )

! Calling object
  class(t_uuid), intent(inout) :: this

! MAC address
  character(len=*), intent(in) :: mac

! Local variables
  integer :: i
  character(len=:), allocatable :: cmac

! Compress the MAC
  cmac = mac
  i = scan( cmac, ':-.' )
  do while( i > 0 )
    cmac(i:) = cmac(i+1:)
    i = scan( cmac, ':-.' )
  end do

! Set the node
  this%node = hex_to_bytes( trim(cmac) )

end subroutine uuid_set_node_from_mac


! Generate the node from the mac
! RFC 4122: 4.1.4 Timestamp
! RFC 4122: 4.2.2 Generation details
pure subroutine uuid_set_timestamp( this, time )

! Calling object
  class(t_uuid), intent(inout) :: this

! Time as ISO8601 structure
  type(t_iso8601_date_time), intent(in) :: time

! Day of the year at the beginning of each month
  integer, parameter, dimension(12) :: doy = [ &
      0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ]

! Local variables
  integer, parameter :: y0 = 1582
  integer, parameter :: m0 = 10
  integer, parameter :: d0 = 10
  integer :: y, m, d, h, mi, s, mis, mus, ns
  integer :: nleapy, ndays
  integer(kind=8) :: day100ns, t64

! Initialise locals
  y = time%get_year()
  m = time%get_month()
  d = time%get_day()

! Compute the base number of leap days (asume year is after 1900)
  nleapy = ( y - y0 ) / 4

! Remove the days for century non-leap years (1700, 1800 and 1900)
  select case(y)
    case(:1699)
    case(1700:1799)
      nleapy = nleapy - 1
    case(1800:1899)
      nleapy = nleapy - 2
    case(1900:2099)
      nleapy = nleapy - 3
    case(2100:)
      nleapy = nleapy - 4
  end select

! Add a leap day if in a leap year after February 28
  if( mod( y, 4 ) == 0 .and. d >= (31 + 28) ) then
    nleapy = nleapy + 1
  end if

! Compute the number of days from the reference epoch 1582-10-15
  ndays = ( y - y0 - 1 ) * 365 + ( 16 + 30 + 31 ) + doy(m) + d + nleapy

! Compute the timestamp (100-ns steps)
  t64 = int(ndays,8) * 24_8 * 60_8 * 60_8 * 10000000_8
  h = time%get_hour()
  mi = time%get_minute()
  s = time%get_second()
  mis = time%get_millisecond()
  mus = time%get_microsecond()
  ns = time%get_nanosecond()
  day100ns = int(h,8)
  day100ns = day100ns * 60_8 + int(mi,8)
  day100ns = day100ns * 60_8 + int(s,8)
  day100ns = day100ns * 1000_8 + int(mis,8)
  day100ns = day100ns * 1000_8 + int(mus,8)
  day100ns = day100ns * 1000_8 + int(ns,8)
  t64 = t64 + day100ns / 100_8

! Set the UUID fields
  this%time_low = int( ibits( t64, 0, 32 ), kind=4 )
  this%time_mid = int( ibits( t64, 32, 16 ), kind=2 )
  this%time_hi_and_version = int( ibits( t64, 48, 12 ), kind=2 )

end subroutine uuid_set_timestamp


! Generate the node from the mac
! RFC 4122: 4.1.5 Clock Sequence
! RFC 4122: 4.2.2 Generation details
subroutine uuid_set_random_clock( this )

! Calling object
  class(t_uuid), intent(inout) :: this

! Local varialbles
  real(kind=4) :: clock
  integer(kind=4) :: iclock

! Generate the random clock
  call random_number(clock)

! Retain the 14 least significant bits
  iclock = int( clock * 2**16 )
  iclock = ibclr( iclock, 15 )
  iclock = ibclr( iclock, 14 )
  this%clock_seq_hi_low = int( iclock, kind=2 )

end subroutine uuid_set_random_clock


! Set the version
! RFC 4122: 4.1.3 Version
! RFC 4122: 4.2.2 Generation details
pure subroutine uuid_set_version( this, version )

! Calling object
  class(t_uuid), intent(inout) :: this

! Version number
  integer(kind=1), intent(in) :: version

! Set the version
! Replace the 4 most significant bits in time_hi_and_version by version
  this%time_hi_and_version = ior( iand( this%time_hi_and_version, uuid_version_mask_set ), &
                                  ishft(int(version,2),12) )

end subroutine uuid_set_version


! Set the variant
! RFC 4122: 4.1.1 Variant
! RFC 4122: 4.2.2 Generation details
pure subroutine uuid_set_variant( this, variant )

! Calling object
  class(t_uuid), intent(inout) :: this

! Variant number
  integer(kind=1), intent(in) :: variant

! Set the variant
! Replace the 3 most significant bits in time_hi_and_version by version
  this%clock_seq_hi_low = ior( iand( this%clock_seq_hi_low, uuid_variant_mask_set ), &
                               ishft(int(variant,2),12) )

end subroutine uuid_set_variant


! Generate the UUID string
elemental function uuid_to_string( this, separator, curly_brackets, capitalise ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Select the field separator (defaults to -)
  character, optional, intent(in) :: separator

! Put curly brackets around the UUID (defaults to false)
  logical, optional, intent(in) :: curly_brackets

! Generate the UUID in uppercase (default to false, i.e. lowercase)
  logical, optional, intent(in) :: capitalise

! UUID formatted string
  character(len=38) :: res

! Local variables
  character :: sep
  character :: lbracket, rbracket
  character(len=*), parameter :: fmt = "(a,z8.8,3(a,z4.4),a,6z2.2,a)"

! Select the separator
  if( present(separator) ) then
    sep = separator
  else
    sep = uuid_default_separator
  end if

! Select the surrounding curly brackets
  if( present(curly_brackets) ) then
    lbracket = uuid_lbracket
    rbracket = uuid_rbracket
  else
    lbracket = ''
    rbracket = ''
  end if

! Generate the string
  write( res, fmt ) lbracket, &
                    this%time_low, sep, &
                    this%time_mid, sep, &
                    this%time_hi_and_version, sep, &
                    this%clock_seq_hi_low, sep, &
                    this%node, &
                    rbracket

! Generate with the selected case
  res = lowercase(trim(adjustl(res)))
  if( present(capitalise) ) then
    if( capitalise ) then
      res = uppercase(res)
    end if
  end if

end function uuid_to_string


! Generate the UUID array of bytes
pure function uuid_to_bytes( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! UUID byte array
  integer(kind=1), dimension(16) :: res

! Generate the array of bytes
  res(4:1:-1) = transfer( this%time_low, res(1:4) )
  res(6:5:-1) = transfer( this%time_mid, res(5:6) )
  res(8:7:-1) = transfer( this%time_hi_and_version, res(7:8) )
  res(10:9:-1) = transfer( this%clock_seq_hi_low, res(9:10) )
  res(11:16) = this%node

end function uuid_to_bytes


! Assign a UUID from bytes
pure subroutine uuid_from_bytes( this, bytes )

! Calling object
  class(t_uuid), intent(inout) :: this

! UUID byte array
  integer(kind=1), dimension(:), intent(in) :: bytes

! Set time_low
  this%time_low = transfer( bytes(4:1:-1), this%time_low )

! Set time_mid
  this%time_mid = transfer( bytes(6:5:-1), this%time_mid )

! Set time_hi_and_version
  this%time_hi_and_version = transfer( bytes(8:7:-1), this%time_hi_and_version )

! Set clock_seq_hi_low
  this%clock_seq_hi_low = transfer( bytes(10:9:-1), this%clock_seq_hi_low )

! Set the node
  this%node = bytes(11:16)

end subroutine uuid_from_bytes


! Getter for time_low
elemental function uuid_get_time_low( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=4) :: res

! Return value
  res = this%time_low

end function uuid_get_time_low


! Getter for time_mid
elemental function uuid_get_time_mid( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=2) :: res

! Return value
  res = this%time_mid

end function uuid_get_time_mid


! Getter for time_hi_and_version
elemental function uuid_get_time_hi_and_version( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=2) :: res

! Return value
  res = this%time_hi_and_version

end function uuid_get_time_hi_and_version


! Getter for clock_seq_hi_low
elemental function uuid_get_clock_seq_hi_low( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=2) :: res

! Return value
  res = this%clock_seq_hi_low

end function uuid_get_clock_seq_hi_low


! Getter for node
pure function uuid_get_node( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=1), dimension(6) :: res

! Return value
  res = this%node

end function uuid_get_node


! Get the version
elemental function uuid_get_version( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=1) :: res

! Return value
  res = int( ishft( iand( this%time_hi_and_version, uuid_version_mask_get ), -12 ), kind=1 )

end function uuid_get_version


! Get the variant
elemental function uuid_get_variant( this ) result(res)

! Calling object
  class(t_uuid), intent(in) :: this

! Getter result
  integer(kind=1) :: res

! Return value
  res = int( ishft( iand( this%clock_seq_hi_low, uuid_variant_mask_get ), -12 ), 1 )

end function uuid_get_variant

end module m_uuid

