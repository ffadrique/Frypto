module unit_m_shasum_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_shasum
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
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_string
  use m_xfunit
  use m_messages
  use m_util_convert
  
  use m_shasum

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_shasum_test_001

  public unit_m_shasum_test_002

  public unit_m_shasum_test_003

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_shasum'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_shasum_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_shasum) :: object

! Local variables
  character(len=:), allocatable :: path, chash
  type(t_string) :: shash
  
! Reset error handling structures
  call msg%reset_error()

 ! Generate the path to the input file
  path = trim(manager%get_unit_data_dir()) // '/grace04c.xml'
  
! Initialise the hashing object by default
  object = shasum()
  
! Verify the hash
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Single file (default)', chash, '8A09D3ED16FF46054E6B78EB840F1097DE192AC6', ignorecase=.true. )
  else
    call ut%assert_fail( 'Single file (default)' )
  end if
    
! Initialise the hashing object with SHA-1
  object = shasum(shasum_1)
  
! Verify the hash
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Single file (sha1)', chash, '8A09D3ED16FF46054E6B78EB840F1097DE192AC6', ignorecase=.true. )
  else
    call ut%assert_fail( 'Single file (sha1)' )
  end if
    
! Initialise the hashing object with SHA-256
  object = shasum(shasum_256)
  
! Verify the hash
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Single file (sha256)', chash, '299C1D0C45776B870F4CA5E35C051D83EADE8CF74E4AA3AD890E8F52992C12E9', ignorecase=.true. )
  else
    call ut%assert_fail( 'Single file (sha256)' )
  end if
    
! Initialise the hashing object with SHA-384
  object = shasum(shasum_384)
  
! Verify the hash
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Single file (sha384)', chash, 'e6820000619165b6fb2c0ac9ea4eb2a51596556889e8b4e8c96b39ace473c9da7ad5757b27b83358a6b388bddba14f13', ignorecase=.true. )
  else
    call ut%assert_fail( 'Single file (sha384)' )
  end if
    
! Initialise the hashing object with SHA-512
  object = shasum(shasum_512)
  
! Verify the hash
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Single file (sha512)', chash, '5238ad6afe2ce6f74f36dc691e2adfa9c34d2856839b3a127163a4349e06bc92ae7d7d84d6ead47e6047b9fa6f4b64dcec5446df223e79579b72392930884be1', ignorecase=.true. )
  else
    call ut%assert_fail( 'Single file (sha512)' )
  end if
    
! Initialise with wrong version (fllas back to SHA-1)
  object = shasum(-7)
  
! Verify the hash
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Single file (wrong version)', chash, '8A09D3ED16FF46054E6B78EB840F1097DE192AC6', ignorecase=.true. )
  else
    call ut%assert_fail( 'Single file (wrong version)' )
  end if

! Verify the hash (file not found)
  call object%encode_file( "", chash, msg )
  if( msg%on_error() ) then
    call ut%assert_pass( 'File not found detected' )
    call msg%reset_error()
  end if
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_shasum_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_shasum_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_shasum) :: object

! Local variables
  character(len=:), dimension(:), allocatable :: path, chash, cref
  type(t_string), dimension(:), allocatable :: spath, shash, sref
  integer :: i
  
! Reset error handling structures
  call msg%reset_error()

 ! Generate the path to the input file
  allocate( spath, source= [ trim(manager%get_unit_data_dir()) // '/grace04c.xml', &
                             trim(manager%get_unit_data_dir()) // '/grace04c.encrypted', &
                             trim(manager%get_unit_data_dir()) // '/grace04c.decrypted' ]  )
  allocate( sref, source= [ string('8A09D3ED16FF46054E6B78EB840F1097DE192AC6'), &
                            string('7E83EC8AD2297FF1E5C7E727C3BACD5DADA5545F'), &
                            string('AA8AF0F5ACFA59ADC24CF6FCA0723A811DE664F7') ] )
  allocate( character(len=128) :: path(3) )
  allocate( character(len=256) :: cref(3) )
  do i = 1, 3
    path(i) = spath(i)%character()
    cref(i) = sref(i)%character()
  end do
  
! Initialise the hashing object by default
  object = shasum()
  
! Verify the hashes
  call object%encode_file( path, chash, msg )
  if( .not. msg%on_error() ) then
    call ut%assert_equal( 'Multiple files', chash, cref, ignorecase=.true. )
  end if
    
! Verify the hashes with one file not found
  path(2) = ' '
  cref(2) = repeat("*", 40 )
  call object%encode_file( path, chash, msg )
  call ut%assert_equal( 'Multiple files (fail)', chash, cref, ignorecase=.true. )
  call msg%reset_error()
    
! Reset error handling structures
  call msg%reset_error()

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_shasum_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_shasum_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_shasum) :: object

! Local varialbes
  character(len=:), allocatable :: text
  integer(kind=1), dimension(:), allocatable :: array
  integer(kind=1), allocatable, dimension(:) :: bytes
  
! Initialise
  object = shasum( shasum_512 )
  allocate( bytes(object%get_hash_size()), source=0_1 )
  
! Reset error handling structures
  call msg%reset_error()

  
! FIPS 180-2 sample for long message
  allocate( array(1000000) )
  array = ichar('a',1)
  bytes = object%encode( array )
  call ut%assert_equal( "Buffer ", &
                        bytes_to_hex(bytes,.true.), &
                        'e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b' )
  deallocate(bytes)

! FIPS 180-2 sample for two blocks from character string
  text = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  bytes = object%encode( text )
  call ut%assert_equal( "Text", &
                        bytes_to_hex(bytes,.true.), &
                        '8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909' )
  deallocate(bytes)
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_shasum_test_003

end module unit_m_shasum_tests
