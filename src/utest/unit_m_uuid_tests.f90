module unit_m_uuid_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_uuid
!             Reference for UUID generation: https://generateuuid.com/
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

  use m_util_convert
  use m_iso8601_date_time
  
  use m_xfunit
  use m_messages

  use m_uuid

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_uuid_test_001

  public unit_m_uuid_test_002

  public unit_m_uuid_test_003

  public unit_m_uuid_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'iau'
  character(len=*), parameter :: module = 'm_uuid'
  
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

subroutine unit_m_uuid_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuid) :: uid

! Local variables
  character(len=:), allocatable :: mac
  type(t_iso8601_date_time) :: time
  character(len=36) :: suuid
  integer(kind=2) :: i2
 
! Reset error handling structures
  call msg%reset_error()

! Initialise
  time = iso8601_date_time( 2017, 6, 17, 23, 55, 37, 723, 432, 332 )
  mac = "00-50-56-C0-00-08"

! Nil UUID
  uid = uuid_nil
  call ut%assert_equal( 'Nil UUID (version 0)', uid%get_version(), 0_1 )
  call ut%assert_equal( 'Nil UUID (variant)', uid%get_variant(), 0_1 )
  call ut%assert_true( 'Is nil UUID', uid == uuid_nil )
  
! Default UUID (random)
  uid = uuid()
  call ut%assert_equal( 'Random UUID (version 4)', uid%get_version(), 4_1 )
  call ut%assert_equal( 'Random UUID (variant)', uid%get_variant(), 8_1 )

! Time/MAC UUID
  uid = uuid( time, mac )
  call ut%assert_equal( 'Time/MAC UUID (version 1)', uid%get_version(), 1_1 )
  call ut%assert_equal( 'Time/MAC UUID (variant)', uid%get_variant(), 8_1 )
  i2 = uid%get_clock_seq_hi_low()
  uid = uuid( time, mac )
  call ut%assert_false( 'Time/MAC clock sequence', i2 == uid%get_clock_seq_hi_low() )
  
! Name based UUID
  uid = uuid( 'ccsds:nwg:ndm' )
  call ut%assert_equal( 'Name based UUID (version default 5)', uid%get_version(), 5_1 )
  call ut%assert_equal( 'Name based UUID (variant)', uid%get_variant(), 8_1 )
  
! Name based UUID
  uid = uuid( 'ccsds:nwg:ndm', 5 )
  call ut%assert_equal( 'Name based UUID (version 5)', uid%get_version(), 5_1 )
  
! Name based UUID
  uid = uuid( 'ccsds:nwg:ndm', 3 )
  call ut%assert_equal( 'Name based UUID (version 3)', uid%get_version(), 3_1 )
  
! Namespace/name based UUID
  uid = uuid( uuid_namespace_url, 'cwe.ccsds.org' )
  call ut%assert_equal( 'Namespace/name based UUID (version default 5)', uid%get_version(), 5_1 )
  call ut%assert_equal( 'Namespace/name based UUID (variant)', uid%get_variant(), 8_1 )
  
! Namespace/name based UUID
  uid = uuid( uuid_namespace_url, 'cwe.ccsds.org', 5 )
  call ut%assert_equal( 'Namespace/name based UUID (version 5)', uid%get_version(), 5_1 )
  
! Namespace/name based UUID
  uid = uuid( uuid_namespace_url, 'cwe.ccsds.org', 3 )
  call ut%assert_equal( 'Namespace/name based UUID (version 3)', uid%get_version(), 3_1 )
  
! Namespace/name based UUID
  uid = uuid( uuid_namespace_url, 'cwe.ccsds.org', 6 )
  call ut%assert_equal( 'Namespace/name based UUID (version invalid)', uid%get_version(), 0_1 )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_uuid_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_uuid_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuid) :: uid

! Local variables
  character(len=:), allocatable :: mac
  type(t_iso8601_date_time) :: time
  character(len=36) :: suuid
  type(t_uuid), dimension(5) :: uuids
  integer :: i
  integer(kind=1), dimension(5) :: version

! Reset error handling structures
  call msg%reset_error()

! Initialise
  time = iso8601_date_time( 2017, 6, 17, 23, 55, 37, 723, 432, 332 )
  mac = "00-50-56-C0-00-08"

! Time/MAC UUID (generate 5)
  do i = 1, size(uuids)
    uuids(i) = uuid( iso8601_date_time( 2017, 6, 10+i), mac )
  end do
  version = uuids%get_version()
  call ut%assert_equal( 'Version', version, 1_1 )
  call ut%assert_equal( 'Variant', uuids%get_variant(), 8_1 )
  do i = 2, size(uuids)
    call ut%assert_false( 'UUID MAC '//trim(character(i))//' time low', uuids(i)%get_time_low() == uuids(1)%get_time_low() )
    call ut%assert_false( 'UUID MAC '//trim(character(i))//' time mid', uuids(i)%get_time_mid() == uuids(1)%get_time_mid() )
    call ut%assert_true( 'UUID MAC '//trim(character(i))//' time high/version', uuids(i)%get_time_hi_and_version() == uuids(1)%get_time_hi_and_version() )
    call ut%assert_equal( 'UUID MAC '//trim(character(i))//' node', uuids(i)%get_node(), uuids(1)%get_node() )
  end do

! Change MAC separator
  uid = uuid( time, mac )
  suuid = uid%to_string()
  suuid(20:23) = 'xxxx'
  call ut%assert_equal( 'UUID(00-50-56-C0-00-08)', suuid, '4b9b8593-52ef-11e7-xxxx-005056c00008', ignorecase=.true. )
  mac = "00:50:56:C0:00:08"
  uid = uuid( time, mac )
  suuid = uid%to_string()
  suuid(20:23) = 'xxxx'
  call ut%assert_equal( 'UUID(00:50:56:C0:00:08)', suuid, '4b9b8593-52ef-11e7-xxxx-005056c00008', ignorecase=.true. )
  mac = "005056C00008"
  uid = uuid( time, mac )
  suuid = uid%to_string()
  suuid(20:23) = 'xxxx'
  call ut%assert_equal( 'UUID(005056C00008)', suuid, '4b9b8593-52ef-11e7-xxxx-005056c00008', ignorecase=.true. )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_uuid_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_uuid_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuid) :: uid

! Local variables
  character(len=:), allocatable :: text
  character(len=36) :: suuid
 
! Reset error handling structures
  call msg%reset_error()

! Initialise
  text = "http://www.google.com"
  
! Name based UUID
  uid = uuid( uuid_namespace_dns, text, version=3 )
  suuid = uid%to_string()
  call ut%assert_equal( 'DNS v3', suuid, "933fd48a-c016-3055-937b-4e10ae8d100b", ignorecase=.true. )
  uid = uuid( uuid_namespace_url, text, version=3 )
  suuid = uid%to_string()
  call ut%assert_equal( 'URL v3', suuid, "53ab554b-3d43-3d57-aad4-e2ef014755af", ignorecase=.true. )
  uid = uuid( uuid_namespace_iso_oid, text, version=3 )
  suuid = uid%to_string()
  call ut%assert_equal( 'IO OID v3', suuid, "8e6b6a52-6243-3ec7-a12e-c5c46efdfe5d", ignorecase=.true. )
  uid = uuid( uuid_namespace_x500, text, version=3 )
  suuid = uid%to_string()
  call ut%assert_equal( 'X500 v3', suuid, "437a4b5e-a64c-3c59-b337-2bef702d45f3", ignorecase=.true. )
  uid = uuid( uuid_namespace_dns, text, version=5 )
  suuid = uid%to_string()
  call ut%assert_equal( 'DNS v5', suuid, "c51ed628-aa87-5213-b6a3-5754c37b7413", ignorecase=.true. )
  uid = uuid( uuid_namespace_url, text )
  suuid = uid%to_string()
  call ut%assert_equal( 'URL v5', suuid, "90d222de-5d7c-5ced-a245-d09badfd335e", ignorecase=.true. )
  uid = uuid( uuid_namespace_iso_oid, text, version=5 )
  suuid = uid%to_string()
  call ut%assert_equal( 'IO OID v5', suuid, "df65da1c-a04a-5bab-841a-e87753378510", ignorecase=.true. )
  uid = uuid( uuid_namespace_x500, text )
  suuid = uid%to_string()
  call ut%assert_equal( 'X500 v5', suuid, "cc7a9eb6-92f6-5c90-b5f4-354341c7c6fc", ignorecase=.true. )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_uuid_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_uuid_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuid) :: uid

! Local variables
  character(len=:), allocatable :: text
  character(len=:), allocatable :: suuid
 
! Reset error handling structures
  call msg%reset_error()

! Initialise
  text = "http://www.google.com"
  
! Name based UUID
  uid = uuid( uuid_namespace_dns, text, version=3 )
  suuid = uid%to_string( separator=':' )
  call ut%assert_equal( "Separator ':'", suuid, "933fd48a:c016:3055:937b:4e10ae8d100b", ignorecase=.true. )
  suuid = uid%to_string( separator=' ' )
  call ut%assert_equal( "Separator ' '", suuid, "933fd48a c016 3055 937b 4e10ae8d100b", ignorecase=.true. )
  suuid = uid%to_string( curly_brackets=.true. )
  call ut%assert_equal( "Curly brackets", suuid, "{933fd48a-c016-3055-937b-4e10ae8d100b}", ignorecase=.true. )
  suuid = uid%to_string( curly_brackets=.false. )
  call ut%assert_equal( "No curly brackets", suuid, "{933fd48a-c016-3055-937b-4e10ae8d100b}", ignorecase=.true. )
  suuid = uid%to_string( separator=':', curly_brackets=.true. )
  call ut%assert_equal( "Curly brackets; Separator ':'", suuid, "{933fd48a:c016:3055:937b:4e10ae8d100b}", ignorecase=.true. )
  suuid = uid%to_string( capitalise=.true. )
  call ut%assert_equal( "Capitalise", suuid, "933FD48A-C016-3055-937B-4E10AE8D100B" )
  suuid = uid%to_string( capitalise=.false. )
  call ut%assert_equal( "Do not capitalise", suuid, "933fd48a-c016-3055-937b-4e10ae8d100b" )
  suuid = uid%to_string( separator=':', curly_brackets=.true., capitalise=.true. )
  call ut%assert_equal( "All options", suuid, "{933FD48A:C016:3055:937B:4E10AE8D100B}" )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_uuid_test_004

end module unit_m_uuid_tests
