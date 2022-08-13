module unit_m_crc32_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_crc32
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
  
  use m_xfunit
  use m_messages

  use m_crc32

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_crc32_test_001

  public unit_m_crc32_test_002

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_crc32'
  
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

subroutine unit_m_crc32_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_crc32) :: object

! Local variables
  integer(kind=1), dimension(9) :: bytes = ichar( [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ] )
  integer :: crc, crcref

! Reset error handling structures
  call msg%reset_error()

! Initialise the CRC32 object
  object = crc32()
  
! Compute the CRC for the bytes
  crc = object%encode( bytes )

! Assertion
  crcref = int( z'CBF43926' )
  call ut%assert_equal( 'CRC for bytes', hex(crc,.true.), hex(crcref,.true.) )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_crc32_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crc32_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_crc32) :: object

! Local variables
  character(len=9) :: c = '123456789'
  integer :: crc, crcref

! Reset error handling structures
  call msg%reset_error()

! Initialise the CRC32 object
  object = crc32()
  
! Compute the CRC for the bytes (auto-initialisation)
  crc = object%encode( c )

! Assertion
  crcref = int( z'CBF43926' )
  call ut%assert_equal( 'CRC for character string', hex(crc,.true.), hex(crcref,.true.) )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_crc32_test_002

end module unit_m_crc32_tests
