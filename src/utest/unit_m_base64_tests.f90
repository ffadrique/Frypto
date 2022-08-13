module unit_m_base64_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_base64
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

  use m_base64

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_base64_test_001
  public unit_m_base64_test_002
  public unit_m_base64_test_003
  public unit_m_base64_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_base64'
  
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

subroutine unit_m_base64_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base64) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise locals
  clocal = "ABCDEFGH"
  bytes = character_to_bytes( clocal )
  
! Initialise
  b64 = base64()
  
! Encode one byte
  call b64%encode( bytes(1:1), cbytes )
  call ut%assert_equal( 'Encode 1 byte', bytes_to_character(cbytes), "QQ==" )
  
! Encode two bytes
  call b64%encode( bytes(1:2), cbytes )
  call ut%assert_equal( 'Encode 2 bytes', bytes_to_character(cbytes), "QUI=" )
  
! Encode three bytes
  call b64%encode( bytes(1:3), cbytes )
  call ut%assert_equal( 'Encode 3 bytes', bytes_to_character(cbytes), "QUJD" )
  
! Encode four bytes
  call b64%encode( bytes(1:4), cbytes )
  call ut%assert_equal( 'Encode 4 bytes', bytes_to_character(cbytes), "QUJDRA==" )

! Encode five bytes
  call b64%encode( bytes(1:5), cbytes )
  call ut%assert_equal( 'Encode 5 bytes', bytes_to_character(cbytes), "QUJDREU=" )

! Encode long string
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call b64%encode( character_to_bytes(clocal), cbytes )
  clocal = "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2" // &
           "VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBh" // &
           "bGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIH" // &
           "VsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQuIER1" // &
           "aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2ZWxpdCBlc3" // &
           "NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRldXIgc2ludCBv" // &
           "Y2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1aSBvZmZpY2lhIG" // &
           "Rlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg=="
  call ut%assert_equal( 'Encode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_base64_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_base64_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base64) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise
  b64 = base64()
  
! DEcode one byte
  clocal = "QQ=="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 1 byte', bytes_to_character(cbytes), "A" )
  
! Decode two bytes
  clocal = "QUI="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 2 bytes', bytes_to_character(cbytes), "AB" )
  
! Decode three bytes
  clocal = "QUJD"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 3 bytes', bytes_to_character(cbytes), "ABC" )
  
! Decode four bytes
  clocal = "QUJDRA=="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 4 bytes', bytes_to_character(cbytes), "ABCD" )

! Decode five bytes
  clocal = "QUJDREU="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 5 bytes', bytes_to_character(cbytes), "ABCDE" )

! Encode long string
  clocal = "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2" // &
           "VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBh" // &
           "bGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIH" // &
           "VsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQuIER1" // &
           "aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2ZWxpdCBlc3" // &
           "NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRldXIgc2ludCBv" // &
           "Y2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1aSBvZmZpY2lhIG" // &
           "Rlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg=="
  call b64%decode( character_to_bytes(clocal), cbytes )
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call ut%assert_equal( 'Decode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_base64_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_base64_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base64) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise locals
  clocal = "ABCDEFGH"
  bytes = character_to_bytes( clocal )
  
! Initialise
  b64 = base64(.false.)
  
! Encode one byte
  call b64%encode( bytes(1:1), cbytes )
  call ut%assert_equal( 'Encode 1 byte', bytes_to_character(cbytes), "QQ" )
  
! Encode two bytes
  call b64%encode( bytes(1:2), cbytes )
  call ut%assert_equal( 'Encode 2 bytes', bytes_to_character(cbytes), "QUI" )
  
! Encode three bytes
  call b64%encode( bytes(1:3), cbytes )
  call ut%assert_equal( 'Encode 3 bytes', bytes_to_character(cbytes), "QUJD" )
  
! Encode four bytes
  call b64%encode( bytes(1:4), cbytes )
  call ut%assert_equal( 'Encode 4 bytes', bytes_to_character(cbytes), "QUJDRA" )

! Encode five bytes
  call b64%encode( bytes(1:5), cbytes )
  call ut%assert_equal( 'Encode 5 bytes', bytes_to_character(cbytes), "QUJDREU" )

! Encode long string
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call b64%encode( character_to_bytes(clocal), cbytes )
  clocal = "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2" // &
           "VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBh" // &
           "bGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIH" // &
           "VsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQuIER1" // &
           "aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2ZWxpdCBlc3" // &
           "NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRldXIgc2ludCBv" // &
           "Y2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1aSBvZmZpY2lhIG" // &
           "Rlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg"
  call ut%assert_equal( 'Encode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_base64_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_base64_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base64) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise
  b64 = base64()
  
! DEcode one byte
  clocal = "QQ"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 1 byte', bytes_to_character(cbytes), "A" )
  
! Decode two bytes
  clocal = "QUI"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 2 bytes', bytes_to_character(cbytes), "AB" )
  
! Decode three bytes
  clocal = "QUJD"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 3 bytes', bytes_to_character(cbytes), "ABC" )
  
! Decode four bytes
  clocal = "QUJDRA"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 4 bytes', bytes_to_character(cbytes), "ABCD" )

! Decode five bytes
  clocal = "QUJDREU"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 5 bytes', bytes_to_character(cbytes), "ABCDE" )

! Encode long string
  clocal = "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2" // &
           "VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBh" // &
           "bGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIH" // &
           "VsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQuIER1" // &
           "aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2ZWxpdCBlc3" // &
           "NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRldXIgc2ludCBv" // &
           "Y2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1aSBvZmZpY2lhIG" // &
           "Rlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg"
  call b64%decode( character_to_bytes(clocal), cbytes )
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call ut%assert_equal( 'Decode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_base64_test_004

end module unit_m_base64_tests
