module unit_m_uuencode_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_uuencode
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

  use m_uuencode

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_uuencode_test_001
  public unit_m_uuencode_test_002
  public unit_m_uuencode_test_003
  public unit_m_uuencode_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_uuencode'
  
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

subroutine unit_m_uuencode_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuencode) :: b64

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
  b64 = uuencode()
  
! Encode one byte
  call b64%encode( bytes(1:1), cbytes )
  call ut%assert_equal( 'Encode 1 byte', bytes_to_character(cbytes), "00``" )
  
! Encode two bytes
  call b64%encode( bytes(1:2), cbytes )
  call ut%assert_equal( 'Encode 2 bytes', bytes_to_character(cbytes), "04(`" )
  
! Encode three bytes
  call b64%encode( bytes(1:3), cbytes )
  call ut%assert_equal( 'Encode 3 bytes', bytes_to_character(cbytes), "04)#" )
  
! Encode four bytes
  call b64%encode( bytes(1:4), cbytes )
  call ut%assert_equal( 'Encode 4 bytes', bytes_to_character(cbytes), "04)#1```" )

! Encode five bytes
  call b64%encode( bytes(1:5), cbytes )
  call ut%assert_equal( 'Encode 5 bytes', bytes_to_character(cbytes), "04)#1$4`" )

! Encode long string
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call b64%encode( character_to_bytes(clocal), cbytes )
  clocal = "3&]R96T@:7!S=6T@9&]L;W(@<VET(&%M970L(&-O;G-E8W1E='5R(&%D:7!I" // &
           "<V-I;F<@96QI=""P@<V5D(&1O(&5I=7-M;V0@=&5M<&]R(&EN8VED:61U;G0@" // &
           "=70@;&%B;W)E(&5T(&1O;&]R92!M86=N82!A;&EQ=6$N(%5T(&5N:6T@860@" // &
           ";6EN:6T@=F5N:6%M+""!Q=6ES(&YO<W1R=60@97AE<F-I=&%T:6]N('5L;&%M" // &
           "8V\@;&%B;W)I<R!N:7-I('5T(&%L:7%U:7`@97@@96$@8V]M;6]D;R!C;VYS" // &
           "97%U870N($1U:7,@875T92!I<G5R92!D;VQO<B!I;B!R97!R96AE;F1E<FET" // &
           "(&EN('9O;'5P=&%T92!V96QI=""!E<W-E(&-I;&QU;2!D;VQO<F4@974@9G5G" // &
           ":6%T(&YU;&QA('!A<FEA='5R+B!%>&-E<'1E=7(@<VEN=""!O8V-A96-A=""!C" // &
           "=7!I9&%T870@;F]N('!R;VED96YT+""!S=6YT(&EN(&-U;'!A('%U:2!O9F9I" // &
           "8VEA(&1E<V5R=6YT(&UO;&QI=""!A;FEM(&ED(&5S=""!L86)O<G5M+@``"
  call ut%assert_equal( 'Encode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_uuencode_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_uuencode_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuencode) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise
  b64 = uuencode()
  
! Decode one byte
  clocal = "00``"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 1 byte', bytes_to_character(cbytes), "A" )
  
! Decode two bytes
  clocal = "04(`"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 2 bytes', bytes_to_character(cbytes), "AB" )
  
! Decode three bytes
  clocal = "04)#"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 3 bytes', bytes_to_character(cbytes), "ABC" )
  
! Decode four bytes
  clocal = "04)#1```"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 4 bytes', bytes_to_character(cbytes), "ABCD" )

! Decode five bytes
  clocal = "04)#1$4`"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 5 bytes', bytes_to_character(cbytes), "ABCDE" )

! Encode long string
  clocal = "3&]R96T@:7!S=6T@9&]L;W(@<VET(&%M970L(&-O;G-E8W1E='5R(&%D:7!I" // &
           "<V-I;F<@96QI=""P@<V5D(&1O(&5I=7-M;V0@=&5M<&]R(&EN8VED:61U;G0@" // &
           "=70@;&%B;W)E(&5T(&1O;&]R92!M86=N82!A;&EQ=6$N(%5T(&5N:6T@860@" // &
           ";6EN:6T@=F5N:6%M+""!Q=6ES(&YO<W1R=60@97AE<F-I=&%T:6]N('5L;&%M" // &
           "8V\@;&%B;W)I<R!N:7-I('5T(&%L:7%U:7`@97@@96$@8V]M;6]D;R!C;VYS" // &
           "97%U870N($1U:7,@875T92!I<G5R92!D;VQO<B!I;B!R97!R96AE;F1E<FET" // &
           "(&EN('9O;'5P=&%T92!V96QI=""!E<W-E(&-I;&QU;2!D;VQO<F4@974@9G5G" // &
           ":6%T(&YU;&QA('!A<FEA='5R+B!%>&-E<'1E=7(@<VEN=""!O8V-A96-A=""!C" // &
           "=7!I9&%T870@;F]N('!R;VED96YT+""!S=6YT(&EN(&-U;'!A('%U:2!O9F9I" // &
           "8VEA(&1E<V5R=6YT(&UO;&QI=""!A;FEM(&ED(&5S=""!L86)O<G5M+@``"
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

end subroutine unit_m_uuencode_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_uuencode_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuencode) :: b64

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
  b64 = uuencode(.false.)
  
! Encode one byte
  call b64%encode( bytes(1:1), cbytes )
  call ut%assert_equal( 'Encode 1 byte', bytes_to_character(cbytes), "00" )
  
! Encode two bytes
  call b64%encode( bytes(1:2), cbytes )
  call ut%assert_equal( 'Encode 2 bytes', bytes_to_character(cbytes), "04(" )
  
! Encode three bytes
  call b64%encode( bytes(1:3), cbytes )
  call ut%assert_equal( 'Encode 3 bytes', bytes_to_character(cbytes), "04)#" )
  
! Encode four bytes
  call b64%encode( bytes(1:4), cbytes )
  call ut%assert_equal( 'Encode 4 bytes', bytes_to_character(cbytes), "04)#1`" )

! Encode five bytes
  call b64%encode( bytes(1:5), cbytes )
  call ut%assert_equal( 'Encode 5 bytes', bytes_to_character(cbytes), "04)#1$4" )

! Encode long string
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call b64%encode( character_to_bytes(clocal), cbytes )
  clocal = "3&]R96T@:7!S=6T@9&]L;W(@<VET(&%M970L(&-O;G-E8W1E='5R(&%D:7!I" // &
           "<V-I;F<@96QI=""P@<V5D(&1O(&5I=7-M;V0@=&5M<&]R(&EN8VED:61U;G0@" // &
           "=70@;&%B;W)E(&5T(&1O;&]R92!M86=N82!A;&EQ=6$N(%5T(&5N:6T@860@" // &
           ";6EN:6T@=F5N:6%M+""!Q=6ES(&YO<W1R=60@97AE<F-I=&%T:6]N('5L;&%M" // &
           "8V\@;&%B;W)I<R!N:7-I('5T(&%L:7%U:7`@97@@96$@8V]M;6]D;R!C;VYS" // &
           "97%U870N($1U:7,@875T92!I<G5R92!D;VQO<B!I;B!R97!R96AE;F1E<FET" // &
           "(&EN('9O;'5P=&%T92!V96QI=""!E<W-E(&-I;&QU;2!D;VQO<F4@974@9G5G" // &
           ":6%T(&YU;&QA('!A<FEA='5R+B!%>&-E<'1E=7(@<VEN=""!O8V-A96-A=""!C" // &
           "=7!I9&%T870@;F]N('!R;VED96YT+""!S=6YT(&EN(&-U;'!A('%U:2!O9F9I" // &
           "8VEA(&1E<V5R=6YT(&UO;&QI=""!A;FEM(&ED(&5S=""!L86)O<G5M+@"
  call ut%assert_equal( 'Encode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_uuencode_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_uuencode_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_uuencode) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise
  b64 = uuencode()
  
! Decode one byte
  clocal = "00"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 1 byte', bytes_to_character(cbytes), "A" )
  
! Decode two bytes
  clocal = "04("
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 2 bytes', bytes_to_character(cbytes), "AB" )
  
! Decode three bytes
  clocal = "04)#"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 3 bytes', bytes_to_character(cbytes), "ABC" )
  
! Decode four bytes
  clocal = "04)#1`"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 4 bytes', bytes_to_character(cbytes), "ABCD" )

! Decode five bytes
  clocal = "04)#1$4"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 5 bytes', bytes_to_character(cbytes), "ABCDE" )

! Encode long string
  clocal = "3&]R96T@:7!S=6T@9&]L;W(@<VET(&%M970L(&-O;G-E8W1E='5R(&%D:7!I" // &
           "<V-I;F<@96QI=""P@<V5D(&1O(&5I=7-M;V0@=&5M<&]R(&EN8VED:61U;G0@" // &
           "=70@;&%B;W)E(&5T(&1O;&]R92!M86=N82!A;&EQ=6$N(%5T(&5N:6T@860@" // &
           ";6EN:6T@=F5N:6%M+""!Q=6ES(&YO<W1R=60@97AE<F-I=&%T:6]N('5L;&%M" // &
           "8V\@;&%B;W)I<R!N:7-I('5T(&%L:7%U:7`@97@@96$@8V]M;6]D;R!C;VYS" // &
           "97%U870N($1U:7,@875T92!I<G5R92!D;VQO<B!I;B!R97!R96AE;F1E<FET" // &
           "(&EN('9O;'5P=&%T92!V96QI=""!E<W-E(&-I;&QU;2!D;VQO<F4@974@9G5G" // &
           ":6%T(&YU;&QA('!A<FEA='5R+B!%>&-E<'1E=7(@<VEN=""!O8V-A96-A=""!C" // &
           "=7!I9&%T870@;F]N('!R;VED96YT+""!S=6YT(&EN(&-U;'!A('%U:2!O9F9I" // &
           "8VEA(&1E<V5R=6YT(&UO;&QI=""!A;FEM(&ED(&5S=""!L86)O<G5M+@"
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

end subroutine unit_m_uuencode_test_004

end module unit_m_uuencode_tests
