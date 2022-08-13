module unit_m_base32_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_base32
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

  use m_base32

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_base32_test_001
  public unit_m_base32_test_002
  public unit_m_base32_test_003
  public unit_m_base32_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_base32'
  
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

subroutine unit_m_base32_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base32) :: b64

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
  b64 = base32()
  
! Encode one byte
  call b64%encode( bytes(1:1), cbytes )
  call ut%assert_equal( 'Encode 1 byte', bytes_to_character(cbytes), "IE======" )
  
! Encode two bytes
  call b64%encode( bytes(1:2), cbytes )
  call ut%assert_equal( 'Encode 2 bytes', bytes_to_character(cbytes), "IFBA====" )
  
! Encode three bytes
  call b64%encode( bytes(1:3), cbytes )
  call ut%assert_equal( 'Encode 3 bytes', bytes_to_character(cbytes), "IFBEG===" )
  
! Encode four bytes
  call b64%encode( bytes(1:4), cbytes )
  call ut%assert_equal( 'Encode 4 bytes', bytes_to_character(cbytes), "IFBEGRA=" )

! Encode five bytes
  call b64%encode( bytes(1:5), cbytes )
  call ut%assert_equal( 'Encode 5 bytes', bytes_to_character(cbytes), "IFBEGRCF" )

! Encode six bytes
  call b64%encode( bytes(1:6), cbytes )
  call ut%assert_equal( 'Encode 6 bytes', bytes_to_character(cbytes), "IFBEGRCFIY======" )

! Encode seven bytes
  call b64%encode( bytes(1:7), cbytes )
  call ut%assert_equal( 'Encode 7 bytes', bytes_to_character(cbytes), "IFBEGRCFIZDQ====" )

! Encode long string
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call b64%encode( character_to_bytes(clocal), cbytes )
  clocal = "JRXXEZLNEBUXA43VNUQGI33MN5ZCA43JOQQGC3LFOQWCAY3PNZZWKY3UMV2HK4RAMFSGS4DJONRWS" // &
           "3THEBSWY2LUFQQHGZLEEBSG6IDFNF2XG3LPMQQHIZLNOBXXEIDJNZRWSZDJMR2W45BAOV2CA3DBMJ" // &
           "XXEZJAMV2CAZDPNRXXEZJANVQWO3TBEBQWY2LROVQS4ICVOQQGK3TJNUQGCZBANVUW42LNEB3GK3T" // &
           "JMFWSYIDROVUXGIDON5ZXI4TVMQQGK6DFOJRWS5DBORUW63RAOVWGYYLNMNXSA3DBMJXXE2LTEBXG" // &
           "S43JEB2XIIDBNRUXC5LJOAQGK6BAMVQSAY3PNVWW6ZDPEBRW63TTMVYXKYLUFYQEI5LJOMQGC5LUM" // &
           "UQGS4TVOJSSAZDPNRXXEIDJNYQHEZLQOJSWQZLOMRSXE2LUEBUW4IDWN5WHK4DUMF2GKIDWMVWGS5" // &
           "BAMVZXGZJAMNUWY3DVNUQGI33MN5ZGKIDFOUQGM5LHNFQXIIDOOVWGYYJAOBQXE2LBOR2XELRAIV4" // &
           "GGZLQORSXK4RAONUW45BAN5RWGYLFMNQXIIDDOVYGSZDBORQXIIDON5XCA4DSN5UWIZLOOQWCA43V" // &
           "NZ2CA2LOEBRXK3DQMEQHC5LJEBXWMZTJMNUWCIDEMVZWK4TVNZ2CA3LPNRWGS5BAMFXGS3JANFSCA" // &
           "ZLTOQQGYYLCN5ZHK3JO"
  call ut%assert_equal( 'Encode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_base32_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_base32_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base32) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(:), allocatable :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise
  b64 = base32()
  
! Decode one byte
  clocal = "IE======"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 1 byte', bytes_to_character(cbytes), "A" )
  
! Decode two bytes
  clocal = "IFBA===="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 2 bytes', bytes_to_character(cbytes), "AB" )
  
! Decode three bytes
  clocal = "IFBEG==="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 3 bytes', bytes_to_character(cbytes), "ABC" )
  
! Decode four bytes
  clocal = "IFBEGRA="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 4 bytes', bytes_to_character(cbytes), "ABCD" )

! Decode five bytes
  clocal = "IFBEGRCF"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 5 bytes', bytes_to_character(cbytes), "ABCDE" )

! Decode six bytes
  clocal = "IFBEGRCFIY======"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 6 bytes', bytes_to_character(cbytes), "ABCDEF" )

! Decode seven bytes
  clocal = "IFBEGRCFIZDQ===="
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 7 bytes', bytes_to_character(cbytes), "ABCDEFG" )

! Encode long string
  clocal = "JRXXEZLNEBUXA43VNUQGI33MN5ZCA43JOQQGC3LFOQWCAY3PNZZWKY3UMV2HK4RAMFSGS4DJONRWS" // &
           "3THEBSWY2LUFQQHGZLEEBSG6IDFNF2XG3LPMQQHIZLNOBXXEIDJNZRWSZDJMR2W45BAOV2CA3DBMJ" // &
           "XXEZJAMV2CAZDPNRXXEZJANVQWO3TBEBQWY2LROVQS4ICVOQQGK3TJNUQGCZBANVUW42LNEB3GK3T" // &
           "JMFWSYIDROVUXGIDON5ZXI4TVMQQGK6DFOJRWS5DBORUW63RAOVWGYYLNMNXSA3DBMJXXE2LTEBXG" // &
           "S43JEB2XIIDBNRUXC5LJOAQGK6BAMVQSAY3PNVWW6ZDPEBRW63TTMVYXKYLUFYQEI5LJOMQGC5LUM" // &
           "UQGS4TVOJSSAZDPNRXXEIDJNYQHEZLQOJSWQZLOMRSXE2LUEBUW4IDWN5WHK4DUMF2GKIDWMVWGS5" // &
           "BAMVZXGZJAMNUWY3DVNUQGI33MN5ZGKIDFOUQGM5LHNFQXIIDOOVWGYYJAOBQXE2LBOR2XELRAIV4" // &
           "GGZLQORSXK4RAONUW45BAN5RWGYLFMNQXIIDDOVYGSZDBORQXIIDON5XCA4DSN5UWIZLOOQWCA43V" // &
           "NZ2CA2LOEBRXK3DQMEQHC5LJEBXWMZTJMNUWCIDEMVZWK4TVNZ2CA3LPNRWGS5BAMFXGS3JANFSCA" // &
           "ZLTOQQGYYLCN5ZHK3JO"
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

end subroutine unit_m_base32_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_base32_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base32) :: b64

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
  b64 = base32(.false.)
  
! Encode one byte
  call b64%encode( bytes(1:1), cbytes )
  call ut%assert_equal( 'Encode 1 byte', bytes_to_character(cbytes), "IE" )
  
! Encode two bytes
  call b64%encode( bytes(1:2), cbytes )
  call ut%assert_equal( 'Encode 2 bytes', bytes_to_character(cbytes), "IFBA" )
  
! Encode three bytes
  call b64%encode( bytes(1:3), cbytes )
  call ut%assert_equal( 'Encode 3 bytes', bytes_to_character(cbytes), "IFBEG" )
  
! Encode four bytes
  call b64%encode( bytes(1:4), cbytes )
  call ut%assert_equal( 'Encode 4 bytes', bytes_to_character(cbytes), "IFBEGRA" )

! Encode five bytes
  call b64%encode( bytes(1:5), cbytes )
  call ut%assert_equal( 'Encode 5 bytes', bytes_to_character(cbytes), "IFBEGRCF" )

! Encode six bytes
  call b64%encode( bytes(1:6), cbytes )
  call ut%assert_equal( 'Encode 6 bytes', bytes_to_character(cbytes), "IFBEGRCFIY" )

! Encode seven bytes
  call b64%encode( bytes(1:7), cbytes )
  call ut%assert_equal( 'Encode 7 bytes', bytes_to_character(cbytes), "IFBEGRCFIZDQ" )

! Encode long string
  clocal = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor " // &
           "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis " // &
           "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. " // &
           "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu " // &
           "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in " // &
           "culpa qui officia deserunt mollit anim id est laborum."
  call b64%encode( character_to_bytes(clocal), cbytes )
  clocal = "JRXXEZLNEBUXA43VNUQGI33MN5ZCA43JOQQGC3LFOQWCAY3PNZZWKY3UMV2HK4RAMFSGS4DJONRWS" // &
           "3THEBSWY2LUFQQHGZLEEBSG6IDFNF2XG3LPMQQHIZLNOBXXEIDJNZRWSZDJMR2W45BAOV2CA3DBMJ" // &
           "XXEZJAMV2CAZDPNRXXEZJANVQWO3TBEBQWY2LROVQS4ICVOQQGK3TJNUQGCZBANVUW42LNEB3GK3T" // &
           "JMFWSYIDROVUXGIDON5ZXI4TVMQQGK6DFOJRWS5DBORUW63RAOVWGYYLNMNXSA3DBMJXXE2LTEBXG" // &
           "S43JEB2XIIDBNRUXC5LJOAQGK6BAMVQSAY3PNVWW6ZDPEBRW63TTMVYXKYLUFYQEI5LJOMQGC5LUM" // &
           "UQGS4TVOJSSAZDPNRXXEIDJNYQHEZLQOJSWQZLOMRSXE2LUEBUW4IDWN5WHK4DUMF2GKIDWMVWGS5" // &
           "BAMVZXGZJAMNUWY3DVNUQGI33MN5ZGKIDFOUQGM5LHNFQXIIDOOVWGYYJAOBQXE2LBOR2XELRAIV4" // &
           "GGZLQORSXK4RAONUW45BAN5RWGYLFMNQXIIDDOVYGSZDBORQXIIDON5XCA4DSN5UWIZLOOQWCA43V" // &
           "NZ2CA2LOEBRXK3DQMEQHC5LJEBXWMZTJMNUWCIDEMVZWK4TVNZ2CA3LPNRWGS5BAMFXGS3JANFSCA" // &
           "ZLTOQQGYYLCN5ZHK3JO"
  call ut%assert_equal( 'Encode long bytes', bytes_to_character(cbytes), clocal )
 
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_base32_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_base32_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_base32) :: b64

! Local variables
  character(len=:), allocatable :: clocal
  integer(kind=1), dimension(8) :: bytes
  integer(kind=1), dimension(:), allocatable :: cbytes
  
! Reset error handling structures
  call msg%reset_error()

! Initialise
  b64 = base32()
  
! Decode one byte
  clocal = "IE"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 1 byte', bytes_to_character(cbytes), "A" )
  
! Decode two bytes
  clocal = "IFBA"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 2 bytes', bytes_to_character(cbytes), "AB" )
  
! Decode three bytes
  clocal = "IFBEG"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 3 bytes', bytes_to_character(cbytes), "ABC" )
  
! Decode four bytes
  clocal = "IFBEGRA"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 4 bytes', bytes_to_character(cbytes), "ABCD" )

! Decode five bytes
  clocal = "IFBEGRCF"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 5 bytes', bytes_to_character(cbytes), "ABCDE" )

! Decode six bytes
  clocal = "IFBEGRCFIY"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 6 bytes', bytes_to_character(cbytes), "ABCDEF" )

! Decode seven bytes
  clocal = "IFBEGRCFIZDQ"
  call b64%decode( character_to_bytes(clocal), cbytes )
  call ut%assert_equal( 'Decode 7 bytes', bytes_to_character(cbytes), "ABCDEFG" )

! Encode long string
  clocal = "JRXXEZLNEBUXA43VNUQGI33MN5ZCA43JOQQGC3LFOQWCAY3PNZZWKY3UMV2HK4RAMFSGS4DJONRWS" // &
           "3THEBSWY2LUFQQHGZLEEBSG6IDFNF2XG3LPMQQHIZLNOBXXEIDJNZRWSZDJMR2W45BAOV2CA3DBMJ" // &
           "XXEZJAMV2CAZDPNRXXEZJANVQWO3TBEBQWY2LROVQS4ICVOQQGK3TJNUQGCZBANVUW42LNEB3GK3T" // &
           "JMFWSYIDROVUXGIDON5ZXI4TVMQQGK6DFOJRWS5DBORUW63RAOVWGYYLNMNXSA3DBMJXXE2LTEBXG" // &
           "S43JEB2XIIDBNRUXC5LJOAQGK6BAMVQSAY3PNVWW6ZDPEBRW63TTMVYXKYLUFYQEI5LJOMQGC5LUM" // &
           "UQGS4TVOJSSAZDPNRXXEIDJNYQHEZLQOJSWQZLOMRSXE2LUEBUW4IDWN5WHK4DUMF2GKIDWMVWGS5" // &
           "BAMVZXGZJAMNUWY3DVNUQGI33MN5ZGKIDFOUQGM5LHNFQXIIDOOVWGYYJAOBQXE2LBOR2XELRAIV4" // &
           "GGZLQORSXK4RAONUW45BAN5RWGYLFMNQXIIDDOVYGSZDBORQXIIDON5XCA4DSN5UWIZLOOQWCA43V" // &
           "NZ2CA2LOEBRXK3DQMEQHC5LJEBXWMZTJMNUWCIDEMVZWK4TVNZ2CA3LPNRWGS5BAMFXGS3JANFSCA" // &
           "ZLTOQQGYYLCN5ZHK3JO"
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

end subroutine unit_m_base32_test_004

end module unit_m_base32_tests
