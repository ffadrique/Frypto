module unit_m_sha384_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_sha384
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
  use m_string
  use m_file_handler
  
  use m_xfunit
  use m_hash
  use m_sha384

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_sha384_suite_before
  public unit_m_sha384_suite_after

  public unit_m_sha384_test_001
  public unit_m_sha384_before_001
  public unit_m_sha384_after_001

  public unit_m_sha384_test_002
  public unit_m_sha384_before_002
  public unit_m_sha384_after_002

  public unit_m_sha384_test_003
  public unit_m_sha384_before_003
  public unit_m_sha384_after_003

  public unit_m_sha384_test_004
  public unit_m_sha384_before_004
  public unit_m_sha384_after_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_sha384'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

!---End of declaration of local variables---------------------------------------

! Type to host ISTCAVP reference test cases
  type t_nist_cavp
    integer :: length
    integer(kind=1), dimension(:), allocatable :: message
    integer(kind=1), dimension(48) :: hash
  end type t_nist_cavp
  type(t_nist_cavp), dimension(:), allocatable, save :: nist_cavp
  
! File names
  character(len=32), save :: Sha384ShortMsg = "SHA384ShortMsg.rsp"
  character(len=32), save :: Sha384LongMsg = "SHA384LongMsg.rsp"
  character(len=32), save :: Grace04c = "grace04c.xml"

! File handler
  type(t_file_handler), allocatable, save :: fhandler
  
!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha384_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

  
end subroutine unit_m_sha384_before_001

! ############################################################################

subroutine unit_m_sha384_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local varialbes
  character(len=:), allocatable :: text
  integer(kind=1), dimension(:), allocatable :: array
  integer(kind=1), allocatable, dimension(:) :: bytes

! Initialise
  allocate( hash, source=sha384() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )

! FIPS 180-2 sample for one block
  text = 'abc'
  bytes = hash%encode( character_to_bytes(text) )
  call ut%assert_equal( "One block", &
                        bytes_to_hex(bytes,.true.), &
                        'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7' )
  deallocate(bytes)

! FIPS 180-2 sample for one block from charcter string
  bytes = hash%encode( text  )
  call ut%assert_equal( "One block (from character string)", &
                        bytes_to_hex(bytes,.true.), &
                        'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7' )
  deallocate(bytes)

! FIPS 180-2 sample for two blocks
  text = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  bytes = hash%encode( character_to_bytes(text) )
  call ut%assert_equal( "Two blocks", &
                        bytes_to_hex(bytes,.true.), &
                        '09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039' )
  deallocate(bytes)
  
! FIPS 180-2 sample for two blocks from character string
  text = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  bytes = hash%encode( text )
  call ut%assert_equal( "Two blocks (from character string)", &
                        bytes_to_hex(bytes,.true.), &
                        '09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039' )
  deallocate(bytes)

! FIPS 180-2 sample for long message
  allocate( array(1000000) )
  array = ichar('a')
  bytes = hash%encode( array )
  call ut%assert_equal( "Long message", &
                        bytes_to_hex(bytes,.true.), &
                        '9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985' )
  deallocate(bytes)
  
end subroutine unit_m_sha384_test_001

! ############################################################################

subroutine unit_m_sha384_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut



end subroutine unit_m_sha384_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha384_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Load test cases
  call LoadTestCases( manager%get_unit_data_dir() // "/sha/" // Sha384ShortMsg )
  
end subroutine unit_m_sha384_before_002

! ############################################################################

subroutine unit_m_sha384_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local varialbes
  integer :: i
  integer(kind=1), allocatable, dimension(:) :: bytes
  
! Initialise
  allocate( hash, source=sha384() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )

! Loop on the test cases
  do i = 1, size(nist_cavp)
    bytes = hash%encode( nist_cavp(i)%message )
    call ut%assert_equal( "TC " // character(i), &
                          bytes_to_hex(bytes,.true.), &
                          bytes_to_hex(nist_cavp(i)%hash,.true.) )
    deallocate(bytes)
  end do
  
end subroutine unit_m_sha384_test_002

! ############################################################################

subroutine unit_m_sha384_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Release resource
  deallocate( nist_cavp )

end subroutine unit_m_sha384_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha384_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Load test cases
  call LoadTestCases( manager%get_unit_data_dir() // "/sha/" // Sha384LongMsg )
  
end subroutine unit_m_sha384_before_003

! ############################################################################

subroutine unit_m_sha384_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local varialbes
  integer :: i
  integer(kind=1), allocatable, dimension(:) :: bytes
  
! Initialise
  allocate( hash, source=sha384() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )

! Loop on the test cases
  do i = 1, size(nist_cavp)
    bytes = hash%encode( nist_cavp(i)%message )
    call ut%assert_equal( "TC " // character(i), &
                          bytes_to_hex(bytes,.true.), &
                          bytes_to_hex(nist_cavp(i)%hash,.true.) )
    deallocate(bytes)
  end do
  
end subroutine unit_m_sha384_test_003

! ############################################################################

subroutine unit_m_sha384_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Release resource
  deallocate( nist_cavp )

end subroutine unit_m_sha384_after_003


! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha384_before_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Open the input file
  allocate( fhandler )
  fhandler = file_handler( manager%get_unit_data_dir() // '/' // Grace04c )
  call fhandler%open(stream=.true.)
  
end subroutine unit_m_sha384_before_004

! ############################################################################

subroutine unit_m_sha384_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local variabes
  integer :: iostat
  integer :: unit
  integer(kind=1), allocatable, dimension(:) :: bytes

! Initialise
  allocate( hash, source=sha384() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )
  
! Compute file hash
  unit = fhandler%get_unit()
  iostat = 0
  bytes = hash%encode( unit, iostat )
  call ut%assert_equal( "File hash", &
                        bytes_to_hex(bytes,.true.), &
                        'e6820000619165b6fb2c0ac9ea4eb2a51596556889e8b4e8c96b39ace473c9da7ad5757b27b83358a6b388bddba14f13' )
  deallocate(bytes)

end subroutine unit_m_sha384_test_004

! ############################################################################

subroutine unit_m_sha384_after_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Close input file
  call fhandler%close()
  deallocate( fhandler )
  
end subroutine unit_m_sha384_after_004


! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_sha384_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_sha384_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_m_sha384_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite
  

end subroutine unit_m_sha384_suite_after

! ############################################################################
! # Auxiliary procedures  ####################################################
! ############################################################################

! Load test case
subroutine LoadTestCases( filename )  

! Input file name
  type(t_string), intent(in) :: filename

! Local variables
  integer :: count
  integer :: itc
  character(len=3) :: key
  character(len=64000) :: rec
  integer :: ios
  
! Count the number of tests on the file
  count = CountTestCases( filename )

! Allocate test array
  if( allocated(nist_cavp) ) deallocate(nist_cavp)
  allocate( nist_cavp(count) )
  
! Open the test case file
  open(99,file=filename%character(),status='old')
  
! Loop reading the tests
  itc = 0
  ios = 0
  do while( ios == 0 )

!   Read record
    read(99,'(a50000)',iostat=ios) rec
    if( ios == 0 ) then
    
!     Parse the record
      call kvn( rec, key ) 

!     Check the type of line to read
      select case(key)

!       First test line; message length
        case('Len')
          itc = itc + 1
          nist_cavp(itc)%length = integer(rec) / 8

!       Test message
        case('Msg')
          allocate( nist_cavp(itc)%message(len_trim(rec) / 2) )
          if( len_trim(rec) > 0 ) then
            nist_cavp(itc)%message = hex_to_bytes(trim(rec))
          end if 
          
!       Resulting message digest (hash)
        case('MD')
          nist_cavp(itc)%hash = hex_to_bytes(trim(rec))

        end select
 
    end if
    
  end do
  
! Close the file
  close(99)

end subroutine LoadTestCases


! Count the number of test cases in a file
function CountTestCases( filename ) result(res)

! Input file name
  type(t_string), intent(in) :: filename
  
! Count of test cases
  integer :: res

! Local variables
  character(len=80) :: rec
  integer :: ios

! Open the file
  open(99,file=filename%character(),status='old')

! Loop on the records
  res = 0
  ios = 0
  do while( ios == 0 )

!   Read new record
    read(99,*,iostat=ios) rec
    
!   Count on the keyword Len
    if( rec(1:3) == "Len" ) res = res + 1

  end do
  
! Close the file
  close(99)
  
end function CountTestCases


! Gets the key and the contents of a record
! Returns the content in the input record variable
subroutine kvn( rec, key )

! Input/output record
  character(len=*), intent(inout) :: rec
  
! Key in the record
  character(len=*), intent(out) :: key
  
! Local variables
  integer :: idx
  
! Check KVN
  idx = index(rec,'=')
  if( idx > 0 ) then
    
!   Strip the key
    key = trim(adjustl(rec(:idx-1)))
    
!   Strip the contents
    rec = trim(adjustl(rec(idx+1:)))

  else
    
!   No KVN rec
    key = ''
    
  end if
  
end subroutine kvn

end module unit_m_sha384_tests

