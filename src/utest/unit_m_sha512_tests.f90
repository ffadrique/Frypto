module unit_m_sha512_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_sha512
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
  use m_sha512

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_sha512_suite_before
  public unit_m_sha512_suite_after

  public unit_m_sha512_test_001
  public unit_m_sha512_before_001
  public unit_m_sha512_after_001

  public unit_m_sha512_test_002
  public unit_m_sha512_before_002
  public unit_m_sha512_after_002

  public unit_m_sha512_test_003
  public unit_m_sha512_before_003
  public unit_m_sha512_after_003

  public unit_m_sha512_test_004
  public unit_m_sha512_before_004
  public unit_m_sha512_after_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_sha512'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

!---End of declaration of local variables---------------------------------------

! Type to host ISTCAVP reference test cases
  type t_nist_cavp
    integer :: length
    integer(kind=1), dimension(:), allocatable :: message
    integer(kind=1), dimension(64) :: hash
  end type t_nist_cavp
  type(t_nist_cavp), dimension(:), allocatable, save :: nist_cavp
  
! File names
  character(len=32), save :: Sha512ShortMsg = "SHA512ShortMsg.rsp"
  character(len=32), save :: Sha512LongMsg = "SHA512LongMsg.rsp"
  character(len=32), save :: Grace04c = "grace04c.xml"

! File handler
  type(t_file_handler), allocatable, save :: fhandler
  
!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha512_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

  
end subroutine unit_m_sha512_before_001

! ############################################################################

subroutine unit_m_sha512_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local varialbes
  character(len=:), allocatable :: text
  integer(kind=1), dimension(:), allocatable :: array
  integer(kind=1), allocatable, dimension(:) :: bytes
  
! Initialise
  allocate( hash, source=sha512() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )

! FIPS 180-2 sample for one block
  text = 'abc'
  bytes = hash%encode( character_to_bytes(text) )
  call ut%assert_equal( "One block", &
                        bytes_to_hex(bytes,.true.), &
                        'ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f' )

! FIPS 180-2 sample for one block from charcter string
  bytes = hash%encode( text )
  call ut%assert_equal( "One block (from character string)", &
                        bytes_to_hex(bytes,.true.), &
                        'ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f' )
  deallocate(bytes)

! FIPS 180-2 sample for two blocks
  text = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  bytes = hash%encode( character_to_bytes(text) )
  call ut%assert_equal( "Two blocks", &
                        bytes_to_hex(bytes,.true.), &
                        '8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909' )
  deallocate(bytes)
  
! FIPS 180-2 sample for two blocks from character string
  text = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  bytes = hash%encode( text )
  call ut%assert_equal( "Two blocks (from character string)", &
                        bytes_to_hex(bytes,.true.), &
                        '8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909' )
  deallocate(bytes)

! FIPS 180-2 sample for long message
  allocate( array(1000000) )
  array = ichar('a',1)
  bytes = hash%encode( array )
  call ut%assert_equal( "Long message", &
                        bytes_to_hex(bytes,.true.), &
                        'e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b' )
  deallocate(bytes)
  
end subroutine unit_m_sha512_test_001

! ############################################################################

subroutine unit_m_sha512_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut



end subroutine unit_m_sha512_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha512_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Load test cases
  call LoadTestCases( manager%get_unit_data_dir() // "/sha/" // Sha512ShortMsg )
  
end subroutine unit_m_sha512_before_002

! ############################################################################

subroutine unit_m_sha512_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local varialbes
  integer :: i
  integer(kind=1), allocatable, dimension(:) :: bytes
  
! Initialise
  allocate( hash, source=sha512() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )

! Loop on the test cases
  do i = 1, size(nist_cavp)
    bytes = hash%encode( nist_cavp(i)%message )
    call ut%assert_equal( "TC " // character(i), &
                          bytes_to_hex(bytes,.true.), &
                          bytes_to_hex(nist_cavp(i)%hash,.true.) )
    deallocate(bytes)
  end do
  
end subroutine unit_m_sha512_test_002

! ############################################################################

subroutine unit_m_sha512_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Release resource
  deallocate( nist_cavp )

end subroutine unit_m_sha512_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha512_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Load test cases
  call LoadTestCases( manager%get_unit_data_dir() // "/sha/" // Sha512LongMsg )
  
end subroutine unit_m_sha512_before_003

! ############################################################################

subroutine unit_m_sha512_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local varialbes
  integer :: i
  integer(kind=1), allocatable, dimension(:) :: bytes
  
! Initialise
  allocate( hash, source=sha512() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )

! Loop on the test cases
  do i = 1, size(nist_cavp)
    bytes = hash%encode( nist_cavp(i)%message )
    call ut%assert_equal( "TC " // character(i), &
                          bytes_to_hex(bytes,.true.), &
                          bytes_to_hex(nist_cavp(i)%hash,.true.) )
    deallocate(bytes)
  end do
  
end subroutine unit_m_sha512_test_003

! ############################################################################

subroutine unit_m_sha512_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Release resource
  deallocate( nist_cavp )

end subroutine unit_m_sha512_after_003


! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_sha512_before_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Open the input file
  allocate( fhandler )
  fhandler = file_handler( manager%get_unit_data_dir() // '/' // Grace04c )
  call fhandler%open(stream=.true.)
  
end subroutine unit_m_sha512_before_004

! ############################################################################

subroutine unit_m_sha512_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  class(t_hash), allocatable :: hash
  
! Local variabes
  integer :: iostat
  integer :: unit
  integer(kind=1), allocatable, dimension(:) :: bytes

! Initialise
  allocate( hash, source=sha512() )
  allocate( bytes(hash%get_hash_size()), source=0_1 )
  
! Compute file hash
  unit = fhandler%get_unit()
  iostat = 0
  bytes = hash%encode( unit, iostat )
  call ut%assert_equal( "File hash", &
                        bytes_to_hex(bytes,.true.), &
                        '5238ad6afe2ce6f74f36dc691e2adfa9c34d2856839b3a127163a4349e06bc92ae7d7d84d6ead47e6047b9fa6f4b64dcec5446df223e79579b72392930884be1' )
  deallocate(bytes)

end subroutine unit_m_sha512_test_004

! ############################################################################

subroutine unit_m_sha512_after_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Close input file
  call fhandler%close()
  deallocate( fhandler )
  
end subroutine unit_m_sha512_after_004


! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_sha512_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_sha512_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_m_sha512_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite
  

end subroutine unit_m_sha512_suite_after

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

end module unit_m_sha512_tests

