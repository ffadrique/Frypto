module unit_m_des_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_des
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
  use m_messages

  use m_xfunit

  use m_des

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_des_suite_before
  public unit_m_des_suite_after

  public unit_m_des_test_001
  public unit_m_des_before_001
  public unit_m_des_after_001

  public unit_m_des_test_002
  public unit_m_des_before_002
  public unit_m_des_after_002

  public unit_m_des_test_003
  public unit_m_des_before_003
  public unit_m_des_after_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_des'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite
  type(t_xfunit_unit), save :: ut

! The error handling structure
  type(t_messages), save :: msg


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_des_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_m_des_before_001

! ############################################################################

subroutine unit_m_des_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(32), parameter :: p = (/ &
       15,  6, 19, 20, 28, 11, 27, 16,  0, 14, 22, 25, &
        4, 17, 30,  9,  1,  7, 23, 13, 31, 26,  2,  8, & 
       18, 12, 29,  5, 21, 10,  3, 24 /) + 1
  integer(kind=1), dimension(32) :: bits = (/ &
        1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, &
        0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0 /)
  integer(kind=1), dimension(32) :: bits_check = (/ &
        1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, &
        0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1 /)

! Invoke des_permutate
  bits = unit_test_private_des_permutate( p, bits )

! Asserts
  call ut%assert_equal( 'Permutate function', bytes_to_hex(bits), bytes_to_hex(bits_check) )

end subroutine unit_m_des_test_001

! ############################################################################

subroutine unit_m_des_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_des_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_des_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_m_des_before_002

! ############################################################################

subroutine unit_m_des_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The DES context
  type(t_des) :: ctx

! Local variables
  integer(kind=1), dimension(8)  :: key
  integer(kind=1), dimension(64)  :: blist
  integer(kind=1), dimension(64)  :: blist_check = (/ &
       0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, &
       0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, &
       0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, &
       1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, &
       0, 1, 1, 1, 0, 0, 1, 0 /)
  integer(kind=1), dimension(64)  :: cipher
  integer(kind=1), dimension(64)  :: cipher_check = (/ &        
       1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, &
       0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, &
       1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, &
       0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, &
       0, 0, 0, 0, 1, 0, 1, 0 /)
  integer(kind=1), dimension(48,16) :: kn

! Invoke des_create_sub_keys
  key = character_to_bytes( 'sGoodKey' )
  ctx = des( key )

! Invoke des_crypt
  blist = blist_check
  call unit_test_private_des_crypt( ctx, blist, 0_1, cipher )

! Assert
  call ut%assert_equal( 'Basic block encrypt', bytes_to_hex(cipher), bytes_to_hex(cipher_check) )

! Invoke des_crypt
  call unit_test_private_des_crypt( ctx, cipher, 1_1, blist )

! Assert
  call ut%assert_equal( 'Basic block encrypt', bytes_to_hex(blist), bytes_to_hex(blist_check) )

end subroutine unit_m_des_test_002

! ############################################################################

subroutine unit_m_des_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_des_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_des_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_m_des_before_003

! ############################################################################

subroutine unit_m_des_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The DES context
  type(t_des) :: ctx

! Local variables
  integer(kind=1), dimension(8)  :: key
  integer(kind=1), dimension(8)  :: text
  integer(kind=1), dimension(8)  :: cipher
  integer(kind=1), dimension(8)  :: local


! Initialise
  key  = character_to_bytes( 'sGoodKey' )
  text = character_to_bytes( 'codetext' )
  ctx = des( key )

! Encrypt
  call ctx%encrypt( text, cipher )

! Asserts
  call ut%assert_equal( 'DES Block encrypt', bytes_to_hex(cipher), 'BAF351E26BBF2716', ignorecase=.true. )

! Decrypt
  call ctx%decrypt( cipher, text )

! Asserts
  call ut%assert_equal( 'DES Block decrypt', bytes_to_hex(text), '636F646574657874', ignorecase=.true. )

end subroutine unit_m_des_test_003

! ############################################################################

subroutine unit_m_des_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_des_after_003

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_des_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_des_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_m_des_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_des_suite_after

end module unit_m_des_tests

