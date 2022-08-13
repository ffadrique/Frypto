module unit_m_blowfish_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_blowfish
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
  
  use m_blowfish
  use unit_m_blowfish_c

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_blowfish_suite_before
  public unit_m_blowfish_suite_after

  public unit_m_blowfish_test_001
  public unit_m_blowfish_before_001
  public unit_m_blowfish_after_001

  public unit_m_blowfish_test_002
  public unit_m_blowfish_before_002
  public unit_m_blowfish_after_002

  public unit_m_blowfish_test_003
  public unit_m_blowfish_before_003
  public unit_m_blowfish_after_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_blowfish'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), save :: manager
  type(t_xfunit_suite), allocatable, save :: suite
  type(t_xfunit_unit), save :: ut

! The error handling structure
  type(t_messages), save :: msg

! Encryption variables for C
  integer(kind=1), bind(c), dimension(8) :: c_text
  character, bind(c), dimension(17) :: c_key
  integer(kind=4), bind(c) :: c_left, c_right
  type(c_blowfish), save :: c_ctx

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_blowfish_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_m_blowfish_before_001

! ############################################################################

subroutine unit_m_blowfish_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Encryption variables
  type(t_blowfish) :: ctx
  integer(kind=1), dimension(16) :: key
  integer(kind=4) :: left, right

! Initialise
  left  = 1
  c_left = left
  right = 0
  c_right = 0
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee35" )
  c_key = transfer( key, c_key )
  c_key(17:17) = char(0)

! Initialisation
  ctx = blowfish( key )
  call blowfish_init( c_ctx, c_key, size(key) );

! Asserts
  call ut%assert_equal( 'P', hex(unit_test_private_blowfish_P(ctx)), hex(int(c_ctx%P)) )

! Invoke F
  right = unit_test_private_blowfish_f( ctx, left )

! Invoke F in C
  c_right = f( c_ctx, c_left )

! Asserts
  call ut%assert_equal( 'F output (' // trim(character(left)) // ')', right, c_right )

! Re-initialise
  left  = -1232132
  c_left = left
  right = 0
  c_right = 0
  
! Invoke F
  right = unit_test_private_blowfish_f( ctx, left )

! Invoke F in C
  c_right = f( c_ctx, c_left )

! Asserts
  call ut%assert_equal( 'F output (' // trim(character(left)) // ')', right, c_right )

end subroutine unit_m_blowfish_test_001

! ############################################################################

subroutine unit_m_blowfish_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_blowfish_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_blowfish_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_blowfish_before_002

! ############################################################################

subroutine unit_m_blowfish_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Encryption variables
  type(t_blowfish) :: ctx
  integer(kind=1), dimension(16) :: key
  integer(kind=4) :: left, right

! Initialise
  left  = 1
  c_left  = left
  right = 2
  c_right = right
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee35" )
  c_key = transfer( key, c_key )
  c_key(17:17) = char(0)

! Invoke encrypt_lr
  ctx = blowfish( key )
  call unit_test_private_blowfish_encrypt_lr( ctx, left, right )

! Invoke encrypt in C
  call blowfish_init( c_ctx, c_key, size(key) );
  call blowfish_encrypt( c_ctx, c_left, c_right )

! Asserts
  call ut%assert_equal( 'Left/right encrypt (' // &
                        trim(character(left)) // ',' // &
                        trim(character(right)) // ') left', left, c_left )
  call ut%assert_equal( 'Left/right encrypt (' // &
                        trim(character(left)) // ',' // &
                        trim(character(right)) // ') right', right, c_right )

! Invoke decrypt_lr
  call unit_test_private_blowfish_decrypt_lr( ctx, left, right )

! Invoke decrypt in C
  call blowfish_decrypt( c_ctx, c_left, c_right )

! Asserts
  call ut%assert_equal( 'Left/right decrypt (' // &
                        trim(character(left)) // ',' // &
                        trim(character(right)) // ') left', left, c_left )
  call ut%assert_equal( 'Left/right decrypt (' // &
                        trim(character(left)) // ',' // &
                        trim(character(right)) // ') right', right, c_right )

end subroutine unit_m_blowfish_test_002

! ############################################################################

subroutine unit_m_blowfish_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_blowfish_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_blowfish_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_blowfish_before_003

! ############################################################################

subroutine unit_m_blowfish_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Encryption variables
  type(t_blowfish) :: ctx
  integer(kind=1), dimension(16) :: key
  integer(kind=1), dimension(8) :: text, ctext
  integer(kind=1), dimension(8) :: cipher, ccipher

! Initialise
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee35" )
  text = hex_to_bytes( "0000000000000000" )
 
! Encryption
  ctx = blowfish( key )
  call ctx%encrypt( text, cipher )

! Encryption for C reference
  c_key = transfer( key, c_key )
  c_key(17:17) = char(0)
  call blowfish_init( c_ctx, c_key, size(key) )
  c_left  = transfer( text(4:1:-1), c_left )
  c_right = transfer( text(8:5:-1), c_right )
  call blowfish_encrypt( c_ctx, c_left, c_right )
  ccipher(4:1:-1) = transfer( c_left, text(1:4) )
  ccipher(8:5:-1) = transfer( c_right, text(5:8) )

! Asserts
  call ut%assert_equal( 'Encrypted text', bytes_to_hex(cipher), bytes_to_hex(ccipher) )

! Initialise
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee35" )
  cipher = hex_to_bytes( "46f2fb342d6f0ab4" )
  
! Decryption
  call ctx%decrypt( cipher, text )
  
! Decryption for C reference
  c_key = transfer( key, c_key )
  c_key(17:17) = char(0)
  call blowfish_init( c_ctx, c_key, size(key) )
  c_left  = transfer( cipher(4:1:-1), c_left )
  c_right = transfer( cipher(8:5:-1), c_right )
  call blowfish_decrypt( c_ctx, c_left, c_right )
  ctext(4:1:-1) = transfer( c_left, text(1:4) )
  ctext(8:5:-1) = transfer( c_right, text(5:8) )

! Asserts
  call ut%assert_equal( 'Decrypted text', bytes_to_hex(text), bytes_to_hex(ctext) )

end subroutine unit_m_blowfish_test_003

! ############################################################################

subroutine unit_m_blowfish_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_blowfish_after_003

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_blowfish_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_blowfish_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_m_blowfish_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_blowfish_suite_after

end module unit_m_blowfish_tests

