module unit_m_aes_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_aes
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

  use m_aes
  use unit_m_aes_c

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_aes_suite_before
  public unit_m_aes_suite_after

  public unit_m_aes_test_001
  public unit_m_aes_before_001
  public unit_m_aes_after_001

  public unit_m_aes_test_002
  public unit_m_aes_before_002
  public unit_m_aes_after_002

  public unit_m_aes_test_003
  public unit_m_aes_before_003
  public unit_m_aes_after_003

  public unit_m_aes_test_004
  public unit_m_aes_before_004
  public unit_m_aes_after_004

  public unit_m_aes_test_005
  public unit_m_aes_before_005
  public unit_m_aes_after_005

  public unit_m_aes_test_006
  public unit_m_aes_before_006
  public unit_m_aes_after_006

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_aes'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite
  type(t_xfunit_unit), save :: ut

! The error handling structure
  type(t_messages), save :: msg

! Encryption variables
  integer(kind=1), save, bind(c), dimension(16) :: text
  integer(kind=1), save, bind(c), dimension(32) :: key, cpk
  integer(kind=1), save, bind(c) :: rc
  type(c_aes), save :: c_ctx

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_aes_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_before_001

! ############################################################################

subroutine unit_m_aes_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(size(text)) :: local, clocal

! Invoke aes_sub_bytes
  local = text
  call unit_test_private_aes_sub_bytes( local )

! Invoke aes_subbytes from C reference
  clocal = text
  call aes_subbytes( clocal )

! Assert
  call ut%assert_equal( 'Sub-types (direct)', bytes_to_hex(local), bytes_to_hex(clocal) )
  
! Invoke aes_sub_bytes_inv
  local = text
  call unit_test_private_aes_sub_bytes_inv( local )

! Invoke aes_subbytes_inv from C reference
  clocal = text
  call aes_subbytes_inv( clocal )

! Assert
  call ut%assert_equal( 'Sub-types (inverse)', bytes_to_hex(local), bytes_to_hex(clocal) )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_test_001

! ############################################################################

subroutine unit_m_aes_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_aes_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_before_002

! ############################################################################

subroutine unit_m_aes_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(size(text)) :: local, clocal
  integer(kind=1), dimension(size(cpk)) :: cpy, ccpy

! Invoke aes_add_round_key
  local = text
  call unit_test_private_aes_add_round_key( local, key )

! Invoke aes_addroundkey from C reference
  clocal = text
  call aes_addroundkey( clocal, key )

! Assert
  call ut%assert_equal( 'Add round key', bytes_to_hex(local), bytes_to_hex(clocal) )
  
! Invoke aes_add_round_key_cpy
  local = text
  call unit_test_private_aes_add_round_key_cpy( local, key, cpy )

! Invoke aes_addroundkey_cpy from C reference
  clocal = text
  call aes_addroundkey_cpy( clocal, key, ccpy )

! Assert
  call ut%assert_equal( 'Add round key CPY (text)', bytes_to_hex(local), bytes_to_hex(clocal) )
  call ut%assert_equal( 'Add round key CPY (copy key)', bytes_to_hex(cpy), bytes_to_hex(ccpy) )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_test_002

! ############################################################################

subroutine unit_m_aes_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_aes_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_before_003

! ############################################################################

subroutine unit_m_aes_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(size(text)) :: local, clocal

! Invoke aes_shift_rows
  local = text
  call unit_test_private_aes_shift_rows( local )

! Invoke aes_shiftrows from C reference
  clocal = text
  call aes_shiftrows( clocal )

! Assert
  call ut%assert_equal( 'Shift rows (direct)', bytes_to_hex(local), bytes_to_hex(clocal) )
  
! Invoke aes_shift_rows_inv
  local = text
  call unit_test_private_aes_shift_rows_inv( local )

! Invoke aes_shiftrows_inv from C reference
  clocal = text
  call aes_shiftrows_inv( clocal )

! Assert
  call ut%assert_equal( 'Shift rows (inverse)', bytes_to_hex(local), bytes_to_hex(clocal) )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_test_003

! ############################################################################

subroutine unit_m_aes_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_after_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_aes_before_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_before_004

! ############################################################################

subroutine unit_m_aes_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(size(text)) :: local, clocal

! Invoke aes_mix_columns
  local = text
  call unit_test_private_aes_mix_columns( local )

! Invoke aes_mixcolumns from C reference
  clocal = text
  call aes_mixcolumns( clocal )

! Assert
  call ut%assert_equal( 'Mix columns (direct)', bytes_to_hex(local), bytes_to_hex(clocal) )
  
! Invoke aes_mix_columns_inv
  local = text
  call unit_test_private_aes_mix_columns_inv( local )

! Invoke aes_mixcolumns_inv from C reference
  clocal = text
  call aes_mixcolumns_inv( clocal )

! Assert
  call ut%assert_equal( 'Mix columns (inverse)', bytes_to_hex(local), bytes_to_hex(clocal) )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_test_004

! ############################################################################

subroutine unit_m_aes_after_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_after_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_aes_before_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_before_005

! ############################################################################

subroutine unit_m_aes_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(size(key)) :: local, clocal
  integer(kind=1) :: lrc, clrc

! Invoke aes_expand_encryption_key
  local = key
  lrc = 1_1
  call unit_test_private_aes_expand_encryption_key( local, lrc )

! Invoke aes_expandenckey from C reference
  clocal = key
  clrc = 1_1
  call aes_expandenckey( clocal, clrc )

! Assert
  call ut%assert_equal( 'Encryption key', bytes_to_hex(local), bytes_to_hex(clocal) )
  call ut%assert_equal( 'Encryption rc', lrc, clrc )

! Invoke aes_expand_decryption_key
  local = key
  lrc = 1_1
  call unit_test_private_aes_expand_decryption_key( local, lrc )

! Invoke aes_expanddeckey from C reference
  clocal = key
  clrc = 1_1
  call aes_expanddeckey( clocal, clrc )

! Assert
  call ut%assert_equal( 'Decryption key', bytes_to_hex(local), bytes_to_hex(clocal) )
  call ut%assert_equal( 'Decryption rc', lrc, clrc )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_test_005

! ############################################################################

subroutine unit_m_aes_after_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_after_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_aes_before_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_before_006

! ############################################################################

subroutine unit_m_aes_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The AES context
  type(t_aes) :: ctx

! Local variables
  integer(kind=1), dimension(size(text)) :: local, clocal

! Initialise the encrypter
  ctx = aes( key )

! Encrypt
  call ctx%encrypt( text, local )

! Encrypt using the C reference
  call aes256_init( c_ctx, key )
  clocal = text
  call aes256_encrypt_ecb( c_ctx, clocal )

! Assert
  call ut%assert_equal( 'Encrypted text', bytes_to_hex(local), bytes_to_hex(clocal) )
  
! Finalise
  call aes256_done( c_ctx )

! Initialise the decrypter
  ctx = aes( key )

! Encrypt
  call ctx%decrypt( local, local )

! Encrypt using the C reference
  call aes256_init( c_ctx, key )
  call aes256_decrypt_ecb( c_ctx, clocal )

! Assert
  call ut%assert_equal( 'Decrypted text', bytes_to_hex(local), bytes_to_hex(clocal) )
  
! Finalise
  call aes256_done( c_ctx )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_test_006

! ############################################################################

subroutine unit_m_aes_after_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_aes_after_006


! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_aes_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite

! Local variables
  integer(kind=1) :: i

! Initilise default text
  do i = 1_1, size(text)
    text(i) = (i - 1_1) * 16_1 + ( i - 1_1 )
  end do

! Initilise default key
  do i = 1_1, size(key)
    key(i) = i - 1_1
  end do

end subroutine unit_m_aes_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_m_aes_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite

end subroutine unit_m_aes_suite_after

end module unit_m_aes_tests

