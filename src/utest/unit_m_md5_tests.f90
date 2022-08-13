module unit_m_md5_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_md5
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

  use m_xfunit
  use m_messages

  use m_md5

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_md5_test_001

  public unit_m_md5_test_002

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'unknown'
  character(len=*), parameter :: module = 'm_md5'
  
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

subroutine unit_m_md5_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested structure
  type(t_md5) :: hash

! Basic short text (< 64)
  character(len=*), parameter :: short = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit'

! Basic long text (> 64)
  character(len=*), parameter :: long = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' // &
                                        'In sit amet auctor lacus. Nulla lacinia iaculis auctor.'

! Very long text (> 256)
  character(len=*), parameter :: ulong = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' // &
                                         'Curabitur convallis blandit dignissim. Ut ac porta turpis. ' // &
                                         'In eu sollicitudin lacus. Sed nec urna imperdiet odio ' // &
                                         'consectetur accumsan. Curabitur pharetra blandit tortor ' // &
                                         'faucibus tincidunt. Interdum et malesuada fames ac ante ipsum ' // &
                                         'primis in faucibus. Vivamus placerat, nulla quis commodo ' // &
                                         'iaculis, nibh enim porta odio, a condimentum nulla purus sed ligula.'

! Reset error handling structures
  call msg%reset_error()

! Initialise
  hash = md5()
  
! Get the hash of the short string
  call hash%encode( short )
  call ut%assert_less( 'Short < 64', len_trim(short), 64 )
  call ut%assert_equal( 'Short string md5', &
                        bytes_to_hex(hash%get_hash(),lower=.true.), &
                        'fc10a08df7fafa3871166646609e1c95', &
                        ignorecase=.true. )

! Get the hash of the long string
  call hash%encode( long )
  call ut%assert_greater( 'Long > 64', len_trim(long), 64 )
  call ut%assert_equal( 'Long string md5', &
                        bytes_to_hex(hash%get_hash(),lower=.true.), &
                        '4919612eabf60423d051139b34a31520', &
                        ignorecase=.true. )

! Get the hash of the 64-byte string
! Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabit
  call hash%encode( ulong(:64) )
  call ut%assert_equal( '64-byte string md5', &
                        bytes_to_hex(hash%get_hash(),lower=.true.), &
                        'f90453e3240587cb9308926470dbcf7e', &
                        ignorecase=.true. ) 

! Get the hash of the 128-byte string
! Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur convallis blandit dignissim. Ut ac porta turpis. In eu sollic
  call hash%encode( ulong(:128) )
  call ut%assert_equal( '128-byte string md5', &
                        bytes_to_hex(hash%get_hash(),lower=.true.), &
                        '7f488689e7fa127fc23bb60cd03538c8', &
                        ignorecase=.true. )

! Get the hash of the 129-byte string
! Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur convallis blandit dignissim. Ut ac porta turpis. In eu sollici
  call hash%encode( ulong(:129) )
  call ut%assert_equal( '129-byte string md5', &
                        bytes_to_hex(hash%get_hash(),lower=.true.), &
                        '0d880c776f8db07a23fd34e8084dca7a', &
                        ignorecase=.true. )
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_md5_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_md5_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! SHA structure
  type(t_md5) :: hash
  
! The test file
  integer, parameter :: unit = 17
  character(len=:), allocatable :: fname
  integer :: iostat

! Reset error handling structures
  call msg%reset_error()

! Initialise
  hash = md5()

! Open the file
  fname = trim(manager%get_unit_data_dir()) // '/grace04c.xml'
  open( unit, file=fname, access='stream', action='read', iostat=iostat )
  if( iostat == 0 ) then

!   Digest the file
    call hash%encode( unit, iostat )
    call ut%assert_equal( 'File: grace04c.xml', &
                          bytes_to_hex(hash%get_hash(),lower=.true.), &
                          '3f79fa10d59a33b68262e7839f401821', &
                          ignorecase=.true. )
    if( iostat == 0 ) then

!     Digest a second time to check for hysteresis
      rewind(unit)
      call hash%encode( unit, iostat )
      call ut%assert_equal( 'File: grace04c.xml (rehash)', &
                            bytes_to_hex(hash%get_hash(),lower=.true.), &
                            '3f79fa10d59a33b68262e7839f401821', &
                            ignorecase=.true. )
      if( iostat /= 0 ) then
        call msg%error( 'unit_m_md5_test', 'unit_m_md5_test_002', &
                         3, 'Re-digesting file' )
      end if

    else
      call msg%error( 'unit_m_md5_test', 'unit_m_md5_test_002', &
                       2, 'Digesting file' )
    end if

!   Close the file
    close( unit )

  else
    call msg%error( 'unit_m_md5_test', 'unit_m_md5_test_002', &
                     1, 'Opening file' )
  end if

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_md5_test_002

end module unit_m_md5_tests

