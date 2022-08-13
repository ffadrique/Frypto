module unit_m_crypto_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_crypto
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
  use m_messages

  use m_xfunit
  use m_crypto
  use m_crypto_padding

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_crypto_test_001
  public unit_m_crypto_before_001
  public unit_m_crypto_after_001

  public unit_m_crypto_test_002
  public unit_m_crypto_before_002
  public unit_m_crypto_after_002

  public unit_m_crypto_test_003
  public unit_m_crypto_before_003
  public unit_m_crypto_after_003

  public unit_m_crypto_test_004

  public unit_m_crypto_test_005

  public unit_m_crypto_test_006

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_crypto'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager

! The error handling structure
  type(t_messages), save :: msg

! Test case strcuture
  type t_crypto_case
    integer :: mode = crypto_mode_ecb
    character(len=:), allocatable :: key
    character(len=:), allocatable :: iv
    character(len=:), allocatable :: text
    character(len=:), allocatable :: cipher    
  end type t_crypto_case

! Test cases
  type(t_string), dimension(:), allocatable, target, save :: des_test_list
  type(t_string), dimension(:), allocatable, target, save :: aes_test_list
  type(t_string), dimension(:), allocatable, target, save :: blowfish_test_list

! Active list of test cases
  type(t_string), save, dimension(:), pointer :: test_list => null()

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crypto_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Initialise the test list
  allocate( aes_test_list(40) )
  aes_test_list = [ &
     string('CBCGFSbox256d.txt    '), &
     string('CBCGFSbox256e.txt    '), &
     string('CBCKeySbox256d.txt   '), &
     string('CBCKeySbox256e.txt   '), &
     string('CBCMMT256d.txt       '), &
     string('CBCMMT256e.txt       '), &
     string('CBCVarKey256d.txt    '), &
     string('CBCVarKey256e.txt    '), &
     string('CBCVarTxt256d.txt    '), &
     string('CBCVarTxt256e.txt    '), &
     string('CFB128GFSbox256d.txt '), &
     string('CFB128GFSbox256e.txt '), &
     string('CFB128KeySbox256d.txt'), &
     string('CFB128KeySbox256e.txt'), &
     string('CFB128MMT256d.txt    '), &
     string('CFB128MMT256e.txt    '), &
     string('CFB128VarKey256d.txt '), &
     string('CFB128VarKey256e.txt '), &
     string('CFB128VarTxt256d.txt '), &
     string('CFB128VarTxt256e.txt '), &
     string('ECBGFSbox256d.txt    '), &
     string('ECBGFSbox256e.txt    '), &
     string('ECBKeySbox256d.txt   '), &
     string('ECBKeySbox256e.txt   '), &
     string('ECBMMT256d.txt       '), &
     string('ECBMMT256e.txt       '), &
     string('ECBVarKey256d.txt    '), &
     string('ECBVarKey256e.txt    '), &
     string('ECBVarTxt256d.txt    '), &
     string('ECBVarTxt256e.txt    '), &
     string('OFBGFSbox256d.txt    '), &
     string('OFBGFSbox256e.txt    '), &
     string('OFBKeySbox256d.txt   '), &
     string('OFBKeySbox256e.txt   '), &
     string('OFBMMT256d.txt       '), &
     string('OFBMMT256e.txt       '), &
     string('OFBVarKey256d.txt    '), &
     string('OFBVarKey256e.txt    '), &
     string('OFBVarTxt256d.txt    '), &
     string('OFBVarTxt256e.txt    ') ]

! Reset error handling structures
  call msg%reset_error()

! Allocate the common list of tests
  test_list => aes_test_list

end subroutine unit_m_crypto_before_001

! ############################################################################

subroutine unit_m_crypto_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Trace
  write( 6, '(a)' ) character(ut%get_name())

! Process the test case using the AES algorithm
  call process_test_cases( ut, crypto_aes256, msg )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_crypto_test_001

! ############################################################################

subroutine unit_m_crypto_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut
  
! Release test cases array
  deallocate( aes_test_list )

end subroutine unit_m_crypto_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crypto_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Initialise the test list
  allocate( des_test_list(32) )
  des_test_list = [ &
     string('CBCMMTKey1d.txt  '), &
     string('CBCMMTKey1e.txt  '), &
     string('CBCMMTKey2d.txt  '), &
     string('CBCMMTKey2e.txt  '), &
     string('CBCMMTKey3d.txt  '), &
     string('CBCMMTKey3e.txt  '), &
     string('CFB64MMTKey1d.txt'), &
     string('CFB64MMTKey1e.txt'), &
     string('CFB64MMTKey2d.txt'), &
     string('CFB64MMTKey2e.txt'), &
     string('CFB64MMTKey3d.txt'), &
     string('CFB64MMTKey3e.txt'), &
     string('ECBMMTKey1d.txt  '), &
     string('ECBMMTKey1e.txt  '), &
     string('ECBMMTKey2d.txt  '), &
     string('ECBMMTKey2e.txt  '), &
     string('ECBMMTKey3d.txt  '), &
     string('ECBMMTKey3e.txt  '), &
     string('ECBPerKeyd.txt   '), &
     string('ECBPerKeye.txt   '), &
     string('ECBSubTabd.txt   '), &
     string('ECBSubTabe.txt   '), &
     string('ECBVarKeyd.txt   '), &
     string('ECBVarKeye.txt   '), &
     string('ECBVarTxtd.txt   '), &
     string('ECBVarTxte.txt   '), &
     string('OFBMMTKey1d.txt  '), &
     string('OFBMMTKey1e.txt  '), &
     string('OFBMMTKey2d.txt  '), &
     string('OFBMMTKey2e.txt  '), &
     string('OFBMMTKey3d.txt  '), &
     string('OFBMMTKey3e.txt  ') ]

! Reset error handling structures
  call msg%reset_error()

! Allocate the common list of tests
  test_list => des_test_list

end subroutine unit_m_crypto_before_002

! ############################################################################

subroutine unit_m_crypto_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Trace
  write( 6, '(a)' ) character(ut%get_name())

! Process the test case using the 3-DES algorithm
  call process_test_cases( ut, crypto_triple_des, msg )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_crypto_test_002

! ############################################################################

subroutine unit_m_crypto_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Release test cases array
  deallocate( des_test_list )

end subroutine unit_m_crypto_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crypto_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Initialise the test list
  allocate( blowfish_test_list(20) )
  blowfish_test_list = [ &
     string('CBCGFSbox256d.txt    '), &
     string('CBCGFSbox256e.txt    '), &
     string('CBCKeySbox256d.txt   '), &
     string('CBCKeySbox256e.txt   '), &
     string('CBCMMT256d.txt       '), &
     string('CBCMMT256e.txt       '), &
     string('CBCVarKey256d.txt    '), &
     string('CBCVarKey256e.txt    '), &
     string('CBCVarTxt256d.txt    '), &
     string('CBCVarTxt256e.txt    '), &
     string('ECBGFSbox256d.txt    '), &
     string('ECBGFSbox256e.txt    '), &
     string('ECBKeySbox256d.txt   '), &
     string('ECBKeySbox256e.txt   '), &
     string('ECBMMT256d.txt       '), &
     string('ECBMMT256e.txt       '), &
     string('ECBVarKey256d.txt    '), &
     string('ECBVarKey256e.txt    '), &
     string('ECBVarTxt256d.txt    '), &
     string('ECBVarTxt256e.txt    ') ]

! Reset error handling structures
  call msg%reset_error()

! Allocate the common list of tests
  test_list => blowfish_test_list

end subroutine unit_m_crypto_before_003

! ############################################################################

subroutine unit_m_crypto_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Trace
  write( 6, '(a)' ) character(ut%get_name())

! Process the test case using the blowfish algorithm
  call process_test_cases( ut, crypto_blowfish, msg )

! Verify the execution completion
  if( msg%on_error() ) then
    call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_crypto_test_003

! ############################################################################

subroutine unit_m_crypto_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Release test cases list
  deallocate( blowfish_test_list )

end subroutine unit_m_crypto_after_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crypto_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Encryption variables
  type(t_crypto) :: ctx
  integer(kind=1), dimension(:), allocatable :: key, iv
  character(len=:), allocatable :: text
  integer(kind=1), dimension(:), allocatable :: cipher, decipher
  
! Initialise
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee3587ca4730c3d33b691df38bab076bc558" )
  iv = hex_to_bytes( "00000000000000000000000000000000" )
  text = "Lorem ipsum dolor sit amet consectetur adipiscing"
  
! Encrypt/decypt with zero padding
  ctx = crypto( crypto_aes256, key, crypto_mode_cbc, crypto_padding_zero, iv )
  call ctx%encrypt( character_to_bytes(text), cipher )
  call ut%assert_equal( "Zero padding (encrypt)", bytes_to_hex(cipher), "2A50092D8A40F22B215651AE33CE2E286FA17BCFB3980107C88C2FE90F82F1BFE5B44984C76F638B89F6DAFCE4FA37ADC635A95AF66B37C5A680BA349CED3FE5" )
  call ctx%reset()
  call ctx%decrypt( cipher, decipher )
  call ut%assert_equal( "Zero padding (decrypt)", bytes_to_character(decipher), text )
  
! Encrypt/decypt with ANSIX923 padding
  ctx = crypto( crypto_aes256, key, crypto_mode_cbc, crypto_padding_ansix923, iv )
  call ctx%encrypt( character_to_bytes(text), cipher )
  call ut%assert_equal( "ANSIX923 padding (encrypt)", bytes_to_hex(cipher), "2A50092D8A40F22B215651AE33CE2E286FA17BCFB3980107C88C2FE90F82F1BFE5B44984C76F638B89F6DAFCE4FA37AD65D6EA28C0320E51B9CFFE2273D23129" )
  call ctx%reset()
  call ctx%decrypt( cipher, decipher )
  call ut%assert_equal( "ANSIX923 padding (decrypt)", bytes_to_character(decipher), text )
  
! Encrypt/decypt with PKCS7 padding
  ctx = crypto( crypto_aes256, key, crypto_mode_cbc, crypto_padding_pkcs7, iv )
  call ctx%encrypt( character_to_bytes(text), cipher )
  call ut%assert_equal( "PKCS7 padding (encrypt)", bytes_to_hex(cipher), "2A50092D8A40F22B215651AE33CE2E286FA17BCFB3980107C88C2FE90F82F1BFE5B44984C76F638B89F6DAFCE4FA37ADB5C0CF1669A40BE8EBA5200A7E3527E1" )
  call ctx%reset()
  call ctx%decrypt( cipher, decipher )
  call ut%assert_equal( "PKCS7 padding (decrypt)", bytes_to_character(decipher), text )
  
! Verify the execution completion
  if( msg%on_error() ) then
    call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_crypto_test_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crypto_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Encryption variables
  type(t_crypto) :: object
  integer(kind=1), dimension(:), allocatable :: key, iv

! Input/output files
  type(t_string) :: filein, fileout, fileref
  
! Initialise
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee3587ca4730c3d33b691df38bab076bc558" )
  iv = hex_to_bytes( "eec088511991aec12d7dc73c07fd334d" )
  filein = trim(manager%get_unit_data_dir()) // '/finals2000a.xml'
  fileout = 'finals2000a.out.encrypted'
  fileref = trim(manager%get_unit_data_dir()) // '/finals2000a.xml.encrypted'

! Initialise the ecnyption
  object = crypto( crypto_aes256, key, crypto_mode_cbc, padding=crypto_padding_zero, inivec=iv )

! Encrypt the file
  call object%encrypt_file( filein%character(), fileout%character(), msg  )
  if( .not. msg%on_error() ) then
  
!   Compare file
    call ut%assert_compare_files( 'Encryption', fileout, fileref )
  
  else
    call msg%error( 'unit_m_crypto_tests', 'unit_m_crypto_test_005', 1, 'Encrypting input' )
  end if
  
! Verify the execution completion
  if( msg%on_error() ) then
    call ut%error( 1, 'Errors during unit test execution', msg )
  end if
  
end subroutine unit_m_crypto_test_005
  
! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_crypto_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Encryption variables
  type(t_crypto) :: object
  integer(kind=1), dimension(:), allocatable :: key, iv

! Input/output files
  type(t_string) :: filein, fileout, fileref
  
! Initialise
  key = hex_to_bytes( "c47b0294dbbbee0fec4757f22ffeee3587ca4730c3d33b691df38bab076bc558" )
  iv = hex_to_bytes( "eec088511991aec12d7dc73c07fd334d" )
  filein = trim(manager%get_unit_data_dir()) // '/finals2000a.xml.encrypted'
  fileout = 'finals2000a.out'
  fileref = trim(manager%get_unit_data_dir()) // '/finals2000a.xml'

! Initialise the ecnyption
  object = crypto( crypto_aes256, key, crypto_mode_cbc, padding=crypto_padding_zero, inivec=iv )

! Decrypt the file
  call object%decrypt_file( filein%character(), fileout%character(), msg  )
  if( .not. msg%on_error() ) then
  
!   Compare file
    call ut%assert_compare_files( 'Decryption', fileout, fileref )
  
  else
    call msg%error( 'unit_m_crypto_tests', 'unit_m_crypto_test_006', 1, 'Decrypting input' )
  end if
  
! Verify the execution completion
  if( msg%on_error() ) then
    call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_crypto_test_006
  
! ############################################################################
! ## Auxiliary functions #####################################################
! ############################################################################

! Process all files in a test case
subroutine process_test_cases( ut, algorithm, msg )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The encryption algrithm
  integer, intent(in) :: algorithm

! The error handling structure
  type(t_messages), intent(inout) :: msg

! The crypto context
  type(t_crypto) :: ctx

! Local variables
  type(t_crypto_case), dimension(:), allocatable :: crypto_case
  integer :: ifile
  integer :: ios
  integer :: icase
  integer :: mode
  character :: direction
  character(len=:), allocatable :: name
  type(t_string) :: file
  integer(kind=1), dimension(:), allocatable :: key, iv, text, cipher
  
! Loop on the files
  do ifile = 1, size(test_list)

!   Trace
    write( 6, '(a)' ) test_list(ifile)%character()

!   Load the file
    file = trim(manager%get_unit_data_dir())
    if( algorithm == crypto_aes256 ) then
      file = file // '/aes/'
    else if( algorithm == crypto_triple_des ) then
      file = file // '/3des/'
    else if( algorithm == crypto_blowfish ) then
      file = file // '/blowfish/'
    end if
    file = file // test_list(ifile)
    call read_test_file( file%character(), crypto_case, mode, direction, ios )
    if( ios == 0 ) then

!     Loop on the crypto cases
      do icase = 1, size(crypto_case)

!       Initialise the test name
        name = test_list(ifile)
        name = name(:index(name,".")-1)
        name = name // '#' // trim(character(icase-1))

!       Initialise the context
        if(allocated(key)) deallocate(key)
        allocate( key, source=hex_to_bytes(crypto_case(icase)%key) )
        if( allocated(crypto_case(icase)%iv) ) then
          if(allocated(iv)) deallocate(iv)
          allocate( iv, source=hex_to_bytes(crypto_case(icase)%iv) )
          ctx = crypto( algorithm, key, mode, inivec=iv )
        else
          ctx = crypto( algorithm, key, mode )
        end if

!       Encrypt/decrypt and assert
        if( allocated(text) ) deallocate(text)
        if( allocated(cipher)  ) deallocate(cipher)
        if( direction == 'E' ) then
          allocate( text, source=hex_to_bytes(crypto_case(icase)%text) )
          call ctx%encrypt( text, cipher )
          call ut%assert_equal( name, bytes_to_hex(cipher), crypto_case(icase)%cipher, ignorecase=.true. )
        else
          allocate( cipher, source=hex_to_bytes(crypto_case(icase)%cipher) )
          call ctx%decrypt( cipher, text, no_padding=.true. )
          call ut%assert_equal( name, bytes_to_hex(text), crypto_case(icase)%text, ignorecase=.true. )
        end if

      end do                                                                                    
    
    else

!     Error
      call msg%error( 'unit_m_crypto_test', character(ut%get_name()), &
                      1, 'Reading test file "' // trim(test_list(ifile)%character()) // '"' )

    end if

!   Terminate structures
    if( allocated( crypto_case ) ) then
      deallocate( crypto_case )
    end if

  end do

end subroutine process_test_cases


! Read one test case file
subroutine read_test_file( file, crypto_case, mode, direction, ios )

! Test file
  character(len=*), intent(in) :: file

! The list of test cases
  type(t_crypto_case), dimension(:), allocatable :: crypto_case
  
! The encryption mode
  integer, intent(out) :: mode
  
! The encryption direction
  character, intent(out) :: direction
  
! The completion status
  integer, intent(out) :: ios

! Local variables
  integer, parameter :: ilun = 17
  integer :: n
  character(len=256) :: rec
  integer :: idx

! Process the file name
  idx = index( file, '/', back=.true. ) + 1
  select case( file(idx:idx+2) )
    case( 'ECB' )
      mode = crypto_mode_ecb
    case( 'CBC' )
      mode = crypto_mode_cbc
    case( 'CFB' )
      mode = crypto_mode_cfb
    case( 'OFB' )
      mode = crypto_mode_ofb
    case default
      mode = crypto_mode_ecb
  end select

! Open the file
  open( ilun, file=file, status='old', action='read', iostat=ios )
  if( ios == 0 ) then

!   Count the test cases in the file
    n = 0
    do
      read( ilun, '(a256)', iostat=ios ) rec
      if( ios == 0 ) then
        if( rec(:len('COUNT')) == 'COUNT' ) n = n + 1
      else
        exit
      end if
    end do

!   Allcoate the return array
    allocate( crypto_case(n) )

!   Get the encryption direction
    rewind(ilun)
    read( ilun, '(a256)', iostat=ios ) rec
    direction = rec(2:2)

!   Read the file details
    n = 0
    do

      rec = ''
      read( ilun, '(a256)', iostat=ios ) rec
      if( ios == 0 ) then

!       Process record
        if( rec(:len('COUNT')) == 'COUNT' ) then
          n = n + 1
!          print *, "COUNT     ='", n, "'"

        elseif( rec(:len('IV')) == 'IV' ) then
          crypto_case(n)%iv = trim(adjustl(rec(index(rec,'=')+1:)))
!          print *, "IV        ='", crypto_case(n)%iv, "'"

        elseif( rec(:len('KEY')) == 'KEY' ) then
          crypto_case(n)%key = trim(adjustl(rec(index(rec,'=')+1:)))
!          print *, "KEY       ='", crypto_case(n)%key, "'"

        elseif( rec(:len('PLAINTEXT')) == 'PLAINTEXT' ) then
          crypto_case(n)%text = trim(adjustl(rec(index(rec,'=')+1:)))
!          print *, "PLAINTEXT ='", crypto_case(n)%text, "'"

        elseif( rec(:len('CIPHERTEXT')) == 'CIPHERTEXT' ) then
          crypto_case(n)%cipher = trim(adjustl(rec(index(rec,'=')+1:)))
!          print *, "CIPHERTEXT='", crypto_case(n)%cipher, "'"

        end if

      else if( ios < 0 ) then
      
!       End of file
        ios = 0
        exit

      else

!       Error
        exit

      end if

    end do

!   Close the file
    close( ilun )

  end if

end subroutine read_test_file


end module unit_m_crypto_tests

