module unit_m_blowfish_c

!-------------------------------------------------------------------------------
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
  
  use, intrinsic :: iso_c_binding

! Local variables
  type, bind(c) :: c_blowfish
    integer(c_long), dimension(16+2)  :: p = 0
    integer(c_long), dimension(256,4) :: s = 0
  end type c_blowfish


! C operation interfaces

  interface
    subroutine blowfish_init0( c_ctx ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_blowfish
      type(c_blowfish),     intent(inout) :: c_ctx
    end subroutine blowfish_init0
  end interface


  interface
    subroutine blowfish_init( c_ctx, key, keylen ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_blowfish
      type(c_blowfish),        intent(inout) :: c_ctx
      character, dimension(*), intent(in)    :: key
      integer(kind=4), value,  intent(in)    :: keylen
    end subroutine blowfish_init
  end interface


  interface
    function f( c_ctx, x ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_blowfish
      type(c_blowfish),        intent(inout) :: c_ctx
      integer(kind=4),  value, intent(in)    :: x
      integer(kind=4) :: f
    end function f
  end interface


  interface
    subroutine blowfish_encrypt( c_ctx, left, right ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_blowfish
      type(c_blowfish), intent(in)    :: c_ctx
      integer(kind=4),  intent(inout) :: left
      integer(kind=4),  intent(inout) :: right
    end subroutine blowfish_encrypt
  end interface


  interface
    subroutine blowfish_decrypt( c_ctx, left, right ) bind(c)
      use, intrinsic :: iso_c_binding
      import c_blowfish
      type(c_blowfish), intent(in)    :: c_ctx
      integer(kind=4),  intent(inout) :: left
      integer(kind=4),  intent(inout) :: right
    end subroutine blowfish_decrypt
  end interface

end module unit_m_blowfish_c
