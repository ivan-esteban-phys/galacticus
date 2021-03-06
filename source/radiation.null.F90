!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,
!!           2019, 2020, 2021
!!    Andrew Benson <abenson@carnegiescience.edu>
!!
!! This file is part of Galacticus.
!!
!!    Galacticus is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.
!!
!!    Galacticus is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with Galacticus.  If not, see <http://www.gnu.org/licenses/>.

  !% Implements a class for null radiation fields.

  !# <radiationField name="radiationFieldNull">
  !#  <description>A radiation field class for null fields.</description>
  !# </radiationField>
  type, extends(radiationFieldClass) :: radiationFieldNull
     !% A radiation field class for null fields.
     private
   contains
     procedure :: flux => nullFlux
  end type radiationFieldNull

  interface radiationFieldNull
     !% Constructors for the {\normalfont \ttfamily null} radiation field class.
     module procedure nullConstructorParameters
  end interface radiationFieldNull

contains

  function nullConstructorParameters(parameters) result(self)
    !% Constructor for the {\normalfont \ttfamily null} radiation field class which takes a parameter list as input.
    use :: Input_Parameters, only : inputParameters
    implicit none
    type(radiationFieldNull)                :: self
    type(inputParameters   ), intent(inout) :: parameters
    !$GLC attributes unused :: parameters

    self=radiationFieldNull()
    return
  end function nullConstructorParameters

  double precision function nullFlux(self,wavelength,node)
    !% Return the flux of a null radiation field.
    implicit none
    class           (radiationFieldNull), intent(inout) :: self
    double precision                    , intent(in   ) :: wavelength
    type            (treeNode          ), intent(inout) :: node
    !$GLC attributes unused :: self, wavelength, node

    nullFlux=0.0d0
    return
  end function nullFlux
