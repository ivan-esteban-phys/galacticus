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

  !% Contains a module which implements a property operator class in which the property value is replaced with an integral over a
  !% normal distribution between given limits, using the property value at the mean of the distribution.

  !# <outputAnalysisPropertyOperator name="outputAnalysisPropertyOperatorNormal">
  !#  <description>A property operator class in which the property value is replaced with an integral over a normal distribution between given limits, using the property value at the mean of the distribution.</description>
  !# </outputAnalysisPropertyOperator>
  type, extends(outputAnalysisPropertyOperatorClass) :: outputAnalysisPropertyOperatorNormal
     !% A normal property operator class.
     private
     double precision :: rangeLower  , rangeUpper , &
          &              extentLower , extentUpper, &
          &              rootVariance
   contains
     procedure :: operate => normalOperate
  end type outputAnalysisPropertyOperatorNormal

  interface outputAnalysisPropertyOperatorNormal
     !% Constructors for the ``normal'' output analysis class.
     module procedure normalConstructorParameters
     module procedure normalConstructorInternal
  end interface outputAnalysisPropertyOperatorNormal

contains

  function normalConstructorParameters(parameters) result(self)
    !% Constructor for the ``normal'' output analysis property operator class which takes a parameter set as input.
    use :: Input_Parameters, only : inputParameter, inputParameters
    implicit none
    type            (outputAnalysisPropertyOperatorNormal)                :: self
    type            (inputParameters                     ), intent(inout) :: parameters
    double precision                                                      :: rangeLower  , rangeUpper , &
         &                                                                   extentLower , extentUpper, &
         &                                                                   rootVariance

    ! Check and read parameters.
    !# <inputParameter>
    !#   <name>rangeLower</name>
    !#   <source>parameters</source>
    !#   <description>Lower integration limit for the normal distribution weight operator.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>rangeUpper</name>
    !#   <source>parameters</source>
    !#   <description>Upper integration limit for the normal distribution weight operator.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>rootVariance</name>
    !#   <source>parameters</source>
    !#   <description>Root variance for the normal distribution weight operator.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>extentLower</name>
    !#   <defaultValue>-huge(0.0d0)</defaultValue>
    !#   <source>parameters</source>
    !#   <description>Lower extent for the normal distribution weight operator.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>extentUpper</name>
    !#   <defaultValue>+huge(0.0d0)</defaultValue>
    !#   <source>parameters</source>
    !#   <description>Upper extent for the normal distribution weight operator.</description>
    !# </inputParameter>
    self=outputAnalysisPropertyOperatorNormal(rangeLower,rangeUpper,extentLower,extentUpper,rootVariance)
    !# <inputParametersValidate source="parameters"/>
    return
  end function normalConstructorParameters

  function normalConstructorInternal(rangeLower,rangeUpper,extentLower,extentUpper,rootVariance) result (self)
    !% Internal constructor for the ``normal'' output analysis distribution operator class.
    implicit none
    type            (outputAnalysisPropertyOperatorNormal)                :: self
    double precision                                      , intent(in   ) :: rangeLower  , rangeUpper , &
         &                                                                   extentLower , extentUpper, &
         &                                                                   rootVariance
    !# <constructorAssign variables="rangeLower, rangeUpper, extentLower , extentUpper, rootVariance"/>

    return
  end function normalConstructorInternal

  double precision function normalOperate(self,propertyValue,node,propertyType,outputIndex)
    !% Implement an normal output analysis property operator.
    use            :: Error_Functions, only : Error_Function
    use, intrinsic :: ISO_C_Binding  , only : c_size_t
    implicit none
    class           (outputAnalysisPropertyOperatorNormal), intent(inout)           :: self
    double precision                                      , intent(in   )           :: propertyValue
    type            (treeNode                            ), intent(inout), optional :: node
    integer                                               , intent(inout), optional :: propertyType
    integer         (c_size_t                            ), intent(in   ), optional :: outputIndex
    !$GLC attributes unused :: propertyType, outputIndex, node

    normalOperate=+(                                                                               &
         &          +Error_Function((self% rangeUpper-propertyValue)/sqrt(2.0d0)/self%rootVariance) &
         &          -Error_Function((self% rangeLower-propertyValue)/sqrt(2.0d0)/self%rootVariance) &
         &         )                                                                               &
         &        /(                                                                               &
         &          +Error_Function((self%extentUpper-propertyValue)/sqrt(2.0d0)/self%rootVariance) &
         &          -Error_Function((self%extentLower-propertyValue)/sqrt(2.0d0)/self%rootVariance) &
         &         )
    return
  end function normalOperate
