!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017
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

  !% Contains a module which implements an output analysis distribution operator class to account for errors on N-body measurements of halo spin.

  use Halo_Spin_Distributions

  !# <outputAnalysisDistributionOperator name="outputAnalysisDistributionOperatorSpinNBodyErrors">
  !#  <description>An output analysis distribution operator class to account for errors on N-body measurements of halo spin.</description>
  !# </outputAnalysisDistributionOperator>
  type, extends(outputAnalysisDistributionOperatorClass) :: outputAnalysisDistributionOperatorSpinNBodyErrors
     !% An output distribution operator class to account for errors on N-body measurements of halo spin.
     private
     class(haloSpinDistributionClass), pointer :: haloSpinDistribution_
   contains
     final     ::                        spinNBodyErrorsDestructor
     procedure :: operateScalar       => spinNBodyErrorsOperateScalar
     procedure :: operateDistribution => spinNBodyErrorsOperateDistribution
  end type outputAnalysisDistributionOperatorSpinNBodyErrors

  interface outputAnalysisDistributionOperatorSpinNBodyErrors
     !% Constructors for the ``spinNBodyErrors'' output analysis distribution operator class.
     module procedure spinNBodyErrorsConstructorParameters
     module procedure spinNBodyErrorsConstructorInternal
  end interface outputAnalysisDistributionOperatorSpinNBodyErrors

contains

  function spinNBodyErrorsConstructorParameters(parameters) result(self)
    !% Constructor for the ``spinNBodyErrors'' output analysis distribution operator class which takes a parameter set as input.
    use Input_Parameters
    implicit none
    type (outputAnalysisDistributionOperatorSpinNBodyErrors)                :: self
    type (inputParameters                                  ), intent(inout) :: parameters
    class(haloSpinDistributionClass                        ), pointer       :: haloSpinDistribution_

    ! Construct the object.
    !# <objectBuilder class="haloSpinDistribution" name="haloSpinDistribution_" source="parameters"/>
    self=outputAnalysisDistributionOperatorSpinNBodyErrors(haloSpinDistribution_)
    !# <inputParametersValidate source="parameters"/>
    return
  end function spinNBodyErrorsConstructorParameters

  function spinNBodyErrorsConstructorInternal(haloSpinDistribution_) result(self)
    !% Internal constructor for the ``spinNBodyErrors'' output analysis distribution operator class.
    use Galacticus_Error
    implicit none
    type (outputAnalysisDistributionOperatorSpinNBodyErrors)                        :: self
    class(haloSpinDistributionClass                        ), intent(in   ), target :: haloSpinDistribution_
    !# <constructorAssign variables="*haloSpinDistribution_"/>

    ! Check that the spin distribution is of the N-body errors class.
    select type (haloSpinDistribution_)
    class is (haloSpinDistributionNbodyErrors)
       ! This is OK, do nothing.
    class default
       ! This is not OK.
       call Galacticus_Error_Report('must provide an N-body errors halo spin distribution class'//{introspection:location})
    end select
    return
  end function spinNBodyErrorsConstructorInternal

  subroutine spinNBodyErrorsDestructor(self)
    !% Destructor for the ``spinNBodyErrors'' output analysis distribution operator class.
    implicit none
    type (outputAnalysisDistributionOperatorSpinNBodyErrors), intent(inout) :: self

    !# <objectDestructor name="self%haloSpinDistribution_" />
    return
  end subroutine spinNBodyErrorsDestructor

  function spinNBodyErrorsOperateScalar(self,propertyValue,propertyType,propertyValueMinimum,propertyValueMaximum,outputIndex,node)
    !% Implement an output analysis distribution operator which accounts for errors in N-body measurements of halo spin.
    use Output_Analyses_Options
    use FGSL
    use Numerical_Integration
    use Galacticus_Error
    implicit none
    class           (outputAnalysisDistributionOperatorSpinNBodyErrors), intent(inout)                                        :: self
    double precision                                                   , intent(in   )                                        :: propertyValue
    integer                                                            , intent(in   )                                        :: propertyType
    double precision                                                   , intent(in   ), dimension(:)                          :: propertyValueMinimum        , propertyValueMaximum
    integer         (c_size_t                                         ), intent(in   )                                        :: outputIndex
    type            (treeNode                                         ), intent(inout)                                        :: node
    double precision                                                                  , dimension(size(propertyValueMinimum)) :: spinNBodyErrorsOperateScalar
    class           (haloSpinDistributionClass                        ), allocatable                                          :: haloSpinDistribution_
    integer         (c_size_t                                         )                                                       :: i
    double precision                                                                                                          :: spinMeasuredMinimum         , spinMeasuredMaximum
    type            (fgsl_function                                    )                                                       :: integrandFunction
    type            (fgsl_integration_workspace                       )                                                       :: integrationWorkspace
    !GCC$ attributes unused :: outputIndex, propertyValue

    ! Make a private copy of the halo spin distribution object to avoid thread conflicts.
    allocate(haloSpinDistribution_,mold=self%haloSpinDistribution_)
    call self%haloSpinDistribution_%deepCopy(haloSpinDistribution_)
    !$omp critical(spinNBodyErrorsOperateScalar)
    do i=1,size(propertyValueMinimum)
       select case (propertyType)
       case (outputAnalysisPropertyTypeLinear)
          spinMeasuredMinimum=        propertyValueMinimum(i)
          spinMeasuredMaximum=        propertyValueMaximum(i)
       case (outputAnalysisPropertyTypeLog10)
          spinMeasuredMinimum=10.0d0**propertyValueMinimum(i)
          spinMeasuredMaximum=10.0d0**propertyValueMaximum(i)
       case default
          call Galacticus_Error_Report('unhandled property type'//{introspection:location})
       end select
       spinNBodyErrorsOperateScalar(i)=+Integrate(                                 &
            &                                    spinMeasuredMinimum             , &
            &                                    spinMeasuredMaximum             , &
            &                                    spinDistributionIntegrate       , &
            &                                    integrandFunction               , &
            &                                    integrationWorkspace            , &
            &                                    toleranceAbsolute        =0.0d+0, &
            &                                    toleranceRelative        =1.0d-6  &
            &                                   )                                  &
            &                          /(                                          &
            &                            +spinMeasuredMaximum                      &
            &                            -spinMeasuredMinimum                      &
            &                           )
       call Integrate_Done(integrandFunction,integrationWorkspace)
    end do
    !$omp end critical(spinNBodyErrorsOperateScalar)
    return
    
  contains
    
    double precision function spinDistributionIntegrate(spinMeasured)
      !% Integrand function used to find cumulative spin distribution over a bin.
      implicit none
      double precision, intent(in   ) :: spinMeasured
      
      select type (haloSpinDistribution_)
      class is (haloSpinDistributionNbodyErrors)
         spinDistributionIntegrate=+                                                  spinMeasured  &
              &                    *haloSpinDistribution_%distributionFixedPoint(node,spinMeasured)
      class default
         spinDistributionIntegrate=0.0d0
      end select
      return
    end function spinDistributionIntegrate

  end function spinNBodyErrorsOperateScalar

  function spinNBodyErrorsOperateDistribution(self,distribution,propertyType,propertyValueMinimum,propertyValueMaximum,outputIndex,node)
    !% Implement an output analysis distribution operator which accounts for errors in N-body measurements of halo spin.
    use Galacticus_Error
    implicit none
    class           (outputAnalysisDistributionOperatorSpinNBodyErrors), intent(inout)                                        :: self
    double precision                                                   , intent(in   ), dimension(:)                          :: distribution
    integer                                                            , intent(in   )                                        :: propertyType
    double precision                                                   , intent(in   ), dimension(:)                          :: propertyValueMinimum              , propertyValueMaximum
    integer         (c_size_t                                         ), intent(in   )                                        :: outputIndex
    type            (treeNode                                         ), intent(inout)                                        :: node
    double precision                                                                  , dimension(size(propertyValueMinimum)) :: spinNBodyErrorsOperateDistribution
    !GCC$ attributes unused :: self, distribution, propertyValueMinimum, propertyValueMaximum, outputIndex, propertyType, node

    spinNBodyErrorsOperateDistribution=0.0d0
    call Galacticus_Error_Report('not implemented'//{introspection:location})
    return
  end function spinNBodyErrorsOperateDistribution