!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017
!!    Andrew Benson <abenson@carnegiescience.edu>
!!
!! This file is part of Galacticus.
!!
!!    Galacticus is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either versteeion 3 of the License, or
!!    (at your option) any later version.
!!
!!    Galacticus is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with Galacticus.  If not, see <http://www.gnu.org/licenses/>.

!% Contains a module which implements an N-body data operator which computes the velocity dispersion in a set of given spherical shells.
  
  use, intrinsic :: ISO_C_Binding

  !# <nbodyOperator name="nbodyOperatorVelocityDispersion">
  !#  <description>An N-body data operator which computes the rotation curve at a set of given radii.</description>
  !# </nbodyOperator>
  type, extends(nbodyOperatorClass) :: nbodyOperatorVelocityDispersion
     !% An N-body data operator which computes the rotation curve at a set of given radii.
     private
     logical                                               :: selfBoundParticlesOnly
     integer         (c_size_t)                            :: bootstrapSampleCount 
     double precision          , allocatable, dimension(:) :: radiusInner            , radiusOuter
   contains
     procedure :: operate => velocityDispersionOperate
  end type nbodyOperatorVelocityDispersion

  interface nbodyOperatorVelocityDispersion
     !% Constructors for the ``velocityDispersion'' N-body operator class.
     module procedure velocityDispersionConstructorParameters
     module procedure velocityDispersionConstructorInternal
  end interface nbodyOperatorVelocityDispersion

contains

  function velocityDispersionConstructorParameters(parameters) result (self)
    !% Constructor for the ``velocityDispersion'' N-body operator class which takes a parameter set as input.
    use Input_Parameters2
    implicit none
    type            (nbodyOperatorVelocityDispersion)                              :: self
    type            (inputParameters                ), intent(inout)               :: parameters
    double precision                                 , allocatable  , dimension(:) :: radiusInner           , radiusOuter 
    logical                                                                        :: selfBoundParticlesOnly
    integer         (c_size_t                       )                              :: bootstrapSampleCount

    allocate(radiusInner(parameters%count('radiusInner')))
    allocate(radiusOuter(parameters%count('radiusOuter')))
    !# <inputParameter>
    !#   <name>selfBoundParticlesOnly</name>
    !#   <source>parameters</source>
    !#   <description>If true, the velocity dispersion is computed only for self-bound particles.</description>
    !#   <type>logical</type>
    !#   <cardinality>0..1</cardinality>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>radiusInner</name>
    !#   <source>parameters</source>
    !#   <description>Inner radii of spherical shells within which the velocity dispersion should be computed.</description>
    !#   <type>float</type>
    !#   <cardinality>0..</cardinality>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>radiusOuter</name>
    !#   <source>parameters</source>
    !#   <description>Outer radii of spherical shells within which the velocity dispersion should be computed.</description>
    !#   <type>float</type>
    !#   <cardinality>0..</cardinality>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>bootstrapSampleCount</name>
    !#   <source>parameters</source>
    !#   <defaultValue>30_c_size_t</defaultValue>
    !#   <description>The number of bootstrap resamples of the particles that should be used.</description>
    !#   <type>integer</type>
    !#   <cardinality>0..1</cardinality>
    !# </inputParameter>
    self=nbodyOperatorVelocityDispersion(selfBoundParticlesOnly,bootstrapSampleCount,radiusInner,radiusOuter)
    !# <inputParametersValidate source="parameters"/>
    return
  end function velocityDispersionConstructorParameters

  function velocityDispersionConstructorInternal(selfBoundParticlesOnly,bootstrapSampleCount,radiusInner,radiusOuter) result (self)
    !% Internal constructor for the ``velocityDispersion'' N-body operator class.
    use Galacticus_Error
    implicit none
    type            (nbodyOperatorVelocityDispersion)                              :: self
    logical                                          , intent(in   )               :: selfBoundParticlesOnly
    integer         (c_size_t                       ), intent(in   )               :: bootstrapSampleCount
    double precision                                 , intent(in   ), dimension(:) :: radiusInner            , radiusOuter
    !# <constructorAssign variables="selfBoundParticlesOnly, bootstrapSampleCount, radiusInner, radiusOuter"/>

    if (size(self%radiusInner) /= size(self%radiusOuter)) call Galacticus_Error_Report('velocityDispersionConstructorInternal','number of inner and outer radii should be equal')
    return
  end function velocityDispersionConstructorInternal

  subroutine velocityDispersionOperate(self,simulation)
    !% Determine the mean position and velocity of N-body particles.
    use Memory_Management
    use Galacticus_Error
    use Numerical_Constants_Physical
    use IO_HDF5
    use FGSL
    use Poisson_Random
    implicit none
    class           (nbodyOperatorVelocityDispersion), intent(inout)                 :: self
    type            (nBodyData                      ), intent(inout)                 :: simulation
    integer                                          , allocatable  , dimension(:,:) :: selfBoundStatus
    double precision                                 , parameter                     :: sampleRate           =1.0d0
    double precision                                 , allocatable  , dimension(:,:) :: positionMean                , velocityDispersion
    double precision                                 , allocatable  , dimension(:  ) :: distanceRadialSquared
    double precision                                 , allocatable  , dimension(:,:) :: positionRelative
    logical                                          , allocatable  , dimension(:  ) :: mask
    double precision                                                , dimension(3  ) :: velocityMean                , velocityMeanSquared
    integer                                                                          :: k
    integer         (c_size_t                       )                                :: i                           , j
    type            (fgsl_rng                       )                                :: pseudoSequenceObject
    logical                                                                          :: pseudoSequenceReset  =.true.

    ! Allocate workspace.
    call allocateArray(distanceRadialSquared,[  size(simulation%position   ,dim =2       )                          ])
    call allocateArray(positionRelative     ,[3,size(simulation%position   ,dim =2       )                          ])
    call allocateArray(velocityDispersion   ,[  size(self      %radiusInner,kind=c_size_t),self%bootstrapSampleCount])
    call allocateArray(mask                 ,[  size(simulation%position   ,dim =2       )                          ])
    ! Determine the particle mask to use.
    if (self%selfBoundParticlesOnly) then
       if (simulation%analysis%hasDataset('selfBoundStatus')) then
          call simulation%analysis%readDataset('selfBoundStatus',selfBoundStatus)
          if (size(selfBoundStatus,dim=2) /= self%bootstrapSampleCount) call Galacticus_Error_Report('velocityDispersionOperate','number of selfBoundStatus samples must equal number of requested bootstrap samples')
       else
          call Galacticus_Error_Report('velocityDispersionOperate','self-bound status not available - apply a self-bound operator first')
       end if
    else
       call allocateArray(selfBoundStatus,[size(simulation%position,dim=2,kind=c_size_t),self%bootstrapSampleCount])
       do i=1,self%bootstrapSampleCount
          do j=1,size(simulation%position,dim=2)
             selfBoundStatus(j,i)=Poisson_Random_Get(pseudoSequenceObject,sampleRate,pseudoSequenceReset)
          end do
       end do
    end if
    ! Get mean position.
    if (.not.simulation%analysis%hasDataset('positionMean')) call Galacticus_Error_Report('velocityDispersionOperate','mean position not available - apply the mean position operator first')
    call simulation%analysis%readDataset('positionMean',positionMean)
    if (size(positionMean,dim=2) /= self%bootstrapSampleCount) call Galacticus_Error_Report('velocityDispersionOperate','number of positionMean samples must equal number of requested bootstrap samples')
    do i=1,self%bootstrapSampleCount
       !$omp parallel workshare
       ! Compute radial distance from the mean position.
       forall(k=1:3)
          positionRelative(k,:)=+simulation  %position(k,:) &
               &                -positionMean         (k,i)
       end forall
       distanceRadialSquared=sum(positionRelative**2,dim=1)
       !$omp end parallel workshare
       ! Compute velocity dispersion within each shell.
       do k=1,size(self%radiusInner)
          !$omp parallel workshare
          mask   = distanceRadialSquared >= self%radiusInner(k)**2 &
               &  .and.                                            &
               &   distanceRadialSquared <  self%radiusOuter(k)**2
          forall(j=1:3)
             velocityMean       (j)=sum(simulation%velocity(j,:)   *dble(selfBoundStatus(:,i)),mask=mask)/dble(sum(selfBoundStatus(:,i),mask))
             velocityMeanSquared(j)=sum(simulation%velocity(j,:)**2*dble(selfBoundStatus(:,i)),mask=mask)/dble(sum(selfBoundStatus(:,i),mask))
          end forall
          velocityDispersion(k,i)=sqrt(sum(velocityMeanSquared-velocityMean**2)/3.0d0)
          !$omp end parallel workshare
       end do
    end do
    ! Store results to file.
    call simulation%analysis%writeDataset(self%radiusInner  ,'velocityDispersionRadiusInner')
    call simulation%analysis%writeDataset(self%radiusInner  ,'velocityDispersionRadiusOuter')
    call simulation%analysis%writeDataset(velocityDispersion,'velocityDispersion'           )

do i=1,size(simulation%position   ,dim =2       )
   write (404,*)  simulation%velocity(:,i),simulation%position(:,i)
end do

    ! Deallocate workspace.
    call deallocateArray(selfBoundStatus      )
    call deallocateArray(distanceRadialSquared)
    call deallocateArray(positionRelative     )
    call deallocateArray(velocityDispersion   )
    call deallocateArray(mask                 )
     return
  end subroutine velocityDispersionOperate
